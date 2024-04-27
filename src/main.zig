const std = @import("std");
const Engine = @import("engine.zig");
const ecs = @import("ecs.zig");

pub const SineMover = struct { // this component makes an entity float around randomly
    offset: f32 = 0.0,
};
pub const CircleCollider = struct { // this component enables sphere-based collisions
    radius: f32 = 1.0,
};

pub const TypeRegistry = [_]type{
    Engine.Camera,
    Engine.Mesh,
    Engine.Transform,
    SineMover,
    CircleCollider,
};
pub const ECS = ecs.CompileECS(TypeRegistry);

pub fn main() !void {
    // select network mode - client or server
    var is_server: bool = true;
    {
        std.debug.print("\nstarted!\n", .{});
        const stdin = std.io.getStdIn().reader();
        const thing = stdin.readBoundedBytes(1) catch unreachable;
        if (std.mem.eql(u8, thing.buffer[0..1], "s")) {
            std.debug.print("using server mode...\n", .{});
        } else if (std.mem.eql(u8, thing.buffer[0..1], "c")) {
            std.debug.print("using client mode...\n", .{});
            is_server = false;
        }
    }

    // initialize global data and engine application and window stuff
    var gd: Engine.GlobalData = undefined;
    try Engine.InitializeStuff(&gd);
    defer Engine.DeinitializeStuff(&gd);

    // create ECS world - this contains all entities in the game world - it's the main part of the game engine
    var world: ECS.ECSWorld = undefined;
    world.init(gd.allocator);
    defer world.deinit() catch unreachable;

    // load shader from file and load texture from file
    const shader_program_GPU = Engine.LoadShader(gd.allocator, "assets/shaders/basic.shader", "assets/shaders/fragment.shader");
    const texture_GPU = Engine.LoadTexture("assets/images/uv_checker.png");

    // spawn an entity with these components: Camera, Transform, CircleCollider
    const new_camera_entity = try world.spawn(.{
        Engine.Camera{ .projection_matrix = undefined },
        Engine.identity(),
        CircleCollider{},
    });
    // get pointers to the camera's data to use as the main rendering camera
    try world.get_component(new_camera_entity, .{ &gd.active_camera_matrix, &gd.active_camera });
    // call this to tell the game the correct window dimensions - used for camera + rendering
    Engine.OnWindowResize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    // import a file containing 3d models (created in Blender) and spawn them in the world
    _ = Engine.SpawnModels(&world, "assets/blender_files/main_scene.bin", gd.allocator, shader_program_GPU, texture_GPU);

    // networking setup
    Engine.networking.ClientMap = @TypeOf(Engine.networking.ClientMap).init(gd.allocator);
    defer Engine.networking.ClientMap.deinit();
    var net_tup: struct { std.os.socket_t, std.net.Address } = undefined;
    if (is_server) {
        net_tup = try Engine.networking.CreateSocketAddress("0.0.0.0", 55555, true);
        try Engine.networking.BindSocketAddress(net_tup);
    } else {
        net_tup = try Engine.networking.CreateSocketAddress("192.168.0.90", 55555, true);
        _ = try std.os.sendto(net_tup[0], "I want to connect!", 0, &net_tup[1].any, net_tup[1].getOsSockLen());
    }

    // MAIN GAME LOOP - runs until user closes the window
    while (!gd.active_window.shouldClose()) {
        // this is needed to calculate frame time and other stuff - runs at the very end of each frame
        defer Engine.EndOfFrameStuff(&gd);

        // server networking
        if (is_server) {
            var packet_data_buffer: [4096]u8 = undefined;
            var player_update_buffer: [32]struct { Engine.networking.ClientInfo, []u8 } = undefined;
            var player_update_num: u32 = 0;
            try Engine.networking.ServerSiftPackets(player_update_buffer[0..], &player_update_num, net_tup[0], &packet_data_buffer);

            // handle any brand new clients
            for (Engine.networking.ClientMap.values()) |*maybe_edl| {
                if (maybe_edl.* == null) {
                    const imported = Engine.SpawnModels(&world, "assets/blender_files/simple.bin", gd.allocator, shader_program_GPU, texture_GPU);
                    maybe_edl.* = imported;
                }
            }

            // for each existing client, update local representation
            for (player_update_buffer[0..player_update_num]) |clientinfo_and_data| {
                const edl = Engine.networking.ClientMap.getEntry(clientinfo_and_data[0]).?.value_ptr.*.?;
                var transform: *Engine.Transform = undefined;
                try world.get_component(edl, &transform);
                @memcpy(@as([*]u8, @ptrCast(&(transform.*[12]))), clientinfo_and_data[1][0..4]);
                @memcpy(@as([*]u8, @ptrCast(&(transform.*[13]))), clientinfo_and_data[1][4..8]);
                @memcpy(@as([*]u8, @ptrCast(&(transform.*[14]))), clientinfo_and_data[1][8..12]);
            }
        }

        // update input + time
        gd.cur_pos = gd.active_window.getCursorPos();
        gd.elapsed_time += @floatCast(gd.frame_delta);

        // spawn floating guy when key "t" is pressed (not held)
        if ((gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press) and !gd.t_down_last_frame) {
            var imported = Engine.SpawnModels(&world, "assets/blender_files/simple.bin", gd.allocator, shader_program_GPU, texture_GPU);
            try world.insert_component(SineMover{ .offset = 0.0 }, &imported);
            try world.insert_component(CircleCollider{}, &imported);
        }
        gd.t_down_last_frame = gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press;

        // systems - these are what updates all of the data and logic in the game
        var query_t_s = try ECS.QueryIterator.create(&world, .{ Engine.Transform, SineMover });
        try SineMoverSystem(&gd, &query_t_s);
        try query_t_s.destroy();

        var query_t_c = try ECS.QueryIterator.create(&world, .{ Engine.Transform, Engine.Camera });
        try FlySystem(&gd, &query_t_c);
        try query_t_c.destroy();

        var query_t_cc_A = try ECS.QueryIterator.create(&world, .{ Engine.Transform, CircleCollider });
        var query_t_cc_B = try ECS.QueryIterator.create(&world, .{ Engine.Transform, CircleCollider });
        try CircleCollisionSystem(&query_t_cc_A, &query_t_cc_B);
        try query_t_cc_A.destroy();
        try query_t_cc_B.destroy();

        var query_t_c_2 = try ECS.QueryIterator.create(&world, .{ Engine.Transform, Engine.Camera });
        try MouseLookSystem(&gd, &query_t_c_2);
        try query_t_c_2.destroy();

        var query_t_m = try ECS.QueryIterator.create(&world, .{ Engine.Transform, Engine.Mesh });
        try RenderSystem(&gd, &query_t_m);
        try query_t_m.destroy();

        // client networking
        if (!is_server) {
            const ptr: [*]u8 = std.mem.asBytes(gd.active_camera_matrix[12..15]);
            _ = try std.os.sendto(net_tup[0], ptr[0..12], 0, &net_tup[1].any, net_tup[1].getOsSockLen());
        }
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn CircleCollisionSystem(
    queryA: *ECS.QueryIterator,
    queryB: *ECS.QueryIterator,
) !void {
    while (queryA.next()) {
        const transformsA = try queryA.field(Engine.Transform);
        for (transformsA) |*transformA| {
            queryB.reset();
            while (queryB.next()) {
                const transformsB = try queryB.field(Engine.Transform);
                for (transformsB) |*transformB| {
                    if (transformA == transformB) {
                        continue;
                    }
                    var delta = Engine.Vec3{ .x = transformA[12] - transformB[12], .y = transformA[13] - transformB[13], .z = transformA[14] - transformB[14] };
                    if (delta.length() < 1.2) {
                        delta = delta.normalized().mult(1.2);

                        transformB[12] = transformA[12] - delta.x;
                        transformB[13] = transformA[13] - delta.y;
                        transformB[14] = transformA[14] - delta.z;
                    }
                }
            }
        }
    }
}

/// Make all entities with a SineMover component and a transform float around randomly
pub fn SineMoverSystem(
    gd: *Engine.GlobalData,
    query_sine_mover: *ECS.QueryIterator,
) !void {
    while (query_sine_mover.next()) {
        const Ts = try query_sine_mover.field(Engine.Transform);
        const Ss = try query_sine_mover.field(SineMover);
        for (Ts, Ss) |*transform, sine_mover| {
            const localtime = gd.elapsed_time - sine_mover.offset;
            transform[14] += @sin(localtime * 3.0) * @as(f32, @floatCast(gd.frame_delta)) * 5.0;
            transform[13] += @sin(localtime * 4.0) * @as(f32, @floatCast(gd.frame_delta)) * 5.0;
            transform[12] += @cos(localtime * 5.0) * @as(f32, @floatCast(gd.frame_delta)) * 5.0;
        }
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn FlySystem(
    gd: *Engine.GlobalData,
    query_fly: *ECS.QueryIterator,
) !void {
    while (query_fly.next()) {
        const Ts = try query_fly.field(Engine.Transform);
        for (Ts) |*transform| {
            var speed: f32 = @floatCast(gd.frame_delta * 100.0);
            if (gd.active_window.getKey(Engine.glfw.Key.left_shift) == Engine.glfw.Action.press) {
                speed *= 0.2;
            } else {
                speed *= 0.05;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.a) == Engine.glfw.Action.press) {
                transform[12] -= transform[0] * speed;
                transform[13] -= transform[1] * speed;
                transform[14] -= transform[2] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.d) == Engine.glfw.Action.press) {
                transform[12] += transform[0] * speed;
                transform[13] += transform[1] * speed;
                transform[14] += transform[2] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.q) == Engine.glfw.Action.press) {
                transform[12] -= transform[4] * speed;
                transform[13] -= transform[5] * speed;
                transform[14] -= transform[6] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.e) == Engine.glfw.Action.press) {
                transform[12] += transform[4] * speed;
                transform[13] += transform[5] * speed;
                transform[14] += transform[6] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.w) == Engine.glfw.Action.press) {
                transform[12] -= transform[8] * speed;
                transform[13] -= transform[9] * speed;
                transform[14] -= transform[10] * speed;
            }
            if (gd.active_window.getKey(Engine.glfw.Key.s) == Engine.glfw.Action.press) {
                transform[12] += transform[8] * speed;
                transform[13] += transform[9] * speed;
                transform[14] += transform[10] * speed;
            }
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn MouseLookSystem(
    gd: *Engine.GlobalData,
    query_mouse_look: *ECS.QueryIterator,
) !void {
    while (query_mouse_look.next()) {
        const Ts = try query_mouse_look.field(Engine.Transform);
        for (Ts) |*transform| {
            var pos: [3]f32 = undefined;
            pos[0] = transform[12];
            pos[1] = transform[13];
            pos[2] = transform[14];
            const rot_mat_hor = Engine.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.cur_pos.xpos * -0.0015));
            const rot_mat_vert = Engine.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.cur_pos.ypos * -0.0015));
            transform.* = Engine.multiply_matrices(rot_mat_hor, rot_mat_vert);
            transform[12] = pos[0];
            transform[13] = pos[1];
            transform[14] = pos[2];
        }
    }
}

/// Render all entities with a transform and a mesh
pub fn RenderSystem(
    gd: *Engine.GlobalData,
    query: *ECS.QueryIterator,
) !void {
    Engine.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    Engine.gl.clear(Engine.gl.COLOR_BUFFER_BIT | Engine.gl.DEPTH_BUFFER_BIT);

    const inv_camera_matrix: [16]f32 = Engine.InvertMatrix(gd.active_camera_matrix.*);

    while (query.next()) {
        const Ms = try query.field(Engine.Mesh);
        const Ts = try query.field(Engine.Transform);
        for (Ms, Ts) |mesh, transform| {
            Engine.DrawMesh2(mesh, transform, gd.active_camera.projection_matrix, inv_camera_matrix);
        }
    }

    gd.active_window.swapBuffers();
}
