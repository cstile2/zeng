const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");

// interop between comptime types and runtime component information
pub const ComponentTypes = [_]type{
    zeng.Camera,
    zeng.Mesh,
    zeng.Transform,
    SineMover,
    CircleCollider,
};
pub const ECS = ecs.CompileECS(ComponentTypes);

// networking meta stuff - need to be able to request resources from the network recieving procedure at runtime
pub const ResourceTypes = [_]type{};
pub var EngineResources = std.AutoHashMap(u32, *anyopaque);

pub const SineMover = struct {
    offset: f32 = 0.0,
};
pub const CircleCollider = struct {
    radius: f32 = 1.0,
};

pub const PlayerEvent = struct {
    amt: f32,
    num: u16,
};

fn SubTuple(comptime T: type, comptime low: usize, comptime high: usize) type {
    const info = @typeInfo(T);
    const old_fields = std.meta.fields(T)[low..high];
    var new_fields: [old_fields.len]std.builtin.Type.StructField = undefined;
    for (old_fields, 0..) |old, i| {
        new_fields[i] = .{
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .type = old.type,
            .default_value = old.default_value,
            .alignment = old.alignment,
            .is_comptime = old.is_comptime,
        };
    }
    return @Type(.{
        .Struct = .{
            .layout = info.Struct.layout,
            .fields = &new_fields,
            .decls = &.{},
            .is_tuple = true,
        },
    });
}

fn Split(comptime T: type, comptime pivot: usize) type {
    const fields = std.meta.fields(T);
    return std.meta.Tuple(&[_]type{
        SubTuple(T, 0, pivot),
        SubTuple(T, pivot, fields.len),
    });
}

fn split(tuple: anytype, comptime pivot: usize) Split(@TypeOf(tuple), pivot) {
    const fields = std.meta.fields(@TypeOf(tuple));
    return .{
        extract(tuple, 0, pivot),
        extract(tuple, pivot, fields.len),
    };
}

pub fn extract(tuple: anytype, comptime low: usize, comptime high: usize) SubTuple(@TypeOf(tuple), low, high) {
    var out: SubTuple(@TypeOf(tuple), low, high) = undefined;
    inline for (low..high, 0..) |i, o| {
        out[o] = tuple[i];
    }
    return out;
}

pub fn main() !void {
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

    var gd: zeng.GlobalData = undefined;
    try zeng.engine_start(&gd);
    defer zeng.engine_end(&gd);

    var world: ECS.World = undefined;
    world.init(gd.allocator);
    defer world.deinit() catch unreachable;

    var commands: zeng.Commands = .{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = gd.allocator };

    const shader_program_GPU = zeng.load_shader(gd.allocator, "assets/shaders/basic.shader", "assets/shaders/fragment.shader");
    const texture_GPU = zeng.load_texture("assets/images/uv_checker.png");

    const new_camera_entity = try world.spawn(.{
        zeng.Camera{ .projection_matrix = undefined },
        zeng.identity_matrix(),
        CircleCollider{},
    });
    try world.get_component(new_camera_entity, .{ &gd.active_camera_matrix, &gd.active_camera });
    zeng.window_resize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    _ = zeng.spawn_models(&world, "assets/blender_files/main_scene.bin", gd.allocator, shader_program_GPU, texture_GPU);

    var socket_and_address: zeng.networking.SocketAndAddress = undefined;
    if (is_server) {
        socket_and_address = try zeng.networking.make_udp_sock_and_address("192.168.0.90", 55555, true);
        try zeng.networking.bind_socket_and_address(socket_and_address);

        MyFunc(14.30, &world);
    } else {
        socket_and_address = try zeng.networking.make_udp_sock_and_address("192.168.0.90", 55555, true);
        _ = try std.os.sendto(socket_and_address.socket, "I want to connect!", 0, &socket_and_address.address.any, socket_and_address.address.getOsSockLen());
    }
    defer std.os.close(socket_and_address.socket);

    // MAIN GAME LOOP - runs until user closes the window
    while (!gd.active_window.shouldClose()) {
        defer zeng.late_frame_calculations(&gd);

        // server networking
        if (is_server) {
            try zeng.networking.network_recieve_all(socket_and_address.socket, &world);
        }

        // update input + time
        gd.cur_pos = gd.active_window.getCursorPos();
        gd.elapsed_time += @floatCast(gd.frame_delta);

        // spawn floating guy when key "t" is pressed (not held)
        if ((gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) and !gd.t_down_last_frame) {
            var imported = zeng.spawn_models(&world, "assets/blender_files/simple.bin", gd.allocator, shader_program_GPU, texture_GPU);
            try world.insert_component(SineMover{ .offset = 0.0 }, &imported);
            try world.insert_component(CircleCollider{}, &imported);

            if (!is_server) {
                commands.remote_call(socket_and_address, MyFunc, .{14.30});
                commands.remote_event(socket_and_address, PlayerEvent{ .amt = 101.0101, .num = 2323 });
            }
        }
        gd.t_down_last_frame = gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;

        // systems - these are what updates all of the data and logic in the game
        var query_t_s = try ECS.QueryIterator.create(&world, .{ zeng.Transform, SineMover });
        try SineMoverSystem(&gd, &query_t_s);
        try query_t_s.destroy();

        var query_t_c = try ECS.QueryIterator.create(&world, .{ zeng.Transform, zeng.Camera });
        try FlySystem(&gd, &query_t_c);
        try query_t_c.destroy();

        var query_t_cc_A = try ECS.QueryIterator.create(&world, .{ zeng.Transform, CircleCollider });
        var query_t_cc_B = try ECS.QueryIterator.create(&world, .{ zeng.Transform, CircleCollider });
        try CircleCollisionSystem(&query_t_cc_A, &query_t_cc_B);
        try query_t_cc_A.destroy();
        try query_t_cc_B.destroy();

        var query_t_c_2 = try ECS.QueryIterator.create(&world, .{ zeng.Transform, zeng.Camera });
        try MouseLookSystem(&gd, &query_t_c_2);
        try query_t_c_2.destroy();

        var query_t_m = try ECS.QueryIterator.create(&world, .{ zeng.Transform, zeng.Mesh });
        try RenderSystem(&gd, &query_t_m);
        try query_t_m.destroy();

        // client networking
        if (!is_server) {
            try zeng.networking.network_send_all(&commands);
        }
    }
}

pub fn MyFunc(f: f32, world: *ECS.World) void {
    var t = zeng.identity_matrix();
    t[13] = f;
    _ = world.spawn(.{
        t,
    }) catch unreachable;
    std.debug.print("i spawned a guy\n", .{});
}

pub const Resources = struct {
    resources: [128]*anyopaque,
    resources_len: u8,
    allocator: std.mem.Allocator,

    pub fn create_unique_id() u32 {
        return 0;
    }
    pub fn store_resource(self: *Resources, resource: anytype, id: u32) !void {
        _ = id; // autofix
        self.resources[self.resources_len] = @ptrCast(try self.allocator.create(@TypeOf(resource)));
        self.resources[self.resources_len];
        self.resources_len += 1;
    }
    pub fn get(a: anytype, b: anytype) a {
        _ = b; // autofix
    }
};

pub fn SpawnQuadTexture(synced_tex_num: u32, world: *ECS.World, resources: *Resources) void {
    const retrieved_tex = resources.get(u32, synced_tex_num);
    _ = try world.spawn{
        zeng.Mesh{ .material = zeng.Material{ .texture_GPU = retrieved_tex } },
    };
}

/// Make all entities with a CircleCollider collide with each other
pub fn CircleCollisionSystem(
    queryA: *ECS.QueryIterator,
    queryB: *ECS.QueryIterator,
) !void {
    while (queryA.next()) {
        const transformsA = try queryA.field(zeng.Transform);
        for (transformsA) |*transformA| {
            queryB.reset();
            while (queryB.next()) {
                const transformsB = try queryB.field(zeng.Transform);
                for (transformsB) |*transformB| {
                    if (transformA == transformB) {
                        continue;
                    }
                    var delta = zeng.Vec3{ .x = transformA[12] - transformB[12], .y = transformA[13] - transformB[13], .z = transformA[14] - transformB[14] };
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
    gd: *zeng.GlobalData,
    query_sine_mover: *ECS.QueryIterator,
) !void {
    while (query_sine_mover.next()) {
        const Ts = try query_sine_mover.field(zeng.Transform);
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
    gd: *zeng.GlobalData,
    query_fly: *ECS.QueryIterator,
) !void {
    while (query_fly.next()) {
        const Ts = try query_fly.field(zeng.Transform);
        for (Ts) |*transform| {
            var speed: f32 = @floatCast(gd.frame_delta * 100.0);
            if (gd.active_window.getKey(zeng.glfw.Key.left_shift) == zeng.glfw.Action.press) {
                speed *= 0.2;
            } else {
                speed *= 0.05;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
                transform[12] -= transform[0] * speed;
                transform[13] -= transform[1] * speed;
                transform[14] -= transform[2] * speed;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
                transform[12] += transform[0] * speed;
                transform[13] += transform[1] * speed;
                transform[14] += transform[2] * speed;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.q) == zeng.glfw.Action.press) {
                transform[12] -= transform[4] * speed;
                transform[13] -= transform[5] * speed;
                transform[14] -= transform[6] * speed;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.e) == zeng.glfw.Action.press) {
                transform[12] += transform[4] * speed;
                transform[13] += transform[5] * speed;
                transform[14] += transform[6] * speed;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
                transform[12] -= transform[8] * speed;
                transform[13] -= transform[9] * speed;
                transform[14] -= transform[10] * speed;
            }
            if (gd.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
                transform[12] += transform[8] * speed;
                transform[13] += transform[9] * speed;
                transform[14] += transform[10] * speed;
            }
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn MouseLookSystem(
    gd: *zeng.GlobalData,
    query_mouse_look: *ECS.QueryIterator,
) !void {
    while (query_mouse_look.next()) {
        const Ts = try query_mouse_look.field(zeng.Transform);
        for (Ts) |*transform| {
            var pos: [3]f32 = undefined;
            pos[0] = transform[12];
            pos[1] = transform[13];
            pos[2] = transform[14];
            const rot_mat_hor = zeng.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.cur_pos.xpos * -0.0015));
            const rot_mat_vert = zeng.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.cur_pos.ypos * -0.0015));
            transform.* = zeng.multiply_matrices(rot_mat_hor, rot_mat_vert);
            transform[12] = pos[0];
            transform[13] = pos[1];
            transform[14] = pos[2];
        }
    }
}

/// Render all entities with a transform and a mesh
pub fn RenderSystem(
    gd: *zeng.GlobalData,
    query: *ECS.QueryIterator,
) !void {
    zeng.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    zeng.gl.clear(zeng.gl.COLOR_BUFFER_BIT | zeng.gl.DEPTH_BUFFER_BIT);

    const inv_camera_matrix: [16]f32 = zeng.invert_matrix(gd.active_camera_matrix.*);

    while (query.next()) {
        const Ms = try query.field(zeng.Mesh);
        const Ts = try query.field(zeng.Transform);
        for (Ms, Ts) |mesh, transform| {
            zeng.draw_mesh(mesh, transform, gd.active_camera.projection_matrix, inv_camera_matrix);
        }
    }

    gd.active_window.swapBuffers();
}
