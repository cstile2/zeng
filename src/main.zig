// this is zig's standard library
const std = @import("std");
// this is the game engine code - contains usefule helper functions for rendering + loading + more
const Engine = @import("engine.zig");
// this is the entity component system (ECS)
const ecs = @import("ecs.zig");

// components - create structs whenever you want to add stuff to your game
pub const SineMover = struct { // this component makes an entity float around randomly
    offset: f32 = 0.0,
};
pub const CircleCollider = struct { // this component enables sphere-based collisions
    radius: f32 = 1.0,
};

// this is a type registry - when you create a new "component" put the name of it in this list for the engine to use it
pub const TypeRegistry = [_]type{
    Engine.Camera,
    Engine.Mesh,
    Engine.Name,
    Engine.Transform,
    SineMover,
    CircleCollider,
};

// this is necessary for the engine to use the components that you made and put them into the ECS
pub const ECS = ecs.CompileECS(TypeRegistry);

// entry point
pub fn main() !void {
    // initialize global data and engine application and window stuff
    var gd: Engine.GlobalData = undefined;
    try Engine.InitializeStuff(&gd);
    defer Engine.DeinitializeStuff(&gd);

    // create ECS world - this contains all entities in the game world - it's the main part of the game engine
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(gd.allocator);
    defer world.Destroy() catch unreachable;

    // load shader from file and load texture from file
    const shader_program_GPU = Engine.LoadShader(gd.allocator, "assets/shaders/basic.shader", "assets/shaders/fragment.shader");
    const texture_GPU = Engine.LoadTexture("assets/images/uv_checker.png");

    // spawn an entity with these components: Camera, Transform, CircleCollider
    const cam = try world.SpawnEntity(.{
        Engine.Camera{ .projection_matrix = undefined },
        Engine.identity(),
        CircleCollider{},
    });
    // get pointers to the camera's data to use as the main rendering camera
    try world.Get(cam, .{ &gd.active_camera_matrix, &gd.active_camera });
    // call this to tell the game the correct window dimensions - used for camera + rendering
    Engine.OnWindowResize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    // import a file containing 3d models (created in Blender)
    _ = Engine.SpawnModels(&world, "assets/blender_files/main_scene.bin", gd.allocator, shader_program_GPU, texture_GPU);

    // MAIN GAME LOOP - runs until user closes the window
    while (!gd.active_window.shouldClose()) {
        // this is needed to calculate delta_time and other stuff
        defer Engine.EndOfFrameStuff(&gd);

        // update input + time
        gd.cur_pos = gd.active_window.getCursorPos();
        gd.elapsed_time += @floatCast(gd.frame_delta);

        // spawn floating guy when key "t" is pressed (not held)
        if ((gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press) and !gd.t_down_last_frame) {
            var imported = Engine.SpawnModels(&world, "assets/blender_files/simple.bin", gd.allocator, shader_program_GPU, texture_GPU);
            try world.SetComponent(SineMover{ .offset = 0.0 }, &imported);
            try world.SetComponent(CircleCollider{}, &imported);
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
    }
}

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
