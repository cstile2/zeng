const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const utils = @import("utils.zig");

pub const ComponentTypes = [_]type{
    zeng.Mesh,
    zeng.Camera,
    zeng.SkinnedMesh,
    zeng.Transform,
    SineMover,
    CircleCollider,
    Flyer,
    Follower,
};
pub const ECS = ecs.CompileECS(ComponentTypes);
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
pub const Flyer = struct {};
pub const Follower = struct {
    anchor_point: zeng.Vec3,
    target: ECS.entity_id,
};

pub const ResourceTypes = [_]type{
    ECS.World,
    zeng.EngineState,
    zeng.Commands,

    Time,
    Input,
    MainCamera,
    world_entity_locations,
    SharedRenderStuff,
    UIRenderData,

    std.Random.Xoshiro256,
};
pub const Time = struct {
    delta_time: f64,
    elapsed_time: f64,
};
pub const Input = struct {
    t_down_last_frame: bool,
};
pub const MainCamera = struct {
    matrix: *[16]f32,
    camera: *zeng.Camera,
};
pub const world_entity_locations = struct {
    entities: *const ECS.entity_locations,
};
pub const SharedRenderStuff = struct {
    skinned_mesh: zeng.SkinnedMesh,
};
pub const UIRenderData = struct {
    shader_program: u32,
    texture: u32,
    vao: u32,
    indices_len: c_int,
};

pub fn main() !void {
    var is_server: bool = true;
    {
        std.debug.print("\nselect network mode:\n", .{});
        const stdin = std.io.getStdIn().reader();
        const thing = stdin.readBoundedBytes(1) catch unreachable;
        if (std.mem.eql(u8, thing.buffer[0..1], "s")) {
            std.debug.print("using server mode...\n", .{});
        } else if (std.mem.eql(u8, thing.buffer[0..1], "c")) {
            std.debug.print("using client mode...\n", .{});
            is_server = false;
        }
    }
    const server_socket_address = try zeng.networking.do_setup(is_server);
    defer zeng.networking.undo_setup(server_socket_address);

    var gd: zeng.EngineState = undefined;
    var res: zeng.Resources = undefined;
    var world: ECS.World = undefined;
    try zeng.engine_start(&gd, &res, &world);
    defer zeng.engine_end(&gd, &res, &world);
    var helper: zeng.EngineAPIHelper = .{ .world = &world, .gd = &gd, .res = &res, .allocator = gd.arena_allocator };

    const default_shader = zeng.load_shader(gd.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const uv_checker_tex = zeng.load_texture("assets/images/uv_checker.png");
    const skin_shader = zeng.load_shader(gd.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    var skinned_mesh: zeng.SkinnedMesh = undefined;
    {
        const bytes = zeng.get_file_bytes("assets/gltf/yobot_anim.gltf", gd.arena_allocator);
        try zeng.parse_gltf(bytes, gd.arena_allocator);
        skinned_mesh = try zeng.gltf_extract_skinned_mesh("assets/gltf/yobot_anim.bin", 1, skin_shader, uv_checker_tex, gd.arena_allocator);
    }
    const screen_shader = zeng.load_shader(gd.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader");
    const sdf = zeng.load_texture("assets/images/sdf_font.png");
    const vao, const length = zeng.thing();

    res.insert(gd.arena_allocator, UIRenderData{ .shader_program = screen_shader, .texture = sdf, .vao = vao, .indices_len = length });
    res.insert(gd.arena_allocator, std.Random.DefaultPrng.init(123));
    res.insert(gd.arena_allocator, SharedRenderStuff{ .skinned_mesh = skinned_mesh });
    res.insert(gd.arena_allocator, zeng.Commands{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = gd.allocator });
    res.insert(gd.arena_allocator, world_entity_locations{ .entities = &world._public_ids });
    res.insert(gd.arena_allocator, Time{ .delta_time = 0.16, .elapsed_time = 0.0 });
    res.insert(gd.arena_allocator, Input{ .t_down_last_frame = false });
    res.insert(gd.arena_allocator, @as(MainCamera, undefined));

    const player = try world.spawn(.{
        zeng.identity_matrix(),
        skinned_mesh,
        CircleCollider{ .radius = 1.0 },
        Flyer{},
    });
    const new_camera_entity = try world.spawn(.{
        zeng.Camera{ .projection_matrix = undefined },
        zeng.identity_matrix(),
        Follower{ .anchor_point = zeng.Vec3{}, .target = player },
    });
    res.get(MainCamera).matrix, res.get(MainCamera).camera = try world.get(world._public_ids[new_camera_entity], .{ zeng.Transform, zeng.Camera });
    zeng.window_resize(gd.active_window, gd.window_width, gd.window_height);

    gd.active_window.setInputModeCursor(zeng.glfw.Window.InputModeCursor.disabled);

    _ = zeng.instantiate_scene(&world, "assets/blender_files/main_scene.bin", gd.allocator, default_shader, uv_checker_tex);

    while (!gd.active_window.shouldClose()) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);

        res.get(Time).elapsed_time += res.get(Time).delta_time;
        helper.run_system(spawn_system);
        helper.run_system(sine_mover_system);
        helper.run_system(fly_system);
        helper.run_system(mouse_look_system);
        helper.run_system(follower_system);
        helper.run_system(circle_collision_system);
        helper.run_system(render_system);

        res.get(zeng.Commands).process_commands(&world);
    }
}

/// Test system
pub fn spawn_system(gd: *zeng.EngineState, commands: *zeng.Commands, sh: *SharedRenderStuff, input: *Input, rand: *std.rand.Xoshiro256) !void {
    if ((gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) and !input.t_down_last_frame) {
        var curr: u32 = 0;
        while (curr < 100) {
            defer curr += 1;
            commands.spawn(.{
                zeng.translated(zeng.identity_matrix(), zeng.Vec3{ .x = rand.random().float(f32), .y = rand.random().float(f32), .z = rand.random().float(f32) }),
                SineMover{ .offset = rand.random().float(f32) * (2.0 * 3.14159) },
                sh.skinned_mesh,
                CircleCollider{ .radius = 1.0 },
            });
        }
    }
    input.t_down_last_frame = gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;
    if (gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) {
        gd.active_window.setInputModeCursor(zeng.glfw.Window.InputModeCursor.normal);
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn circle_collision_system(q_4: *ECS.Query(.{ zeng.Transform, CircleCollider })) !void {
    var A = try zeng.Iterator(.{ zeng.Transform, CircleCollider }).create(q_4);

    while (A.next()) |_A| {
        const transformA, _ = _A;

        var B = A;
        _ = B.next();
        while (B.next()) |_B| {
            const transformB, _ = _B;

            const p_a = zeng.get_column(transformA.*, 3);
            const p_b = zeng.get_column(transformB.*, 3);

            const radius = 0.6;
            const delta = p_a.sub(p_b);
            if (delta.length_sq() > 0.0 and delta.length() < 2.0 * radius) {
                const push = (2.0 * radius - delta.length()) * 0.5;
                zeng.set_matrix_position(transformB, p_b.add(delta.normalized().mult(-push)));
                zeng.set_matrix_position(transformA, p_a.add(delta.normalized().mult(push)));
            }
        }
    }
}

/// Make all entities with a SineMover component and a transform float around randomly
pub fn sine_mover_system(time: *Time, q: *ECS.Query(.{ zeng.Transform, SineMover })) !void {
    var sine_iterator = try zeng.Iterator(.{ zeng.Transform, SineMover }).create(q);
    while (sine_iterator.next()) |transform_sine| {
        const transform, const sine_mover = transform_sine;

        const localtime = @as(f32, @floatCast(time.elapsed_time)) - sine_mover.offset;
        const dt = @as(f32, @floatCast(time.delta_time));
        transform.* = zeng.translated(transform.*, zeng.Vec3{ .x = @cos(localtime * 5.0) * dt, .y = @sin(localtime * 4.0) * dt, .z = @sin(localtime * 3.0) * dt });
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn fly_system(gd: *zeng.EngineState, cam: *MainCamera, time: *Time, q: *ECS.Query(.{ zeng.Transform, Flyer })) !void {
    var it = try zeng.Iterator(.{ zeng.Transform, Flyer }).create(q);
    while (it.next()) |transform_flyer| {
        const transform, _ = transform_flyer;

        var speed: f32 = @floatCast(time.delta_time * 100.0);
        if (gd.active_window.getKey(zeng.glfw.Key.left_shift) == zeng.glfw.Action.press) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            transform[12] -= cam.matrix[0] * speed;
            transform[13] -= cam.matrix[1] * speed;
            transform[14] -= cam.matrix[2] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            transform[12] += cam.matrix[0] * speed;
            transform[13] += cam.matrix[1] * speed;
            transform[14] += cam.matrix[2] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.q) == zeng.glfw.Action.press) {
            transform[12] -= cam.matrix[4] * speed;
            transform[13] -= cam.matrix[5] * speed;
            transform[14] -= cam.matrix[6] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.e) == zeng.glfw.Action.press) {
            transform[12] += cam.matrix[4] * speed;
            transform[13] += cam.matrix[5] * speed;
            transform[14] += cam.matrix[6] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            transform[12] -= cam.matrix[8] * speed;
            transform[13] -= cam.matrix[9] * speed;
            transform[14] -= cam.matrix[10] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            transform[12] += cam.matrix[8] * speed;
            transform[13] += cam.matrix[9] * speed;
            transform[14] += cam.matrix[10] * speed;
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn mouse_look_system(gd: *zeng.EngineState, q: *ECS.Query(.{ zeng.Transform, zeng.Camera })) !void {
    var look_iterator = try zeng.Iterator(.{ zeng.Transform, zeng.Camera }).create(q);
    while (look_iterator.next()) |transform_camera| {
        const transform, _ = transform_camera;

        var pos: [3]f32 = undefined;
        pos[0] = transform[12];
        pos[1] = transform[13];
        pos[2] = transform[14];
        const rot_mat_hor = zeng.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.active_window.getCursorPos().xpos * -0.0015));
        const rot_mat_vert = zeng.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.active_window.getCursorPos().ypos * -0.0015));
        transform.* = zeng.multiply_matrices(rot_mat_hor, rot_mat_vert);
        transform[12] = pos[0];
        transform[13] = pos[1];
        transform[14] = pos[2];
    }
}

/// Render all entities with a transform and a mesh
pub fn render_system(gd: *zeng.EngineState, ui_ren: *UIRenderData, cam: *MainCamera, render_q: *ECS.Query(.{ zeng.Transform, zeng.Mesh }), skinned_q: *ECS.Query(.{ zeng.Transform, zeng.SkinnedMesh })) !void {
    zeng.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    zeng.gl.clear(zeng.gl.COLOR_BUFFER_BIT | zeng.gl.DEPTH_BUFFER_BIT);

    const inv_camera_matrix: [16]f32 = zeng.invert_matrix(cam.matrix.*);

    var render_iterator = try zeng.Iterator(.{ zeng.Transform, zeng.Mesh }).create(render_q);
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = try zeng.Iterator(.{ zeng.Transform, zeng.SkinnedMesh }).create(skinned_q);
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_skinned_mesh(skin.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    draw_text("the quick brown fox JUMPS OVER THE FENCE!!!", ui_ren);

    gd.active_window.swapBuffers();
}

/// Draws the text
pub fn draw_text(string: []const u8, ui_ren: *UIRenderData) void {
    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ui_ren.texture);

    zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "dims"), 0.02, 0.05);

    var horizontal: usize = 0;
    for (string) |char| {
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos"), @as(f32, @floatFromInt(horizontal)) * 0.038, 0.0);
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "image_point"), @as(f32, @floatFromInt((char - 32) % 16)), @as(f32, @floatFromInt((char - 32) / 16)));
        zeng.gl.drawElements(zeng.gl.TRIANGLES, ui_ren.indices_len, zeng.gl.UNSIGNED_INT, null);
        horizontal += 1;
    }

    zeng.gl.bindVertexArray(0);
}

/// Makes all follower entities follow their target
pub fn follower_system(cam_q: *ECS.Query(.{ Follower, zeng.Transform }), q: *ECS.Query(.{ Flyer, zeng.Transform }), entities: *world_entity_locations) !void {
    var cam_it = try zeng.Iterator(.{ Follower, zeng.Transform }).create(cam_q);
    while (cam_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.get_matrix_position(q.get(entities.entities.*, cam_follower.target, zeng.Transform).*);

        cam_follower.anchor_point = zeng.Vec3.lerp(cam_follower.anchor_point, target_position, 0.1);

        zeng.set_matrix_position(cam_transform, cam_follower.anchor_point.add(zeng.get_matrix_z_axis(cam_transform.*).mult(2.0).add(zeng.Vec3{ .y = 1.0 })));
    }
}

// need quality collision system - plane method + shapecast
// need real animations - fuller gltf support
// need sounds
// better rendering - lights
