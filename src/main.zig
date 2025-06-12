const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const phy = @import("physics.zig");
const aud = @import("audio.zig");
const util = @import("utils.zig");

pub const COMPONENT_TYPES = [_]type{
    zeng.mesh,
    zeng.camera,
    zeng.skinned_mesh,
    zeng.world_matrix,
    sphere_collider,
    fly_component,
    follow_component,
    children,
    local_matrix,
    player,
    animation_component,
    zeng.skeleton,
    input_implement,
};
pub const sphere_collider = struct {
    radius: f32 = 1.0,
};
pub const fly_component = struct {};
pub const follow_component = struct {
    anchor_point: zeng.vec3,
    target: ecs.entity_id,
};
pub const animation_player = struct {
    time: f32,
    current_animation: usize,
    animations: []zeng.Animation,
    skeleton_ptr: *zeng.skeleton,
};
pub const children = struct {
    items: []ecs.entity_id,
};
pub const local_matrix = struct {
    transform: zeng.world_matrix = zeng.mat_identity,
};
pub const player = struct {
    velocity: zeng.vec3,
    old_velocity: zeng.vec3 = zeng.vec3.ZERO,
    ground_normal: zeng.vec3,
    grounded: bool,
    animation_controller: ecs.entity_id,
    tilt: zeng.vec3 = zeng.vec3.ZERO,
};
pub const animation_component = struct {
    time: f32,
    current_animation: usize,
};
pub const input_implement = struct {
    move_fn: *const fn (*zeng.engine_context) zeng.vec2,
    jump_fn: *const fn (*zeng.engine_context) bool,
    pub fn default_move_fn(ctx: *zeng.engine_context) zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (ctx.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            input_vect.x += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            input_vect.x += 1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            input_vect.y += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_move_fn2(ctx: *zeng.engine_context) zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (ctx.active_window.getKey(zeng.glfw.Key.left) == zeng.glfw.Action.press) {
            input_vect.x += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.right) == zeng.glfw.Action.press) {
            input_vect.x += 1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.up) == zeng.glfw.Action.press) {
            input_vect.y += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.down) == zeng.glfw.Action.press) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_jump(ctx: *zeng.engine_context) bool {
        return (ctx.active_window.getKey(zeng.glfw.Key.space) == zeng.glfw.Action.press);
    }
    pub fn default_jump2(ctx: *zeng.engine_context) bool {
        return (ctx.active_window.getKey(zeng.glfw.Key.slash) == zeng.glfw.Action.press);
    }
};

pub const RESOURCE_TYPES = [_]type{
    ecs.world,
    zeng.engine_context,
    zeng.commands,
    time_res,
    input_res,
    main_camera_res,
    text_render_res,
    networking_res,
    sound_res,
    std.Random.Xoshiro256,
    debug_res,
    animation_res,
    std.ArrayList(phy.collider_info),
    cube_tracker_res,
    events([]const u8),
};
pub const time_res = struct {
    delta_time: f64,
    dt: f32,
    elapsed_time: f64,
};
pub const input_res = struct {
    t_down_last_frame: bool,
};
pub const main_camera_res = struct {
    matrix: *[16]f32,
    camera: *zeng.camera,
};
pub const text_render_res = struct {
    shader_program: u32,
    texture: u32,
    vao: u32,
    indices_len: c_int,
};
pub const networking_res = struct {
    main_socket: zeng.net.socket_t,
    server_address: zeng.net.address_t,
    is_server: bool = undefined,
};
pub const sound_res = struct {
    first: aud.audio_sample_info,
};
pub const entity_dependency_res = struct {
    map: std.AutoHashMap(struct { u32, type }, *anyopaque), // gives direct reference to a component

    pub fn get(this: *entity_dependency_res, id: u32, comp_type: type) ?*comp_type {
        return @ptrCast(this.map.get(.{ id, comp_type }) orelse return null);
    }
};
pub const debug_res = @import("render.zig").triangle_debug_info;
pub const animation_res = std.ArrayList(zeng.Animation);
pub const cube_tracker_res = struct {
    map: std.AutoHashMap(dumb, void),
    cube_mesh: zeng.mesh,
    world_mesh_data: *const anyopaque,
    cube_mesh_data: *const anyopaque,
};

pub const Pose = struct { []zeng.quat, []zeng.vec3, []zeng.vec3 };
pub fn events(T: type) type {
    return struct {
        array: std.ArrayList(T),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{ .array = std.ArrayList(T).init(allocator) };
        }
        pub fn deinit(self: *@This()) void {
            self.array.deinit();
        }
        pub fn send(this: *@This(), event: T) void {
            this.array.append(event) catch unreachable;
        }
        pub fn items(this: *@This()) []T {
            return this.array.items;
        }
        pub fn clear(this: *@This()) void {
            this.array.clearAndFree();
        }
    };
}
const dumb = struct { i32, i32, i32 };

pub fn main() !void {
    var ctx: zeng.engine_context = undefined;
    var res: zeng.resources_t = undefined;
    var world: ecs.world = undefined;
    var fet: zeng.resource_fetcher = undefined;
    try zeng.engine_start(&ctx, &res, &world, &fet);
    defer zeng.engine_end(&ctx, &res, &world);
    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});

    var is_server: bool = true;
    const cmd = zeng.get_file_bytes("assets/extras/command_input.txt", ctx.allocator);
    if (cmd[0] == 'c') is_server = false;
    ctx.allocator.free(cmd);
    const main_socket, const server_address = zeng.net.do_setup("192.168.1.104", 12345, is_server) catch unreachable;
    defer zeng.net.undo_setup(main_socket);

    const triangle_vao, const triangle_vbo = zeng.create_triangle_mesh();
    const cube_vao, const cube_len = zeng.create_cube_mesh_with_normals();
    const cube_collider_pos, const cube_collider_indices = zeng.create_cube_mesh_collider();
    const sky_shader = zeng.load_shader(ctx.allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const static_shader = zeng.load_shader(ctx.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.load_shader(ctx.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.load_shader(ctx.allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.load_texture("assets/images/uv_checker.png", true, false);
    const black_tex = zeng.load_texture("assets/images/black.png", true, false);
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .texture = uv_checker_tex }, .vao_gpu = cube_vao };

    res.insert(animation_res.init(ctx.arena_allocator));

    const land_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "land", skin_shader, static_shader, uv_checker_tex);

    const model_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined }, model_root);
    const E = find_child(&world, model_root, zeng.skinned_mesh, fet.fresh_query(.{children})).?;
    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.get(E, zeng.skinned_mesh).?.skeleton);
    world.get(model_root, player).?.animation_controller = world.get(E, zeng.skinned_mesh).?.skeleton;
    world.add(input_implement{ .move_fn = input_implement.default_move_fn, .jump_fn = input_implement.default_jump }, model_root);
    world.get(model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root, zeng.world_matrix).?.*, zeng.vec3{ .y = 4.0 });

    const model_root2 = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined }, model_root2);
    const E2 = find_child(&world, model_root2, zeng.skinned_mesh, fet.fresh_query(.{children})).?;
    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.get(E2, zeng.skinned_mesh).?.skeleton);
    world.get(model_root2, player).?.animation_controller = world.get(E2, zeng.skinned_mesh).?.skeleton;
    world.add(input_implement{ .move_fn = input_implement.default_move_fn2, .jump_fn = input_implement.default_jump2 }, model_root2);
    world.get(model_root2, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root2, zeng.world_matrix).?.*, zeng.vec3{ .y = 4.0 });

    const castle_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "medieval", skin_shader, static_shader, uv_checker_tex);
    world.get(castle_root, zeng.world_matrix).?.* = zeng.mat_tran(zeng.mat_scal(zeng.mat_identity, zeng.vec3.ONE.mult(3.5)), zeng.vec3{ .x = -10.0, .z = -6.0, .y = -2.0 });
    const pistol_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "pistol", skin_shader, static_shader, black_tex);
    const head_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "head", skin_shader, static_shader, uv_checker_tex);

    const square_vao, const square_indices_length = zeng.create_square_mesh();
    res.insert(text_render_res{ .shader_program = zeng.load_shader(ctx.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader"), .texture = zeng.load_texture("assets/images/sdf_font.png", false, true), .vao = square_vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    res.insert(zeng.commands{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = ctx.allocator });
    res.insert(time_res{ .delta_time = 0.16, .elapsed_time = 0.0, .dt = 0.006944 });
    res.insert(input_res{ .t_down_last_frame = false });
    res.insert(@as(main_camera_res, undefined));
    res.insert_ptr(&ctx);
    res.insert_ptr(&world);
    res.insert(networking_res{ .main_socket = main_socket, .server_address = server_address, .is_server = is_server });
    res.insert(sound_res{ .first = aud.get_audio_file_data(zeng.get_file_bytes("assets/sounds/ahem.wav", ctx.arena_allocator)) catch unreachable });
    res.insert(@as(cube_tracker_res, undefined));

    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, follow_component{ .target = model_root, .anchor_point = zeng.mat_position(world.get(model_root, zeng.world_matrix).?.*) } });
    res.get(main_camera_res).matrix = world.get(main_camera, zeng.world_matrix).?;
    res.get(main_camera_res).camera = world.get(main_camera, zeng.camera).?;
    zeng.window_resize_handler(ctx.active_window, ctx.active_window.getSize().width, ctx.active_window.getSize().height);
    ctx.active_window.setInputModeCursor(.disabled);

    res.get(cube_tracker_res).map = std.AutoHashMap(dumb, void).init(ctx.allocator);
    defer res.get(cube_tracker_res).map.deinit();
    res.get(cube_tracker_res).cube_mesh = cube_mesh;

    var colliders = std.ArrayList(phy.collider_info).init(ctx.allocator);
    defer colliders.deinit();
    const _data = struct { []zeng.vec3, []u32 }{ util.convert_float_slice_to_vec_slice(cube_collider_pos[0..]), cube_collider_indices[0..] };
    colliders.append(phy.collider_info{ .data = @ptrCast(&_data), .matrix = zeng.mat_identity, .support = undefined, .tag = .mesh }) catch unreachable;
    const __data = struct { []zeng.vec3, []u32 }{ zeng.global_mesh_verts, zeng.global_mesh_indices };
    colliders.append(phy.collider_info{ .data = @ptrCast(&__data), .matrix = zeng.mat_identity, .support = undefined, .tag = .mesh }) catch unreachable;
    res.insert_ptr(&colliders);

    res.get(cube_tracker_res).cube_mesh_data = @ptrCast(&_data);
    res.get(cube_tracker_res).world_mesh_data = @ptrCast(&__data);

    var str_events = events([]const u8).init(ctx.allocator);
    defer str_events.deinit();

    res.insert_ptr(&str_events);

    var top_children = std.ArrayList(ecs.entity_id).init(ctx.allocator);
    top_children.append(model_root) catch unreachable;
    top_children.append(model_root2) catch unreachable;
    top_children.append(pistol_root) catch unreachable;
    top_children.append(castle_root) catch unreachable;
    top_children.append(land_root) catch unreachable;
    top_children.append(head_root) catch unreachable;
    defer top_children.deinit();

    // var f_pressed = false;
    var paused: bool = false;
    while (!ctx.active_window.shouldClose()) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);
        if (is_server) zeng.net.SERVER_recieve_all(main_socket, &res);
        defer if (!is_server) zeng.net.CLIENT_send_all(res.get(zeng.commands));

        res.get(time_res).elapsed_time += res.get(time_res).delta_time;
        if (ctx.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) {
            world.remove(fly_component, main_camera);
            world.add(follow_component{ .target = model_root, .anchor_point = zeng.mat_position(world.get(model_root, zeng.world_matrix).?.*) }, main_camera);
            paused = false;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.y) == zeng.glfw.Action.press) {
            world.add(fly_component{}, main_camera);
            world.remove(follow_component, main_camera);
            paused = true;
        }
        res.get(main_camera_res).matrix = world.get(main_camera, zeng.world_matrix).?;
        res.get(main_camera_res).camera = world.get(main_camera, zeng.camera).?;

        res.insert(debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = res.get(main_camera_res).camera.projection_matrix, .inv_camera_matrix = zeng.mat_invert(res.get(main_camera_res).matrix.*) });

        fet.run_system(mouse_look_system);
        fet.run_system(fly_system);
        if (!paused) {
            fet.run_system(ray_system);
            fet.run_system(spawn_system);
            fet.run_system(player_collision_system);
            fet.run_system(player_logic_system);
            fet.run_system(follower_system);

            const A = fet.fresh_query(.{zeng.world_matrix});
            const B = fet.fresh_query(.{children});
            const C = fet.fresh_query(.{local_matrix});
            world.get(pistol_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(main_camera, zeng.world_matrix).?.*, zeng.mat_mult_vec4(world.get(main_camera, zeng.world_matrix).?.*, zeng.vec4{ .x = 0.15, .y = -0.15, .z = -0.2 }).to_vec3());
            for (top_children.items) |ch| {
                sync_transforms_children(ch, A, B, C);
            }
        }

        zeng.gl.useProgram(sky_shader);
        zeng.gl.bindVertexArray(square_vao);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_world_space"), 1, zeng.gl.FALSE, res.get(main_camera_res).matrix);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_perspective"), 1, zeng.gl.FALSE, &res.get(main_camera_res).camera.projection_matrix);
        zeng.gl.drawElements(zeng.gl.TRIANGLES, square_indices_length, zeng.gl.UNSIGNED_INT, null);
        zeng.gl.clear(zeng.gl.DEPTH_BUFFER_BIT);

        zeng.draw_mesh(cube_mesh, zeng.mat_identity, res.get(main_camera_res).camera.projection_matrix, zeng.mat_invert(res.get(main_camera_res).matrix.*));

        fet.run_system(render_system);
        // var buffer: [64]u8 = undefined;
        // zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / res.get(time_res).delta_time}) catch unreachable, res.get(text_render_res));
        for (str_events.array.items) |str| {
            zeng.draw_text(str, res.get(text_render_res));
        }
        str_events.clear();
        ctx.active_window.swapBuffers();

        res.get(zeng.commands).process_commands(&world);
    }
}

/// Spawns entities random: *std.Random.Xoshiro256,
pub fn spawn_system(ctx: *zeng.engine_context, sound_list: *sound_res, commands: *zeng.commands, input: *input_res) !void {
    _ = commands; // autofix
    const t_down = ctx.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;
    defer input.t_down_last_frame = t_down;

    if (t_down and !input.t_down_last_frame) {
        // if (!netres.is_server) {
        //     commands.remote_call(netres.main_socket, netres.server_address, rpc.CTS_print_hello, .{});
        //     commands.remote_call(netres.main_socket, netres.server_address, rpc.CTS_print_int, .{85});
        // }

        aud.play_sound(sound_list.first, .one_shot);
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn circle_collision_system(q: *ecs.query(.{ zeng.world_matrix, sphere_collider })) !void {
    var A = q.iterator();
    while (A.next()) |_A| {
        const transformA, _ = _A;
        var B = A;
        while (B.next()) |_B| {
            const transformB, _ = _B;
            if (transformA == transformB) unreachable;

            const p_a = zeng.mat_position(transformA.*);
            const p_b = zeng.mat_position(transformB.*);

            const radius = 0.6;
            const delta = p_a.sub(p_b);
            if (delta.length_sq() > 0.0 and delta.length() < 2.0 * radius) {
                const push = (2.0 * radius - delta.length()) * 0.5;
                zeng.mat_position_set(transformB, p_b.add(delta.normalized().mult(-push)));
                zeng.mat_position_set(transformA, p_a.add(delta.normalized().mult(push)));
            }
        }
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn fly_system(ctx: *zeng.engine_context, cam: *main_camera_res, time: *time_res, q: *ecs.query(.{ zeng.world_matrix, fly_component })) !void {
    _ = time; // autofix
    var it = q.iterator();
    while (it.next()) |transform_flyer| {
        const transform, _ = transform_flyer;

        var speed: f32 = 0.2; //@floatCast(time.delta_time * 100.0);
        if (ctx.active_window.getKey(zeng.glfw.Key.left_shift) == zeng.glfw.Action.press) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam.matrix.*).mult(speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.q) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.e) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam.matrix.*).mult(speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam.matrix.*).mult(speed)));
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn mouse_look_system(ctx: *zeng.engine_context, q: *ecs.query(.{ zeng.world_matrix, zeng.camera })) !void {
    var look_iterator = q.iterator();
    while (look_iterator.next()) |transform_camera| {
        const transform, _ = transform_camera;

        const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(ctx.active_window.getCursorPos().xpos * -0.0015));
        const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(ctx.active_window.getCursorPos().ypos * -0.0015));
        transform.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(transform.*));
    }
}

/// Render all entities with a transform and a mesh
pub fn render_system(world: *ecs.world, cam: *main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam.matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_animated_skinned_mesh(world, skin.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    // zeng.draw_text("the end is never the end is never the end is never the ", ui_ren);

}

/// Makes all follower entities follow their target
pub fn follower_system(T: *time_res, follower_q: *ecs.query(.{ follow_component, zeng.world_matrix }), followee_q: *ecs.query(.{ player, zeng.world_matrix })) !void {
    var follower_it = follower_q.iterator();
    while (follower_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.mat_position(followee_q.get(cam_follower.target, zeng.world_matrix).?.*);

        cam_follower.anchor_point = zeng.vec3.lerp(cam_follower.anchor_point, target_position, 5.0 * T.dt);

        zeng.mat_position_set(cam_transform, cam_follower.anchor_point.add(zeng.mat_forward(cam_transform.*).mult(2.5)).add(zeng.vec3{ .y = 0.5 }));
    }
}

pub fn player_collision_system(player_q: *ecs.query(.{ player, zeng.world_matrix }), debug: *debug_res, colliders: *std.ArrayList(phy.collider_info)) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        // var min_t: f32 = 1000.0;
        // var do_it = false;
        // const b_coll = phy.collider_info{ .data = undefined, .matrix = world_matrix.*, .support = phy.sphere };
        const b_coll2 = phy.collider_info{ .data = undefined, .matrix = world_matrix.*, .support = phy.dual_point };
        var a_coll = phy.collider_info{ .data = undefined, .matrix = zeng.mat_identity, .support = phy.triangle };
        const old_grounded = plyr.grounded;
        var closest_dist = std.math.floatMax(f32);
        var cloest_point: zeng.vec3 = undefined;
        var combined_normal = zeng.vec3.ZERO;
        var combined_normal_count: usize = 0;
        plyr.grounded = false;

        for (colliders.items) |coll| {
            if (coll.tag == .mesh) {
                const positions, const indices = @as(*const struct { []zeng.vec3, []u32 }, @alignCast(@ptrCast(coll.data))).*;

                var currr: usize = 0;
                while (currr < indices.len) {
                    defer currr += 3;

                    var tri_data: [3]zeng.vec3 = .{ positions[indices[currr]], positions[indices[currr + 1]], positions[indices[currr + 2]] };
                    a_coll.data = &tri_data;
                    a_coll.matrix = coll.matrix;

                    // var _error = false;
                    // var enter_t: f32 = undefined;
                    // var exit_t: f32 = undefined;
                    // const b = phy.shape_cast(a_coll, b_coll, .{ .y = -1 }, &enter_t, &exit_t, &_error);
                    // if (b) {
                    //     if (enter_t < min_t) min_t = enter_t;
                    //     do_it = true;
                    // }

                    // if (!phy.shape_overlap(a_coll, b_coll)) {
                    // world.get(model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root, zeng.world_matrix).?.*, phy.shape_separation2(a_coll, b_coll, info, NUM));
                    // const v = phy.shape_separation2(a_coll, b_coll, info, NUM);
                    // const p = zeng.mat_position(world.get(model_root, zeng.world_matrix).?.*);
                    // @import("render.zig").draw_triangle(.{ p, p, p.add(v) }, info);

                    // world.get(model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root, zeng.world_matrix).?.*, zeng.vec3{ // similar amount of jitter
                    //     .x = (res.get(std.Random.Xoshiro256).random().float(f32) - 0.5) * 2.0 * 0.0001,
                    //     .y = (res.get(std.Random.Xoshiro256).random().float(f32) - 0.5) * 2.0 * 0.0001,
                    //     .z = (res.get(std.Random.Xoshiro256).random().float(f32) - 0.5) * 2.0 * 0.0001,
                    // });
                    // }
                    // world.get(model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root, zeng.world_matrix).?.*, phy.shape_separation(a_coll, b_coll, info, NUM));

                    const p = phy.shape_separation(a_coll, b_coll2, debug.*, 10);
                    if (p.length() < 0.35) {
                        if (p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                            plyr.grounded = true;
                            plyr.ground_normal = p.neg().normalized();
                        }
                        world_matrix.* = zeng.mat_tran(world_matrix.*, p.add(p.neg().normalized().mult(0.35)));
                        // world.get(model_root, player).?.velocity = world.get(model_root, player).?.velocity.slide(p);
                        combined_normal = combined_normal.add(p.neg().normalized());
                        combined_normal_count += 1;
                    }
                    if (p.length() < closest_dist) {
                        cloest_point = p;
                        closest_dist = p.length();
                    }
                }
            }
        }

        if (old_grounded and !plyr.grounded and closest_dist < 0.5) {
            if (cloest_point.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                plyr.grounded = true;
                plyr.ground_normal = cloest_point.neg().normalized();
                world_matrix.* = zeng.mat_tran(world_matrix.*, cloest_point.add(cloest_point.neg().normalized().mult(0.35)));
            }
        }
        if (combined_normal_count > 0) {
            combined_normal = combined_normal.div(@floatFromInt(combined_normal_count));
            plyr.velocity = plyr.velocity.slide(combined_normal);
        }
        // if (do_it) {
        //     const t = world.get(model_root, zeng.world_matrix).?;
        //     zeng.mat_position_set(t, zeng.mat_position(t.*).add((zeng.vec3{ .y = -1 }).mult(min_t)));
        // }
        // std.debug.print("{}\n", .{zeng.mat_position(world.get(model_root, zeng.world_matrix).?.*).add(mv)});

    }
}

pub fn player_logic_system(T: *time_res, player_q: *ecs.query(.{ player, zeng.world_matrix, input_implement }), animator_q: *ecs.query(.{ zeng.skeleton, animation_component }), ctx: *zeng.engine_context, cam: *main_camera_res, animations: *animation_res) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const p, const m, const i = player_curr;

        const input_vect = i.move_fn(ctx);

        if (i.jump_fn(ctx) and p.grounded) {
            p.velocity = p.velocity.add(zeng.vec3{ .y = 5 });
            p.grounded = false;
            p.ground_normal = zeng.vec3.UP;
        }

        const acc: f32 = 20.0;
        const basis_right = zeng.mat_right(cam.matrix.*).slide(p.ground_normal).normalized();
        const basis_forward = basis_right.cross(p.ground_normal);
        var move_vect = basis_right.mult(input_vect.x).add(basis_forward.mult(input_vect.y));

        var tilt = zeng.vec3.ZERO;
        if (p.grounded) {
            if (input_vect.length() > 0.1) {
                if (p.velocity.length_sq() > 0.01) {
                    const g = move_vect.sub(p.velocity.normalized()).clamp(1.0);
                    const h = g.mult(2.0).add(move_vect).normalized();
                    var h_v = h.project(p.velocity);
                    const h_h = h.sub(h_v);
                    if (p.velocity.length() > 3.8 and h_v.dot(p.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                    tilt = h_v.add(h_h);
                    p.velocity = p.velocity.add(h_v.add(h_h).mult(acc * T.dt));
                } else {
                    p.velocity = p.velocity.add(move_vect.mult(acc * T.dt));
                }
            } else {
                tilt = p.velocity.neg().clamp(1.0);
                p.velocity = p.velocity.add(p.velocity.neg().clamp(acc * T.dt));
            }
        } else {
            p.velocity = p.velocity.add(zeng.vec3.UP.mult(-9.8 * T.dt));
            p.ground_normal = zeng.vec3.UP;
            p.velocity = p.velocity.add(move_vect.mult(acc * 0.3 * T.dt));
            p.velocity = p.velocity.slide(zeng.vec3.UP).add(p.velocity.project(zeng.vec3.UP));
        }
        p.tilt = p.tilt.lerp(tilt, 8.0 * T.dt);
        m.* = zeng.mat_tran(m.*, p.velocity.mult(T.dt));

        if (p.velocity.slide(zeng.vec3.UP).length() > 0.05) {
            p.old_velocity = p.old_velocity.slerp(p.velocity.slide(zeng.vec3.UP).normalized(), 8 * T.dt);
        }
        if (p.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
            const _up = (zeng.vec3.UP.add(p.tilt.mult(0.3))).normalized();
            m.* = zeng.mat_rebasis(m.*, _up.cross(p.old_velocity.slide(_up)).normalized(), _up, p.old_velocity.slide(_up).normalized());
        }

        const anim = animator_q.get(p.animation_controller, animation_component).?;
        const skel = animator_q.get(p.animation_controller, zeng.skeleton).?;
        const blend = p.velocity.div(3.0).clamp(1.0).length();
        anim.time += T.dt / zeng.lerp(animations.items[1].duration, animations.items[4].duration, blend);
        while (anim.time > 1.0) {
            anim.time -= 1.0;
        }

        const rotations = ctx.allocator.alloc(zeng.quat, skel.bone_parent_indices.len) catch unreachable;
        const translations = ctx.allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
        const scales = ctx.allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
        get_animation_pose_with_weight(&animations.items[4], anim.time, .{ rotations, translations, scales }, blend);
        add_animation_pose_with_weight(&animations.items[1], anim.time, .{ rotations, translations, scales }, 1.0 - blend);
        normalize_pose(.{ rotations, translations, scales });
        apply_pose(skel, .{ rotations, translations, scales });
        ctx.allocator.free(rotations);
        ctx.allocator.free(translations);
        ctx.allocator.free(scales);
    }
}

pub fn ray_system(ctx: *zeng.engine_context, world: *ecs.world, camera: *main_camera_res, colliders: *std.ArrayList(phy.collider_info), cube_tracker: *cube_tracker_res, str_events: *events([]const u8)) !void {
    const ro = zeng.mat_position(camera.matrix.*);
    const rd = zeng.mat_forward(camera.matrix.*).neg();
    var min_result: phy.raycast_result = undefined;
    const hit = phy.ray_cast(ro, rd, colliders.items, &min_result);
    if (hit and ctx.active_window.getKey(zeng.glfw.Key.f) == zeng.glfw.Action.press) {
        const new_pos = ro.add(rd.mult(min_result.t)).add(min_result.normal.normalized());
        const new_pos_rounded = zeng.vec3{
            .x = @round(new_pos.x / 2.0) * 2.0,
            .y = @round(new_pos.y / 2.0) * 2.0,
            .z = @round(new_pos.z / 2.0) * 2.0,
        };
        if (!cube_tracker.map.contains(.{ @intFromFloat(new_pos_rounded.x), @intFromFloat(new_pos_rounded.y), @intFromFloat(new_pos_rounded.z) })) {
            _ = world.spawn(.{ cube_tracker.cube_mesh, zeng.mat_tran(zeng.mat_identity, new_pos_rounded) });
            cube_tracker.map.put(.{ @intFromFloat(new_pos_rounded.x), @intFromFloat(new_pos_rounded.y), @intFromFloat(new_pos_rounded.z) }, void{}) catch unreachable;
            colliders.append(phy.collider_info{ .data = cube_tracker.cube_mesh_data, .matrix = zeng.mat_tran(zeng.mat_identity, new_pos_rounded), .support = undefined, .tag = .mesh }) catch unreachable;
        }
    }
    if (hit) str_events.send("hit") else str_events.send("no");
    // f_pressed = ctx.active_window.getKey(zeng.glfw.Key.f) == zeng.glfw.Action.press;
}

// Extra stuff
pub fn find_child(world: *ecs.world, parent: ecs.entity_id, component_type: type, q_children: *ecs.query(.{children})) ?ecs.entity_id {
    if (world.get(parent, component_type) != null) return parent;

    const childrens = world.get(parent, children) orelse return null;
    for (childrens.items) |_c| {
        const res = find_child(world, _c, component_type, q_children);
        if (res != null) return res.?;
    }

    return null;
}
pub fn sync_transforms_children(id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn sync_transforms_recursive(parent_global: zeng.world_matrix, id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const local = q_local_transform.get(id, local_matrix) orelse return;
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    global.* = zeng.mat_mult(parent_global, local.transform);

    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn binary_search(inputs: []const f32, time: f32) usize {
    if (time <= inputs[0]) return 0;
    if (time >= inputs[inputs.len - 1]) return inputs.len - 2;

    var left: usize = 0;
    var right: usize = inputs.len - 1;

    while (left < right - 1) {
        const mid = left + (right - left) / 2;
        if (inputs[mid] <= time) {
            left = mid;
        } else {
            right = mid;
        }
    }
    return left;
}
pub fn animation_pose(anim: *animation_player, delta_time: f32) void {
    const skeleton = anim.skeleton_ptr;
    anim.time += delta_time;
    while (anim.time > anim.animations[anim.current_animation].duration) {
        anim.time -= anim.animations[anim.current_animation].duration;
    }
    var rotations: [100]zeng.quat = .{zeng.quat.IDENTITY} ** 100;
    var translations: [100]zeng.vec3 = .{zeng.vec3.ZERO} ** 100;
    var scales: [100]zeng.vec3 = .{zeng.vec3.ONE} ** 100;

    for (anim.animations[anim.current_animation].channels) |channel| {
        const time = binary_search(channel.inputs, anim.time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[time], channel.inputs[time + 1], anim.time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[time].nlerp(channel.outputs.rotation[time + 1], lerp_amount);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[time].lerp(channel.outputs.translation[time + 1], lerp_amount);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[time].lerp(channel.outputs.scale[time + 1], lerp_amount);
        }
    }

    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotations[curr]), zeng.mat_scal(zeng.mat_identity, scales[curr])), translations[curr]);
        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }
        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}
pub fn get_animation_pose_with_weight(animation: *zeng.Animation, time_norm: f32, pose: Pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight);
        }
    }
}
pub fn add_animation_pose_with_weight(animation: *zeng.Animation, time_norm: f32, pose: Pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = rotations[channel.target].add2(channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .translation) {
            translations[channel.target] = translations[channel.target].add(channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .scale) {
            scales[channel.target] = scales[channel.target].add(channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight));
        }
    }
}
pub fn normalize_pose(pose: Pose) void {
    const rotations = pose[0];
    for (rotations) |*r| {
        r.* = r.normalize();
    }
}
pub fn apply_pose(skeleton: *zeng.skeleton, pose: Pose) void {
    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(pose[0][curr]), zeng.mat_scal(zeng.mat_identity, pose[2][curr])), pose[1][curr]);
        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }
        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}

// need quality collision system - gjk[O] + epa[ ] + shapecast[X] + plane method[ ] -> need spatial acceleration
// make audio system more robust and accurate
// robust text rendering
// better rendering - lights
// better material system
