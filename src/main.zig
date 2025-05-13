const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const phy = @import("physics.zig");
const aud = @import("audio.zig");

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
    ground_normal: zeng.vec3,
    grounded: bool,
};

pub const debug_res = @import("render.zig").triangle_debug_info;
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

pub fn main() !void {
    var is_server: bool = true;
    {
        std.debug.print("\nselect network mode:\n", .{});
        const thing = std.io.getStdIn().reader().readBoundedBytes(1) catch unreachable;
        if (std.mem.eql(u8, thing.buffer[0..1], "s")) {
            std.debug.print("SERVER\n", .{});
        } else if (std.mem.eql(u8, thing.buffer[0..1], "c")) {
            std.debug.print("CLIENT\n", .{});
            is_server = false;
        }
    }
    // const main_socket, const server_address = zeng.net.do_setup("192.168.1.104", 12345, is_server) catch unreachable;
    // defer zeng.net.undo_setup(main_socket);

    var ctx: zeng.engine_context = undefined;
    var res: zeng.resources_t = undefined;
    var world: ecs.world = undefined;
    var fet: zeng.resource_fetcher = undefined;
    try zeng.engine_start(&ctx, &res, &world, &fet);
    defer zeng.engine_end(&ctx, &res, &world);

    const triangle_vao, const triangle_vbo = zeng.create_triangle_mesh();

    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});

    const sky_shader = zeng.load_shader(ctx.allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const static_shader = zeng.load_shader(ctx.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.load_shader(ctx.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.load_shader(ctx.allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.load_texture("assets/images/uv_checker.png", true, false);
    var singular_animation_player = animation_player{ .skeleton_ptr = undefined, .time = 0.0, .animations = undefined, .current_animation = 3 };

    const land_root = zeng.instantiate(&world, &ctx, "assets/gltf/land.gltf", "assets/gltf/land.bin", skin_shader, static_shader, uv_checker_tex, null);
    const model_root = zeng.instantiate(&world, &ctx, "assets/gltf/static_test.gltf", "assets/gltf/static_test.bin", skin_shader, static_shader, uv_checker_tex, &singular_animation_player);
    const castle_root = zeng.instantiate(&world, &ctx, "assets/gltf/medieval.gltf", "assets/gltf/medieval.bin", skin_shader, static_shader, uv_checker_tex, null);
    world.get(castle_root, zeng.world_matrix).?.* = zeng.mat_tran(zeng.mat_scal(zeng.mat_identity, zeng.vec3.ONE.mult(3.5)), zeng.vec3{ .z = -6.0, .y = 2.0 });
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false }, model_root);

    const square_vao, const square_indices_length = zeng.create_square_mesh();
    res.insert(text_render_res{ .shader_program = zeng.load_shader(ctx.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader"), .texture = zeng.load_texture("assets/images/sdf_font.png", false, true), .vao = square_vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    res.insert(zeng.commands{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = ctx.allocator });
    res.insert(time_res{ .delta_time = 0.16, .elapsed_time = 0.0, .dt = 0.006944 });
    res.insert(input_res{ .t_down_last_frame = false });
    res.insert(@as(main_camera_res, undefined));
    res.insert(ctx);
    // res.insert(networking_res{ .main_socket = main_socket, .server_address = server_address, .is_server = is_server });
    res.insert(sound_res{ .first = aud.get_audio_file_data(zeng.get_file_bytes("assets/sounds/ahem.wav", ctx.arena_allocator)) catch unreachable });

    const main_camera = world.spawn(.{
        zeng.camera{ .projection_matrix = undefined },
        zeng.mat_identity,
        fly_component{},
        sphere_collider{ .radius = 1.0 },
    });
    res.get(main_camera_res).matrix = world.get(main_camera, zeng.world_matrix).?;
    res.get(main_camera_res).camera = world.get(main_camera, zeng.camera).?;
    zeng.window_resize_handler(ctx.active_window, ctx.active_window.getSize().width, ctx.active_window.getSize().height);
    ctx.active_window.setInputModeCursor(.disabled);

    var paused: bool = false;
    while (!ctx.active_window.shouldClose()) {
        // std.time.sleep(std.time.ns_per_ms * 8);
        zeng.start_of_frame();
        // if (is_server) zeng.net.SERVER_recieve_all(main_socket, &res);

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
        // zeng.gl.clearColor(0.4, 0.7, 0.9, 1.0);
        zeng.gl.clear(zeng.gl.COLOR_BUFFER_BIT);

        res.insert(debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = res.get(main_camera_res).camera.projection_matrix, .inv_camera_matrix = zeng.mat_invert(res.get(main_camera_res).matrix.*) });

        fet.run_system(mouse_look_system);
        fet.run_system(fly_system);
        if (!paused) {
            animation_pose(&singular_animation_player, @floatCast(res.get(time_res).dt));

            fet.run_system(spawn_system);
            fet.run_system(circle_collision_system);
            fet.run_system(player_system0);
            fet.run_system(player_system);
            fet.run_system(follower_system);

            sync_transforms_children(castle_root, fet.fresh_query(.{zeng.world_matrix}), fet.fresh_query(.{children}), fet.fresh_query(.{local_matrix}));
            sync_transforms_children(land_root, fet.fresh_query(.{zeng.world_matrix}), fet.fresh_query(.{children}), fet.fresh_query(.{local_matrix}));
            sync_transforms_children(model_root, fet.fresh_query(.{zeng.world_matrix}), fet.fresh_query(.{children}), fet.fresh_query(.{local_matrix}));
        }
        zeng.gl.useProgram(sky_shader);
        zeng.gl.bindVertexArray(square_vao);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_world_space"), 1, zeng.gl.FALSE, res.get(main_camera_res).matrix);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_perspective"), 1, zeng.gl.FALSE, &res.get(main_camera_res).camera.projection_matrix);
        zeng.gl.drawElements(zeng.gl.TRIANGLES, square_indices_length, zeng.gl.UNSIGNED_INT, null);
        zeng.gl.clear(zeng.gl.DEPTH_BUFFER_BIT);
        fet.run_system(render_system);

        @import("render.zig").draw_triangle(.{ zeng.vec3.ZERO, zeng.vec3.RIGHT.mult(0.05), zeng.vec3.UP.mult(0.05) }, res.get(debug_res).*);

        ctx.active_window.swapBuffers();

        res.get(zeng.commands).process_commands(&world);
        // if (!is_server) zeng.net.CLIENT_send_all(res.get(zeng.commands));
        zeng.end_of_frame(&res);
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
pub fn render_system(tt: *time_res, ui_ren: *text_render_res, cam: *main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam.matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_animated_skinned_mesh(skin.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    // zeng.draw_text("the end is never the end is never the end is never the ", ui_ren);
    var buffer: [64]u8 = undefined;
    zeng.draw_text(try std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / tt.delta_time}), ui_ren);
}

/// Makes all follower entities follow their target
pub fn follower_system(T: *time_res, follower_q: *ecs.query(.{ follow_component, zeng.world_matrix }), followee_q: *ecs.query(.{ player, zeng.world_matrix })) !void {
    var follower_it = follower_q.iterator();
    while (follower_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.mat_position(followee_q.get(cam_follower.target, zeng.world_matrix).?.*);

        cam_follower.anchor_point = zeng.vec3.lerp(cam_follower.anchor_point, target_position, 5.0 * T.dt);

        zeng.mat_position_set(cam_transform, cam_follower.anchor_point.add(zeng.mat_forward(cam_transform.*).mult(1.8)).add(zeng.vec3{ .y = 0.5 }));
    }
}

pub fn player_system0(player_q: *ecs.query(.{ player, zeng.world_matrix }), debug: *debug_res) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        var currr: usize = 0;
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
        while (currr < zeng.global_mesh_indices.len) {
            defer currr += 3;

            var tri_data: [3]zeng.vec3 = .{ zeng.global_mesh_verts[zeng.global_mesh_indices[currr]], zeng.global_mesh_verts[zeng.global_mesh_indices[currr + 1]], zeng.global_mesh_verts[zeng.global_mesh_indices[currr + 2]] };
            a_coll.data = &tri_data;

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

pub fn player_system(T: *time_res, player_q: *ecs.query(.{ player, zeng.world_matrix }), ctx: *zeng.engine_context, cam: *main_camera_res) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const p, const m = player_curr;

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
        input_vect = input_vect.clamp(1);

        if (ctx.active_window.getKey(zeng.glfw.Key.space) == zeng.glfw.Action.press and p.grounded) {
            p.velocity = p.velocity.add(zeng.vec3{ .y = 5 });
            p.grounded = false;
            p.ground_normal = zeng.vec3.UP;
        }

        if (!p.grounded) {
            p.velocity = p.velocity.add(zeng.vec3.UP.mult(-9.8 * T.dt));
            p.ground_normal = zeng.vec3.UP;
        }
        const acc: f32 = 40.0;
        const basis_right = zeng.mat_right(cam.matrix.*).slide(p.ground_normal).normalized();
        const basis_forward = basis_right.cross(p.ground_normal);
        var move_vect = basis_right.mult(input_vect.x).add(basis_forward.mult(input_vect.y));

        if (p.grounded) {
            p.velocity = p.velocity.add(move_vect.mult(acc * T.dt));
            if (input_vect.length() < 0.1) p.velocity = p.velocity.add(p.velocity.neg().clamp(acc * T.dt));
            p.velocity = p.velocity.clamp(3.8);
        } else {
            p.velocity = p.velocity.add(move_vect.mult(acc * 0.3 * T.dt));
            p.velocity = p.velocity.slide(zeng.vec3.UP).clamp(3.5).add(p.velocity.project(zeng.vec3.UP));
        }

        if (p.velocity.slide(zeng.vec3.UP).length() > 0.1)
            m.* = zeng.mat_rebasis(m.*, p.velocity.slide(zeng.vec3.UP).cross(zeng.vec3.UP.neg()).normalized(), zeng.vec3.UP, p.velocity.slide(zeng.vec3.UP).normalized());

        var o = zeng.mat_position(m.*);
        o = o.add(p.velocity.mult(T.dt));
        zeng.mat_position_set(m, o);
    }
}

// Extra stuff
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
pub fn get_animation_pose(animation: *zeng.Animation, time: f32, skeleton: *zeng.skeleton) void {
    _ = skeleton; // autofix
    // var _time = __time;
    // _time += delta_time;
    // while (_time > animation.duration) {
    //     _time -= animation.duration;
    // }

    var rotations: [100]zeng.quat = .{zeng.quat.IDENTITY} ** 100;
    var translations: [100]zeng.vec3 = .{zeng.vec3.ZERO} ** 100;
    var scales: [100]zeng.vec3 = .{zeng.vec3.ONE} ** 100;
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount);
        }
    }

    // var curr: usize = 0;
    // while (curr < skeleton.bone_parent_indices.len) {
    //     skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotations[curr]), zeng.mat_scal(zeng.mat_identity, scales[curr])), translations[curr]);
    //     const parent_index = skeleton.bone_parent_indices[curr];
    //     if (parent_index != -1) {
    //         skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
    //     }
    //     skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
    //     curr += 1;
    // }
}

// next up - need to import materials(?) and create rendering to support them
// fix the way skeletons are stored (currently floating in arena allocator)

// need quality collision system - gjk[O] + epa[ ] + shapecast[X] + plane method[ ]
// make audio system more robust and accurate
// robust text rendering
// better rendering - lights

// idea for object dependencies - have objects publish themselves to a global dependency resource (hashmap), allowing other objects to get a direct reference to components
// the objects which publish themselves are responsible for maintaining their references and deleting themselves from the registry

// ecs alternative: objects have stable mem addresses in contiguous arrays. objects link together using relationships/trees
// relationship_map: std.AutoHashMap(struct{*anyopaque, *anyopaque}, void) = undefined; - best for o(1) determining if relationship exists
// relationship_map: std.AutoHashMap(*anyopaque, List(*anyopaque)) = undefined; - best for traversing graph
// parent_map: []usize; - best for children knowing their parent - only works for tree graphs

// general graphs - adjacency list map, trees - parent map

// entities and the components themselves must be accessed through entity_id's - which feels bad cuz seems slow and is tedious

// if entity tables recycle rows instead of filling them immediately, we can make unstable ID's be stable! They are already so close to being stable
// new form entity_id (table_index, row_index) -> index into the worlds new table array to acquire table pointer, dereference it, index using row - MUCH better
// if table pointers were stable, and rows were stable, entity (component) pointers would also be stable - until deletion

// THE PLAN: make component pointers stable: liveness will be determined through some global structure like a hashmap to determine generation number
// this keeps sanity checks optional, meaning if we are sure that certain pointers are good to go, we can use them directly - easier to look at and faster
// entity references are strange tho - we don't just need a single component at all times - so we have the entity layer and component layer. We probably need both.
