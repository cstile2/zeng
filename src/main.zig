const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const utils = @import("utils.zig");
const parser = @import("parser.zig");

pub const ComponentTypes = [_]type{
    zeng.mesh,
    zeng.camera,
    zeng.skinned_mesh,
    zeng.world_matrix,
    oscilate_component,
    sphere_collider,
    fly_component,
    follow_component,
    children,
    local_matrix,
};
pub const ECS = ecs.CompileECS(ComponentTypes);
pub const oscilate_component = struct {
    offset: f32 = 0.0,
};
pub const sphere_collider = struct {
    radius: f32 = 1.0,
};
pub const player_event = struct {
    amt: f32,
    num: u16,
};
pub const fly_component = struct {};
pub const follow_component = struct {
    anchor_point: zeng.vec3,
    target: ECS.entity_id,
};
pub const animation_player = struct {
    time: f32,
    current_animation: usize,
    animations: []parser.Animation,
    skeleton: *zeng.skeleton,
};
pub const children = struct {
    items: []ECS.entity_id,
};
pub const local_matrix = struct {
    transform: zeng.world_matrix = zeng.mat_identity,
};

pub const ResourceTypes = [_]type{
    ECS.world,
    zeng.engine_state,
    zeng.Commands,

    time_res,
    input_res,
    main_camera_res,
    shared_render_res,
    text_render_res,

    std.Random.Xoshiro256,
};
pub const time_res = struct {
    delta_time: f64,
    elapsed_time: f64,
};
pub const input_res = struct {
    t_down_last_frame: bool,
};
pub const main_camera_res = struct {
    matrix: *[16]f32,
    camera: *zeng.camera,
};
pub const shared_render_res = struct {
    skinned_mesh: zeng.skinned_mesh,
};
pub const text_render_res = struct {
    shader_program: u32,
    texture: u32,
    vao: u32,
    indices_len: c_int,
};

pub fn main() !void {
    const server_socket_address = try zeng.networking.do_setup(true);
    defer zeng.networking.undo_setup(server_socket_address);

    var gd: zeng.engine_state = undefined;
    var res: zeng.Resources = undefined;
    var world: ECS.world = undefined;
    var dep: zeng.resource_fetcher = undefined;
    try zeng.engine_start(&gd, &res, &world, &dep);
    defer zeng.engine_end(&gd, &res, &world);

    const default_shader = zeng.load_shader(gd.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const uv_checker_tex = zeng.load_texture("assets/images/uv_checker.png", true, true);
    const skin_shader = zeng.load_shader(gd.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const mesh_slice, const animation_slice, _ = parser.extract(parser.parse_gltf(zeng.get_file_bytes("assets/gltf/superduper2.gltf", gd.arena_allocator), gd.arena_allocator), "assets/gltf/superduper2.bin", "assets/gltf/", gd.arena_allocator, skin_shader);

    var anim = animation_player{ .skeleton = undefined, .time = 0.0, .animations = animation_slice, .current_animation = 0 };
    var entity_list = std.ArrayList(ECS.entity_id).init(gd.arena_allocator);
    for (mesh_slice) |mesh_like| {
        if (mesh_like == .skinned) {
            const id = try world.spawn(.{
                zeng.mat_identity,
                local_matrix{},
                mesh_like.skinned,
            });
            anim.skeleton = mesh_like.skinned.skeleton;
            entity_list.append(id) catch unreachable;
        }
    }
    const root = try world.spawn(.{
        zeng.translated(zeng.mat_identity, .{ .z = 2.0 }),
        sphere_collider{ .radius = 1.0 },
        children{ .items = entity_list.items },
    });

    const vao, const length = zeng.create_square_mesh();
    res.insert(gd.arena_allocator, text_render_res{ .shader_program = zeng.load_shader(gd.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader"), .texture = zeng.load_texture("assets/images/sdf_font.png", false, true), .vao = vao, .indices_len = length });
    res.insert(gd.arena_allocator, std.Random.DefaultPrng.init(123));
    res.insert(gd.arena_allocator, zeng.Commands{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = gd.allocator });
    res.insert(gd.arena_allocator, time_res{ .delta_time = 0.16, .elapsed_time = 0.0 });
    res.insert(gd.arena_allocator, input_res{ .t_down_last_frame = false });
    res.insert(gd.arena_allocator, @as(main_camera_res, undefined));

    const new_camera_entity = try world.spawn(.{
        zeng.camera{ .projection_matrix = undefined },
        zeng.mat_identity,
        fly_component{},
        sphere_collider{ .radius = 1.0 },
    });
    res.get(main_camera_res).matrix, res.get(main_camera_res).camera = try world.get(world._locations[new_camera_entity], .{ zeng.world_matrix, zeng.camera });
    zeng.window_resize(gd.active_window, gd.window_width, gd.window_height);
    gd.active_window.setInputModeCursor(zeng.glfw.Window.InputModeCursor.disabled);
    _ = zeng.instantiate_scene(&world, "assets/blender_files/main_scene.bin", gd.allocator, default_shader, uv_checker_tex);

    while (!gd.active_window.shouldClose()) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);

        res.get(time_res).elapsed_time += res.get(time_res).delta_time;
        dep.run_system(spawn_system);
        dep.run_system(oscilate_system);
        dep.run_system(fly_system);
        dep.run_system(mouse_look_system);
        dep.run_system(follower_system);
        dep.run_system(circle_collision_system);
        sync_children(root, dep.fresh_query(.{zeng.world_matrix}), dep.fresh_query(.{children}), dep.fresh_query(.{local_matrix}));
        animation_pose(&anim, @floatCast(res.get(time_res).delta_time));
        dep.run_system(render_system);

        res.get(zeng.Commands).process_commands(&world);
    }
}

/// Test system
pub fn spawn_system(gd: *zeng.engine_state, commands: *zeng.Commands, input: *input_res, rand: *std.rand.Xoshiro256) !void {
    if ((gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) and !input.t_down_last_frame) {
        var curr: u32 = 0;
        while (curr < 10) {
            defer curr += 1;
            commands.spawn(.{
                zeng.translated(zeng.mat_identity, zeng.vec3{ .x = rand.random().float(f32), .y = rand.random().float(f32), .z = rand.random().float(f32) }),
                oscilate_component{ .offset = rand.random().float(f32) * (2.0 * 3.14159) },
                // sh.skinned_mesh, // this makes it crash
                sphere_collider{ .radius = 1.0 },
            });
        }
    }
    input.t_down_last_frame = gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;
    if (gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) {
        gd.active_window.setInputModeCursor(zeng.glfw.Window.InputModeCursor.normal);
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn circle_collision_system(q: *ECS.query(.{ zeng.world_matrix, sphere_collider })) !void {
    var A = try zeng.iterator(.{ zeng.world_matrix, sphere_collider }).create(q);
    while (A.next()) |_A| {
        const transformA, _ = _A;

        var B = A;
        while (B.next()) |_B| {
            const transformB, _ = _B;

            const p_a = zeng.get_column_vector(transformA.*, 3);
            const p_b = zeng.get_column_vector(transformB.*, 3);

            const radius = 0.6;
            const delta = p_a.sub(p_b);
            if (delta.length_sq() > 0.0 and delta.length() < 2.0 * radius) {
                const push = (2.0 * radius - delta.length()) * 0.5;
                zeng.set_mat_position(transformB, p_b.add(delta.normalized().mult(-push)));
                zeng.set_mat_position(transformA, p_a.add(delta.normalized().mult(push)));
            }
        }
    }
}

/// Make all entities with a SineMover component and a transform float around randomly
pub fn oscilate_system(time: *time_res, q: *ECS.query(.{ zeng.world_matrix, oscilate_component })) !void {
    var sine_iterator = try zeng.iterator(.{ zeng.world_matrix, oscilate_component }).create(q);
    while (sine_iterator.next()) |transform_sine| {
        const transform, const sine_mover = transform_sine;

        const localtime = @as(f32, @floatCast(time.elapsed_time)) - sine_mover.offset;
        const dt = @as(f32, @floatCast(time.delta_time));
        transform.* = zeng.translated(transform.*, zeng.vec3{ .x = @cos(localtime * 5.0) * dt, .y = @sin(localtime * 4.0) * dt, .z = @sin(localtime * 3.0) * dt });
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn fly_system(gd: *zeng.engine_state, cam: *main_camera_res, time: *time_res, q: *ECS.query(.{ zeng.world_matrix, fly_component })) !void {
    var it = try zeng.iterator(.{ zeng.world_matrix, fly_component }).create(q);
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
pub fn mouse_look_system(gd: *zeng.engine_state, q: *ECS.query(.{ zeng.world_matrix, zeng.camera })) !void {
    var look_iterator = try zeng.iterator(.{ zeng.world_matrix, zeng.camera }).create(q);
    while (look_iterator.next()) |transform_camera| {
        const transform, _ = transform_camera;

        var pos: [3]f32 = undefined;
        pos[0] = transform[12];
        pos[1] = transform[13];
        pos[2] = transform[14];
        const rot_mat_hor = zeng.mat_axis_angle(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.active_window.getCursorPos().xpos * -0.0015));
        const rot_mat_vert = zeng.mat_axis_angle(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.active_window.getCursorPos().ypos * -0.0015));
        transform.* = zeng.mat_mult(rot_mat_hor, rot_mat_vert);
        transform[12] = pos[0];
        transform[13] = pos[1];
        transform[14] = pos[2];
    }
}

pub fn sync_children(id: ECS.entity_id, q_transform: *ECS.query(.{zeng.world_matrix}), q_children: *ECS.query(.{children}), q_local_transform: *ECS.query(.{local_matrix})) void {
    const global = q_transform.get_maybe(id, zeng.world_matrix) orelse return;
    const childrens = q_transform.get_maybe(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn sync_transforms(parent_global: zeng.world_matrix, id: ECS.entity_id, q_transform: *ECS.query(.{zeng.world_matrix}), q_children: *ECS.query(.{children}), q_local_transform: *ECS.query(.{local_matrix})) void {
    const local = q_local_transform.get_maybe(id, local_matrix) orelse return;
    const global = q_transform.get_maybe(id, zeng.world_matrix) orelse return;
    global.* = zeng.mat_mult(parent_global, local.transform);

    const childrens = q_transform.get_maybe(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn animation_pose(anim: *animation_player, delta_time: f32) void {
    const skeleton = anim.skeleton;
    anim.time += delta_time;
    while (anim.time > anim.animations[anim.current_animation].duration) {
        anim.time -= anim.animations[anim.current_animation].duration;
    }
    var rotations: [100]zeng.quat = .{zeng.quat{ .x = 0, .y = 0, .z = 0, .w = 1.0 }} ** 100;
    var translations: [100]zeng.vec3 = .{zeng.vec3{ .x = 0, .y = 0, .z = 0 }} ** 100;
    var scales: [100]zeng.vec3 = .{zeng.vec3{ .x = 1.0, .y = 1.0, .z = 1.0 }} ** 100;

    for (anim.animations[anim.current_animation].channels) |channel| {
        if (channel.outputs == .rotation) {
            var time: usize = 0;
            while ((time < channel.inputs.len - 1) and !(channel.inputs[time] <= anim.time and anim.time < channel.inputs[time + 1])) {
                time += 1;
            }
            rotations[channel.target] = channel.outputs.rotation[time].nlerp(channel.outputs.rotation[time + 1], zeng.inv_lerp(channel.inputs[time], channel.inputs[time + 1], anim.time));
        } else if (channel.outputs == .translation) {
            var time: usize = 0;
            while ((time < channel.inputs.len - 1) and !(channel.inputs[time] <= anim.time and anim.time < channel.inputs[time + 1])) {
                time += 1;
            }
            translations[channel.target] = channel.outputs.translation[time].lerp(channel.outputs.translation[time + 1], zeng.inv_lerp(channel.inputs[time], channel.inputs[time + 1], anim.time));
        } else if (channel.outputs == .scale) {
            var time: usize = 0;
            while ((time < channel.inputs.len - 1) and !(channel.inputs[time] <= anim.time and anim.time < channel.inputs[time + 1])) {
                time += 1;
            }
            scales[channel.target] = channel.outputs.scale[time].lerp(channel.outputs.scale[time + 1], zeng.inv_lerp(channel.inputs[time], channel.inputs[time + 1], anim.time));
        }
    }

    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.translated(zeng.mat_mult(zeng.quat_to_mat(rotations[curr]), zeng.scaled(zeng.mat_identity, scales[curr])), translations[curr]);

        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1 and parent_index < skeleton.bone_parent_indices.len) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }

        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);

        curr += 1;
    }
}

/// Render all entities with a transform and a mesh
pub fn render_system(gd: *zeng.engine_state, ui_ren: *text_render_res, cam: *main_camera_res, render_q: *ECS.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ECS.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    zeng.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    zeng.gl.clear(zeng.gl.COLOR_BUFFER_BIT | zeng.gl.DEPTH_BUFFER_BIT);

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam.matrix.*);

    var render_iterator = try zeng.iterator(.{ zeng.world_matrix, zeng.mesh }).create(render_q);
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = try zeng.iterator(.{ zeng.world_matrix, zeng.skinned_mesh }).create(skinned_q);
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_animated_skinned_mesh(skin.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    zeng.draw_text("work in progress ;)", ui_ren);

    gd.active_window.swapBuffers();
}

/// Makes all follower entities follow their target
pub fn follower_system(cam_q: *ECS.query(.{ follow_component, zeng.world_matrix }), q: *ECS.query(.{ fly_component, zeng.world_matrix })) !void {
    var cam_it = try zeng.iterator(.{ follow_component, zeng.world_matrix }).create(cam_q);
    while (cam_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.mat_position(q.get_maybe(cam_follower.target, zeng.world_matrix).?.*);

        cam_follower.anchor_point = zeng.vec3.lerp(cam_follower.anchor_point, target_position, 0.1);

        zeng.set_mat_position(cam_transform, cam_follower.anchor_point.add(zeng.mat_z_axis(cam_transform.*).mult(2.0).add(zeng.vec3{ .y = 1.0 })));
    }
}

// fuller gltf support
// need quality collision system - plane method + shapecast
// need sounds
// better rendering - lights

// next up - need to import textures + materials(?)
// perhaps make the skeleton an entity, and reference it using edl? or can use a hashmap resource
