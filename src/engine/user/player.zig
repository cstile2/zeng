const std = @import("std");
const zeng = @import("../zeng.zig");
const ecs = zeng.ecs;
const rpc = zeng.rpc;
const phy = zeng.phy;
const aud = zeng.aud;
const util = zeng.utils;
const net = zeng.net;
const gl = zeng.gl;
const c = zeng.c;

const debug_res = zeng.debug_res;
const Datablob = zeng.Datablob;
const time_res = zeng.time_res;

pub const player = struct {
    velocity: zeng.vec3,
    old_velocity: zeng.vec3 = zeng.vec3.ZERO,
    ground_normal: zeng.vec3,
    grounded: bool,
    animation_controller: ecs.entity_id,
    tilt: zeng.vec3 = zeng.vec3.ZERO,
    camera: ecs.entity_id,
};

pub fn create_player(datablob: *Datablob, world: *ecs.world, skin_shader: u32, static_shader: u32, uv_checker_tex: u32, fet: *zeng.resource_fetcher, top_children: *std.ArrayList(ecs.entity_id), allocator: std.mem.Allocator, net_id_c: zeng.net_id_component) ecs.entity_id {
    const player_gltf = zeng.loader.auto_import(datablob, world, "assets/gltf", "static_test", skin_shader, static_shader, uv_checker_tex, allocator);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, player_gltf);
    world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2.ZERO, .rot_x = 0.0, .rot_y = 0.0, .shoot = false }, player_gltf);
    world.add(net_id_c, player_gltf);

    // find the first instance of a skinned mesh component > retrieve the entity with that skeleton > add animation component to the player skeleton entity > ...
    // attach skeleton entity to the animation_controller on player > add netid to player
    const player_random_skinned_mesh = zeng.find_component_of_type(world, player_gltf, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
    const player_skeleton_entity = world.get(player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, player_skeleton_entity);
    world.get(player_gltf, player).?.animation_controller = player_skeleton_entity;

    top_children.append(allocator, player_gltf) catch unreachable;
    return player_gltf;
}

pub fn _player_matrix_from_rotations(x: f64, y: f64) zeng.world_matrix {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    return zeng.mat_mult(rot_mat_hor, rot_mat_vert);
}

pub fn player_collision_system(player_q: *ecs.query(.{ player, zeng.world_matrix }), debug: *debug_res, tri_ev: *zeng.events([3]zeng.vec3), spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info))) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        simulate_collision(plyr, world_matrix, spatial_hash_grid, tri_ev, debug);
    }
}
/// Runs player movement simulation and visual animations once per tick
pub fn player_simulate_and_animate_system(datablob: *Datablob, time: *time_res, player_q: *ecs.query(.{ player, rpc.input_message, zeng.world_matrix }), animator_q: *ecs.query(.{ zeng.skeleton, zeng.animation_component })) !void {
    const animation_A = datablob.get("assets/gltf/static_test.gltf/animations/idle", zeng.loader.animation);
    const animation_B = datablob.get("assets/gltf/static_test.gltf/animations/run_in_place2", zeng.loader.animation);

    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const _player, const input: *rpc.input_message, const matrix = player_curr;

        simulate_player(_player, input, matrix, time);

        const anim = animator_q.get(_player.animation_controller, zeng.animation_component).?;
        const skel = animator_q.get(_player.animation_controller, zeng.skeleton).?;
        const blend = _player.velocity.div(3.0).clamp(1.0).length();

        anim.time += time.fixed_dt / zeng.lerp(animation_A.duration, animation_B.duration, blend);
        while (anim.time > 1.0) {
            anim.time -= 1.0;
        }
        const pose = zeng.create_pose(std.heap.c_allocator, skel.*);
        zeng.get_animation_pose_with_weight(animation_B, anim.time, pose, blend);
        zeng.add_animation_pose_with_weight(animation_A, anim.time, pose, 1.0 - blend);
        zeng.normalize_pose_quaternions(pose);
        zeng.apply_pose_to_skeleton(skel, pose);
        zeng.free_pose(std.heap.c_allocator, pose);
    }
}
/// Collision detection for players - designed to be run multiple times per frame for latency compensation
pub fn simulate_collision(plyr: *player, world_matrix: *zeng.world_matrix, spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)), tri_ev: *zeng.events([3]zeng.vec3), debug: *debug_res) void {
    _ = tri_ev; // autofix
    const b_coll = phy.collider_info{ .data = undefined, .matrix = world_matrix.*, .support = phy.dual_point };
    const old_grounded = plyr.grounded;
    var closest_dist = std.math.floatMax(f32);
    var cloest_point: zeng.vec3 = undefined;
    var combined_normal = zeng.vec3.ZERO;
    var combined_normal_count: usize = 0;
    plyr.grounded = false;

    const right, const left, const up, const down, const forward, const backward = phy.collider_bounds(b_coll);

    var collection = std.ArrayList(std.ArrayList(*phy.collider_info)).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer collection.deinit(std.heap.c_allocator);

    var already_checked = std.AutoHashMap(*phy.collider_info, void).init(std.heap.c_allocator);
    defer already_checked.deinit();

    var i: isize = left;
    while (i <= right) {
        defer i += 1;

        var j: isize = down;
        while (j <= up) {
            defer j += 1;

            var k: isize = backward;
            while (k <= forward) {
                defer k += 1;

                // const vec = zeng.vec3{
                //     .x = @as(f32, @floatFromInt(i)) * phy.GRID_SIZE,
                //     .y = @as(f32, @floatFromInt(j)) * phy.GRID_SIZE,
                //     .z = @as(f32, @floatFromInt(k)) * phy.GRID_SIZE,
                // };
                // tri_ev.send(.{ vec, vec.add(zeng.vec3.UP.mult(0.1)), vec.add(zeng.vec3.RIGHT.mult(0.1)) });

                const guy = spatial_hash_grid.get(.{ i, j, k });
                if (guy != null) collection.append(std.heap.c_allocator, guy.?) catch unreachable;
            }
        }
    }

    for (collection.items) |Q| {
        for (Q.items) |coll| {
            if (already_checked.contains(coll)) continue;
            already_checked.put(coll, void{}) catch unreachable;

            if (coll.tag != .support_based) unreachable; // just for now

            // const coll_data = @as(*const phy.mesh_triangle_data, @alignCast(@ptrCast(coll.data)));

            // tri_ev.send(.{
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[0]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[1]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[2]].to_vec4(1.0)).to_vec3(),
            // });

            const p = phy.shape_separation(coll.*, b_coll, debug.*, 10);
            if (p.length() < 0.35) {
                if (p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                    plyr.grounded = true;
                    plyr.ground_normal = p.neg().normalized();
                }
                world_matrix.* = zeng.mat_tran(world_matrix.*, p.add(p.neg().normalized().mult(0.35)));
                combined_normal = combined_normal.add(p.neg().normalized());
                combined_normal_count += 1;
            }
            if (p.length() < closest_dist) {
                cloest_point = p;
                closest_dist = p.length();
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
        plyr.velocity = plyr.velocity.slide(combined_normal);
    }
}
/// Player movement and logic - designed to be multiple times per frame for latency compensation
pub fn simulate_player(_player: *player, input: *const rpc.input_message, matrix: *zeng.world_matrix, time: *time_res) void {
    const rotated_matrix = _player_matrix_from_rotations(input.rot_x, input.rot_y);

    if (input.jump and _player.grounded) {
        _player.velocity = _player.velocity.add(zeng.vec3{ .y = 6 });
        _player.grounded = false;
        _player.ground_normal = zeng.vec3.UP;
    }

    const acc: f32 = 60.0;
    const basis_right = zeng.mat_right(rotated_matrix).slide(_player.ground_normal).normalized();
    const basis_forward = basis_right.cross(_player.ground_normal);
    var move_vect = basis_right.mult(input.move_vect.x).add(basis_forward.mult(input.move_vect.y));

    var tilt = zeng.vec3.ZERO;
    if (_player.grounded) {
        if (input.move_vect.length() > 0.1) {
            if (_player.velocity.length_sq() > 0.01) {
                const g = move_vect.sub(_player.velocity.normalized()).clamp(1.0);
                const h = g.mult(2.0).add(move_vect).normalized();
                var h_v = h.project(_player.velocity);
                const h_h = h.sub(h_v);
                if (_player.velocity.length() > 3.8 and h_v.dot(_player.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                tilt = h_v.add(h_h);
                _player.velocity = _player.velocity.add(h_v.add(h_h).mult(acc * time.fixed_dt));
            } else {
                _player.velocity = _player.velocity.add(move_vect.mult(acc * time.fixed_dt));
            }
        } else {
            tilt = _player.velocity.neg().clamp(1.0);
            _player.velocity = _player.velocity.add(_player.velocity.neg().clamp(acc * time.fixed_dt));
        }
    } else {
        _player.velocity = _player.velocity.add(zeng.vec3.UP.mult(-9.8 * time.fixed_dt));
        _player.ground_normal = zeng.vec3.UP;
        _player.velocity = _player.velocity.add(move_vect.mult(acc * 0.1 * time.fixed_dt));
        _player.velocity = _player.velocity.slide(zeng.vec3.UP).add(_player.velocity.project(zeng.vec3.UP));
    }
    _player.tilt = _player.tilt.lerp(tilt, 8.0 * time.fixed_dt);
    matrix.* = zeng.mat_tran(matrix.*, _player.velocity.mult(time.fixed_dt));

    if (_player.velocity.slide(zeng.vec3.UP).length() > 0.05) {
        _player.old_velocity = _player.old_velocity.slerp(_player.velocity.slide(zeng.vec3.UP).normalized(), 8 * time.fixed_dt);
    }
    if (_player.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
        const _up = (zeng.vec3.UP.add(_player.tilt.mult(0.3))).normalized();
        matrix.* = zeng.mat_rebasis(matrix.*, _up.cross(_player.old_velocity.slide(_up)).normalized(), _up, _player.old_velocity.slide(_up).normalized());
    }

    if (zeng.mat_position(matrix.*).y < -30.0) {
        zeng.mat_position_set(matrix, .{ .y = 20.0, .x = -5.0 });
        _player.velocity = zeng.vec3.ZERO;
    }
}

pub fn shoot_system(q: *ecs.query(.{ rpc.input_message, zeng.world_matrix, zeng.net_id_component }), datablob: *Datablob, peer_map: *std.AutoHashMap(net.peer_info_t, zeng.client_info), world: *ecs.world, commands: *zeng.commands, tracker: *net.packet_ack_tracker_t, hitmarker_events: *zeng.events(rpc.hitmarker)) !void {
    var players = std.ArrayList(ecs.entity_id).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer players.deinit(std.heap.c_allocator);

    var it_ = peer_map.iterator();
    while (it_.next()) |e| {
        players.append(std.heap.c_allocator, e.value_ptr.player) catch unreachable;
    }
    players.append(std.heap.c_allocator, zeng.global_player_entity) catch unreachable;

    var it = q.iterator();
    while (it.next()) |curr| {
        const input: *rpc.input_message, const matrix, const netid: *zeng.net_id_component = curr;
        if (input.shoot) {
            for (players.items) |thing| {
                if (matrix == world.get(thing, zeng.world_matrix)) continue;
                const a_coll = phy.collider_info{ .data = undefined, .matrix = world.get(thing, zeng.world_matrix).?.*, .support = &phy.player_capsule, .tag = .support_based };
                const b_coll = phy.collider_info{ .data = undefined, .matrix = matrix.*, .support = &phy.point, .tag = .support_based };

                const player_matrix = _player_matrix_from_rotations(input.rot_x, input.rot_y);
                const ray_direction = zeng.mat_mult_vec3(player_matrix, zeng.vec3{ .z = -1 });
                var enter_t: f32 = undefined;
                var exit_t: f32 = undefined;
                var _error: bool = undefined;
                const result = phy.shape_cast(a_coll, b_coll, ray_direction, &enter_t, &exit_t, &_error);

                if (result) {
                    if (netid.remote_peer) |net_id_remote_peer| {
                        net.remote_event(commands, tracker, datablob.get("main_socket", net.socket_t).*, net_id_remote_peer, rpc.hitmarker{}, .reliable);
                    } else {
                        hitmarker_events.send(.{});
                    }

                    world.get(thing, zeng.world_matrix).?.* = zeng.mat_identity;
                }
            }
        }
    }
}
