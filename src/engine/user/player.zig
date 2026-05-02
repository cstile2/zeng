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
const asset_registry = zeng.asset_registry_t;
const time_res = zeng.time_res;

pub const player_component = struct {
    velocity: zeng.vec3,
    old_velocity: zeng.vec3 = zeng.vec3.ZERO,
    ground_normal: zeng.vec3,
    grounded: bool,
    animation_controller: ecs.entity_id,
    tilt: zeng.vec3 = zeng.vec3.ZERO,
    camera: ecs.entity_id,
    enemy: bool = false,
};

pub var shoot_presentation_trigger: bool = false;

/// Spawn a player prefab
pub fn create_player(asset_reg: *asset_registry, world: *ecs.world_t, skin_shader: u32, static_shader: u32, default_tex: u32, fet: *zeng.resource_fetcher_t, top_children: *std.ArrayList(ecs.entity_id), io: std.Io, allocator: std.mem.Allocator, net_id_c: zeng.net_id_component) ecs.entity_id {
    const player_gltf = zeng.loader.auto_import(asset_reg, world, "assets/gltf/people", "KingShiny", skin_shader, static_shader, default_tex, io, allocator, null);
    world.add(player_component{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, player_gltf);
    world.add(rpc.input_message{ .tick = 0, .jump = false, .sprint = false, .move_vect = zeng.vec2.ZERO, .rot_x = 0.0, .rot_y = 0.0, .shoot = false, .aiming = false, .shoot_origin = undefined }, player_gltf);
    world.add(net_id_c, player_gltf);
    world.get(world.get(player_gltf, zeng.children_component).?.items[0], zeng.local_matrix).?.transform = zeng.mat_tran(zeng.mat_identity, .{ .y = -0.84 });

    if (zeng.find_component_of_type(world, player_gltf, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component}))) |player_random_skinned_mesh| {
        const player_skeleton_entity = world.get(player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
        world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, player_skeleton_entity);
        world.get(player_gltf, player_component).?.animation_controller = player_skeleton_entity;
    }

    top_children.append(allocator, player_gltf) catch unreachable;
    return player_gltf;
}
/// Spawn a host player prefab in a server
pub fn construct_local_player(asset_reg: *zeng.asset_registry_t, world: *ecs.world_t, skin_shader: u32, static_shader: u32, uv_checker_tex: u32, fet: *zeng.resource_fetcher_t, top_children: *std.ArrayList(ecs.entity_id), io: std.Io, allocator: std.mem.Allocator) ecs.entity_id {
    const result = zeng.player_module.create_player(asset_reg, world, skin_shader, static_shader, uv_checker_tex, fet, top_children, io, allocator, .{ .net_id = zeng.get_new_netid(), .remote_peer = null });
    world.add(zeng.input_implement{ .move_fn = zeng.input_implement.default_move_fn, .jump_fn = zeng.input_implement.default_jump }, result);
    world.add(@as(zeng.frame_interpolator, undefined), result);
    var found_entity = zeng.find_component_of_type(world, result, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component}));
    while (found_entity) |_| {
        world.remove(zeng.skinned_mesh, found_entity.?);
        found_entity = zeng.find_component_of_type(world, result, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component}));
    }
    zeng.global_player_entity = result;
    return result;
}
pub fn construct_replicated_player(asset_reg: *zeng.asset_registry_t, world: *ecs.world_t, skin_shader: u32, static_shader: u32, uv_checker_tex: u32, fet: *zeng.resource_fetcher_t, top_children: *std.ArrayList(ecs.entity_id), io: std.Io, allocator: std.mem.Allocator) ecs.entity_id {
    const remote_player_entity = zeng.loader.auto_import(asset_reg, world, "assets/gltf/people", "KingShiny", skin_shader, static_shader, uv_checker_tex, io, allocator, null);
    top_children.append(allocator, remote_player_entity) catch unreachable;

    world.get(world.get(remote_player_entity, zeng.children_component).?.items[0], zeng.local_matrix).?.transform = zeng.mat_tran(zeng.mat_identity, .{ .y = -0.84 });

    const remote_player_random_skinned_mesh = zeng.find_component_of_type(world, remote_player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component})).?;
    const remote_player_skeleton_entity = world.get(remote_player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, remote_player_skeleton_entity);
    world.add(zeng.snapshot_interpolator{ .buffer = undefined }, remote_player_entity);

    top_children.append(allocator, remote_player_entity) catch unreachable;
    return remote_player_entity;
}

/// Helper function for euler angles
fn matrix_from_euler(x: f64, y: f64) zeng.world_matrix {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y));
    return zeng.mat_mult(rot_mat_hor, rot_mat_vert);
}

/// Allows the player to shoot their gun
pub fn shoot_system(player_distinguishing: *zeng.player_distinguishing_res, events: *zeng.events_t, res: *zeng.resources_t, players_query: *ecs.query(.{ rpc.input_message, zeng.world_matrix, zeng.net_id_component }), peer_map: *std.AutoHashMap(net.peer_info_t, zeng.client_info), collision_space: *zeng.collision_space_t, world: *ecs.world_t) !void {
    if (world.get(player_distinguishing.main_player_id, rpc.input_message).?.shoot) shoot_presentation_trigger = true;

    const mr = res.get_maybe(zeng.multiplayer_res);
    if (mr != null and !mr.?.is_server) return;

    var involved_entities = std.ArrayList(ecs.entity_id).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer involved_entities.deinit(std.heap.c_allocator);

    var peer_map_iterator = peer_map.iterator();
    while (peer_map_iterator.next()) |peer_map_entry| {
        involved_entities.append(std.heap.c_allocator, peer_map_entry.value_ptr.player) catch unreachable;
    }
    involved_entities.append(std.heap.c_allocator, zeng.global_player_entity) catch unreachable;

    // loop
    var players_it = players_query.iterator();
    while (players_it.next()) |curr| {
        const entity_input: *rpc.input_message, _, const entity_netid: *zeng.net_id_component = curr;

        if (entity_input.shoot) {
            const player_matrix = matrix_from_euler(entity_input.rot_x, entity_input.rot_y);
            const ray_direction = zeng.mat_mult_vec3(player_matrix, zeng.vec3{ .z = -100 });
            const b_coll = phy.convex_collider{ .data = undefined, .matrix = zeng.mat_tran(zeng.mat_identity, entity_input.shoot_origin), .support = &phy.point };

            const shg_result = phy.ray_cast_collision_space(entity_input.shoot_origin, ray_direction, collision_space, events);

            var minimum_t: ?f32 = null;
            var minimum_entity: ecs.entity_id = undefined;

            for (involved_entities.items) |involved_entity| {
                if (involved_entity == players_it.current_entity_id) continue;
                const a_coll = phy.convex_collider{ .data = undefined, .matrix = world.get(involved_entity, zeng.world_matrix).?.*, .support = &phy.player_capsule };

                var enter_t: f32 = undefined;
                var exit_t: f32 = undefined;
                var raycast_error: bool = undefined;
                const did_hit = phy.shape_cast(a_coll, b_coll, ray_direction, &enter_t, &exit_t, &raycast_error);

                if (did_hit) {
                    if (minimum_t == null or enter_t < minimum_t.?) {
                        minimum_t = enter_t;
                        minimum_entity = involved_entity;
                    }
                }
            }

            if ((minimum_t != null) and (!shg_result.hitting or minimum_t.? < shg_result.t)) {
                if (entity_netid.remote_peer) |net_id_remote_peer| {
                    events.send_remote(net_id_remote_peer, rpc.hitmarker{}, .reliable);
                } else {
                    events.send_local(rpc.hitmarker{});
                }

                world.get(minimum_entity, zeng.world_matrix).?.* = zeng.mat_tran(zeng.mat_identity, .{ .y = 5.0 });
                const victim_netid = world.get(minimum_entity, zeng.net_id_component);
                if (victim_netid) |_victim_netid| {
                    if (_victim_netid.remote_peer) |net_id_remote_peer| {
                        events.send_remote(net_id_remote_peer, rpc.got_hit{ .source_position = entity_input.shoot_origin }, .reliable);
                    } else {
                        events.send_local(rpc.got_hit{ .source_position = entity_input.shoot_origin });
                    }
                }
            }
        }
    }
}

/// Runs player collision once per tick
pub fn player_collision_system(player_q: *ecs.query(.{ player_component, zeng.world_matrix }), collision_space: *zeng.collision_space_t, events: *zeng.events_t) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        simulate_collision(plyr, world_matrix, collision_space, events);
    }
}
/// Runs player movement simulation and visual animations once per tick
pub fn player_simulate_and_animate_system(world: *ecs.world_t, pdr: *zeng.player_distinguishing_res, asset_reg: *asset_registry, time: *time_res, player_q: *ecs.query(.{ player_component, rpc.input_message, zeng.world_matrix }), animator_q: *ecs.query(.{ zeng.skeleton, zeng.animation_component })) !void {
    const animation_A = asset_reg.get_maybe("assets/gltf/people/KingShiny.gltf/animations/Idle", zeng.loader.animation).?;
    const animation_B = asset_reg.get_maybe("assets/gltf/people/KingShiny.gltf/animations/Run", zeng.loader.animation).?;

    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const _player, const input: *rpc.input_message, const matrix = player_curr;

        simulate_player(world, _player, input, matrix, time, pdr);

        const anim = animator_q.get(_player.animation_controller, zeng.animation_component).?;
        const skel = animator_q.get(_player.animation_controller, zeng.skeleton).?;
        const blend = _player.velocity.div_scalar(3.0).clamp(1.0).length();

        anim.time += time.fixed_delta_time / zeng.lerp(animation_A.duration, animation_B.duration, blend);
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
pub fn simulate_collision(plyr: *player_component, world_matrix: *zeng.world_matrix, collision_space: *zeng.collision_space_t, events: *zeng.events_t) void {
    var capsule_collider = phy.convex_collider{ .data = undefined, .matrix = world_matrix.*, .support = phy.dual_point };
    const capsule_radius: f32 = 0.35;

    const old_grounded = plyr.grounded;
    var closest_dist = std.math.floatMax(f32);
    var closest_point: zeng.vec3 = undefined;
    var combined_normal = zeng.vec3.ZERO;
    var combined_normal_count: usize = 0;
    plyr.grounded = false;

    var normals = std.ArrayList(zeng.vec3).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer normals.deinit(std.heap.c_allocator);

    const right, const left, const up, const down, const forward, const backward = phy.collider_bound_indices(capsule_collider, capsule_radius + 0.01);

    var already_checked_collider_ptrs = std.AutoHashMap(*phy.convex_collider, void).init(std.heap.c_allocator);
    defer already_checked_collider_ptrs.deinit();

    var tri_count: usize = 0;
    var cell_count: usize = 0;

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
                // events.send_local([3]zeng.vec3{ vec, vec.add(zeng.vec3.UP.mult_scalar(0.1)), vec.add(zeng.vec3.RIGHT.mult_scalar(0.1)) });

                if (collision_space.spatial_hash_grid.get(.{ i, j, k })) |cell_list| {
                    cell_count += 1;
                    for (cell_list.keys()) |collider| {
                        if (already_checked_collider_ptrs.contains(collider)) continue;
                        already_checked_collider_ptrs.put(collider, void{}) catch unreachable;
                        tri_count += 1;

                        _ = events;
                        // const clean = collider.data.mesh_triangle_clean;
                        // const tri = [3]zeng.vec3{
                        //     zeng.mat_mult_vec3(collider.matrix, clean.mesh.positions[clean.mesh.indices[clean.triangle_index + 0]]),
                        //     zeng.mat_mult_vec3(collider.matrix, clean.mesh.positions[clean.mesh.indices[clean.triangle_index + 1]]),
                        //     zeng.mat_mult_vec3(collider.matrix, clean.mesh.positions[clean.mesh.indices[clean.triangle_index + 2]]),
                        // };
                        // events.send_local(tri);

                        const p = phy.shape_separation(collider.*, capsule_collider, 10);
                        if (p.length() < capsule_radius) {
                            if (p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                                plyr.grounded = true;
                                plyr.ground_normal = p.neg().normalized();
                            }
                            world_matrix.* = zeng.mat_tran(world_matrix.*, p.add(p.neg().normalized().mult_scalar(capsule_radius)));
                            capsule_collider.matrix = world_matrix.*;
                            combined_normal = combined_normal.add(p.neg().normalized());
                            combined_normal_count += 1;
                            normals.append(std.heap.c_allocator, p) catch unreachable;
                        }
                        if (p.length() < closest_dist and p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                            closest_point = p;
                            closest_dist = p.length();
                        }
                    }
                }
            }
        }
    }

    if (old_grounded and !plyr.grounded and closest_dist < 0.6) {
        if (closest_point.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
            plyr.grounded = true;
            plyr.ground_normal = closest_point.neg().normalized();
            world_matrix.* = zeng.mat_tran(world_matrix.*, closest_point.add(closest_point.neg().normalized().mult_scalar(capsule_radius)));
            plyr.velocity = plyr.velocity.slide(plyr.ground_normal);
            return;
        }
    }
    if (combined_normal_count > 0) plyr.velocity = plyr.velocity.slide(combined_normal);
}
/// Player movement and logic - designed to be multiple times per frame for latency compensation
pub fn simulate_player(world: *ecs.world_t, _player: *player_component, input: *const rpc.input_message, matrix: *zeng.world_matrix, time: *time_res, main_player_res: *zeng.player_distinguishing_res) void {
    if (!_player.enemy) {
        const rotated_matrix = matrix_from_euler(input.rot_x, input.rot_y);
        const basis_right = zeng.mat_right(rotated_matrix).slide(_player.ground_normal).normalized();
        const basis_forward = basis_right.cross(_player.ground_normal);
        const move_vect = basis_right.mult_scalar(input.move_vect.x).add(basis_forward.mult_scalar(input.move_vect.y));
        simulate_player_wth_move_vect(move_vect, _player, input, matrix, time);
    } else {
        const p = zeng.mat_position(world.get(main_player_res.main_player_id, zeng.world_matrix).?.*);
        const d = p.sub(zeng.mat_position(matrix.*)).slide(.UP).clamp(1.0);
        simulate_player_wth_move_vect(d, _player, input, matrix, time);
    }
}

pub fn simulate_player_wth_move_vect(move_vect: zeng.vec3, _player: *player_component, input: *const rpc.input_message, matrix: *zeng.world_matrix, time: *time_res) void {
    if (input.jump and _player.grounded) {
        _player.velocity = _player.velocity.add(zeng.vec3{ .y = 4 });
        _player.grounded = false;
        _player.ground_normal = zeng.vec3.UP;
    }

    const max_speed: f32 = if (input.sprint and !input.aiming) 5.0 else 2.5;
    const acc: f32 = 60.0;

    if (_player.grounded) {
        if (move_vect.length() > 0.1) {
            if (_player.velocity.length_sq() > 0.01) {
                const g = move_vect.sub(_player.velocity.normalized()).clamp(1.0);
                const h = g.mult_scalar(2.0).add(move_vect).normalized();
                var h_v = h.project(_player.velocity);
                const h_h = h.sub(h_v);
                if (_player.velocity.length() > max_speed and h_v.dot(_player.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                _player.velocity = _player.velocity.add(h_v.add(h_h).mult_scalar(acc * time.fixed_delta_time));
            } else {
                _player.velocity = _player.velocity.add(move_vect.mult_scalar(acc * time.fixed_delta_time));
            }
        } else {
            _player.velocity = _player.velocity.add(_player.velocity.neg().clamp(acc * time.fixed_delta_time));
        }
        _player.velocity = _player.velocity.slide(zeng.vec3.UP).clamp(max_speed).add(_player.velocity.project(zeng.vec3.UP));
    } else {
        _player.velocity = _player.velocity.add(zeng.vec3.UP.mult_scalar(-9.8 * time.fixed_delta_time));
        _player.ground_normal = zeng.vec3.UP;
        _player.velocity = _player.velocity.add(move_vect.mult_scalar(acc * 0.1 * time.fixed_delta_time));
        _player.velocity = _player.velocity.slide(zeng.vec3.UP).clamp(max_speed).add(_player.velocity.project(zeng.vec3.UP));
    }
    matrix.* = zeng.mat_tran(matrix.*, _player.velocity.mult_scalar(time.fixed_delta_time));

    if (_player.velocity.slide(zeng.vec3.UP).length() > 0.05) {
        _player.old_velocity = _player.old_velocity.slerp(_player.velocity.slide(zeng.vec3.UP).normalized(), 8 * time.fixed_delta_time);
    }
    if (_player.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
        const _up = zeng.vec3.UP;
        matrix.* = zeng.mat_rebasis(matrix.*, _up.cross(_player.old_velocity.slide(_up)).normalized(), _up, _player.old_velocity.slide(_up).normalized());
    }

    if (zeng.mat_position(matrix.*).y < -30.0) {
        zeng.mat_position_set(matrix, .{ .y = 20.0, .x = -5.0 });
        _player.velocity = zeng.vec3.ZERO;
    }
}

// pub fn adhoc_collision_routine(input_collider: phy.convex_collider, spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.convex_collider)), debug: *debug_res) ?zeng.vec3 {
//     const my_collider = input_collider;

//     var closest_dist = std.math.floatMax(f32);
//     var cloest_point: ?zeng.vec3 = null;

//     const right, const left, const up, const down, const forward, const backward = phy.collider_bound_indices(my_collider);

//     var relevant_collision_cells = std.ArrayList(std.ArrayList(*phy.convex_collider)).initCapacity(std.heap.c_allocator, 0) catch unreachable;
//     defer relevant_collision_cells.deinit(std.heap.c_allocator);

//     var redundant_colliders = std.AutoHashMap(*phy.convex_collider, void).init(std.heap.c_allocator);
//     defer redundant_colliders.deinit();

//     var i: isize = left;
//     while (i <= right) {
//         defer i += 1;

//         var j: isize = down;
//         while (j <= up) {
//             defer j += 1;

//             var k: isize = backward;
//             while (k <= forward) {
//                 defer k += 1;

//                 // const vec = zeng.vec3{
//                 //     .x = @as(f32, @floatFromInt(i)) * phy.GRID_SIZE,
//                 //     .y = @as(f32, @floatFromInt(j)) * phy.GRID_SIZE,
//                 //     .z = @as(f32, @floatFromInt(k)) * phy.GRID_SIZE,
//                 // };
//                 // tri_ev.send(.{ vec, vec.add(zeng.vec3.UP.mult(0.1)), vec.add(zeng.vec3.RIGHT.mult(0.1)) });

//                 const cell_of_colliders = spatial_hash_grid.get(.{ i, j, k });
//                 if (cell_of_colliders != null) relevant_collision_cells.append(std.heap.c_allocator, cell_of_colliders.?) catch unreachable;
//             }
//         }
//     }

//     for (relevant_collision_cells.items) |curr_collision_cell| {
//         for (curr_collision_cell.items) |curr_collider| {
//             if (redundant_colliders.contains(curr_collider)) continue;
//             redundant_colliders.put(curr_collider, void{}) catch unreachable;

//             std.debug.assert(curr_collider.tag == .support_based); // just for now

//             // const coll_data = @as(*const phy.mesh_triangle_data, @ptrCast(@alignCast(curr_collider.data)));
//             // tri_ev.send(.{
//             //     zeng.mat_mult_vec4(curr_collider.matrix, coll_data.positions[coll_data.indices[0]].to_vec4(1.0)).to_vec3(),
//             //     zeng.mat_mult_vec4(curr_collider.matrix, coll_data.positions[coll_data.indices[1]].to_vec4(1.0)).to_vec3(),
//             //     zeng.mat_mult_vec4(curr_collider.matrix, coll_data.positions[coll_data.indices[2]].to_vec4(1.0)).to_vec3(),
//             // });

//             const p = phy.shape_separation(curr_collider.*, my_collider, debug.*, 10);
//             if (p.length() < closest_dist) {
//                 cloest_point = p;
//                 closest_dist = p.length();
//             }
//         }
//     }

//     return cloest_point;
// }
