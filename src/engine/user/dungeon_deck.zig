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
const msg = zeng.msg;

pub const fire_ball_component = struct {
    velocity: zeng.vec3,
    bouncy: bool = false,
    damage: f32 = 10,
    time_till_despawn: f32 = 2,
    every_bounce_increases_dmg: bool = false,
};
pub const health_component = struct {
    health: f32,
};
pub const card_caster = struct {
    fire_ball_prototype: fire_ball_component,
};
pub const ghost_component = struct {
    timer: f32,
};
pub const playing_card = struct {
    play_fn: *const fn () void,
};
pub const playing_card_context = struct {
    cube_mesh: zeng.mesh,
};
pub const entity_collider_res = struct {
    colliders: std.AutoHashMap(ecs.entity_id, phy.convex_collider),

    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.colliders = @TypeOf(this.colliders).init(allocator);
    }
    pub fn deinit(this: *@This()) void {
        this.colliders.deinit();
    }
};

pub fn shoot_fireball(pos: zeng.vec3, vel: zeng.vec3, res: *zeng.resources_t, fire_ball_prototype: fire_ball_component) void {
    aud.play_sound(res.get(zeng.asset_registry).get("sounds/fireball.wav", aud.audio_sample_info).*, .one_shot);

    const world = res.get(ecs.world_t);

    var fire_ball = fire_ball_prototype;
    fire_ball.velocity = vel;

    _ = world.spawn(.{
        zeng.mat_scal(zeng.mat_tran(zeng.mat_identity, pos), zeng.vec3.ONE.mult(0.2)),
        zeng.sprite3D{ .size = undefined, .texture = undefined },
        fire_ball,
    });
}

pub fn playing_card_fire_ball(res: *zeng.resources_t) void {
    // const commands = res.get(zeng.commands);
    const world = res.get(ecs.world_t);
    const main_cam_info = res.get(zeng.main_camera_res);
    const main_player_info = res.get(zeng.main_player_res);
    const player_card_caster = world.get(main_player_info.id, card_caster).?;
    const cam_matrix = world.get(main_cam_info.id, zeng.world_matrix).?;

    shoot_fireball(zeng.mat_position(cam_matrix.*).add(zeng.mat_forward(cam_matrix.*).mult(-0.5)), zeng.mat_forward(cam_matrix.*).mult(-10.0), res, player_card_caster.fire_ball_prototype);

    // var fire_ball = player_card_caster.fire_ball_prototype;
    // fire_ball.velocity = zeng.mat_forward(cam_matrix.*).mult(-10.0);

    // commands.spawn(.{
    //     zeng.mat_scal(cam_matrix.*, zeng.vec3.ONE.mult(0.2)),
    //     zeng.sprite3D{ .size = undefined, .texture = undefined },
    //     fire_ball,
    // });
}
pub fn playing_card_bouncify(res: *zeng.resources_t) void {
    const world = res.get(ecs.world_t);
    const main_player_info = res.get(zeng.main_player_res);
    const player_card_caster = world.get(main_player_info.id, card_caster).?;

    player_card_caster.fire_ball_prototype.bouncy = true;
}
pub fn playing_card_inc_lifetime(res: *zeng.resources_t) void {
    const world = res.get(ecs.world_t);
    const main_player_info = res.get(zeng.main_player_res);
    const player_card_caster = world.get(main_player_info.id, card_caster).?;

    player_card_caster.fire_ball_prototype.time_till_despawn *= 1.5;
}
pub fn playing_card_bounce_more_damage(res: *zeng.resources_t) void {
    const world = res.get(ecs.world_t);
    const main_player_info = res.get(zeng.main_player_res);
    const player_card_caster = world.get(main_player_info.id, card_caster).?;

    player_card_caster.fire_ball_prototype.every_bounce_increases_dmg = true;
}

pub fn fire_ball_system(q: *ecs.query(.{ zeng.world_matrix, fire_ball_component }), q_health: *ecs.query(.{health_component}), delete_events: *msg(zeng.delete_event), time: *zeng.time_res, debug: *zeng.debug_res, spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.convex_collider)), asset_reg: *zeng.asset_registry, entity_colliders: *entity_collider_res) !void {
    var it = q.iterator();
    while (it.next()) |curr| {
        const entity_matrix, const entity_fire_ball: *fire_ball_component = curr;
        entity_matrix.* = zeng.mat_tran(entity_matrix.*, entity_fire_ball.velocity.mult(time.fixed_dt));

        const input_collider = phy.convex_collider{ .data = undefined, .matrix = entity_matrix.*, .support = phy.sphere, .tag = .support_based };
        var cp = zeng.Player.adhoc_collision_routine(input_collider, spatial_hash_grid, debug);

        var maybe_entity: ?ecs.entity_id = null;
        var coll_it = entity_colliders.colliders.iterator();
        while (coll_it.next()) |curr_entity_collider| {
            const _cp = phy.shape_separation(curr_entity_collider.value_ptr.*, input_collider, debug.*, 10);
            if (cp == null or _cp.length() < cp.?.length()) {
                cp = _cp;
                maybe_entity = curr_entity_collider.key_ptr.*;
            }
        }

        if (cp != null and cp.?.length() < 0.2) {
            if (maybe_entity) |actual_entity| {
                const entity_health = q_health.get(actual_entity, health_component).?;
                entity_health.health -= entity_fire_ball.damage;
                aud.play_sound(asset_reg.get("sounds/damage.wav", aud.audio_sample_info).*, .one_shot);
                delete_events.send(.{ .entity_id = it.HACKY_CURR_ENTITY_ID });
            } else {
                if (entity_fire_ball.bouncy) {
                    if (entity_fire_ball.every_bounce_increases_dmg) entity_fire_ball.damage += 10;

                    entity_fire_ball.velocity = entity_fire_ball.velocity.slide(cp.?).add(entity_fire_ball.velocity.project(cp.?).neg());
                } else delete_events.send(.{ .entity_id = it.HACKY_CURR_ENTITY_ID });
            }
        }

        entity_fire_ball.time_till_despawn -= time.fixed_dt;
        if (entity_fire_ball.time_till_despawn <= 0.0) {
            delete_events.send(.{ .entity_id = it.HACKY_CURR_ENTITY_ID });
        }
    }
}
pub fn sync_entity_colliders_system(entity_colliders: *entity_collider_res, world: *ecs.world_t) !void {
    var delete_commands = std.ArrayList(ecs.entity_id).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer delete_commands.deinit(std.heap.c_allocator);

    var coll_it = entity_colliders.colliders.iterator();
    while (coll_it.next()) |curr_entity_collider| {
        const matrix = (world.get_checked(curr_entity_collider.key_ptr.*, zeng.world_matrix) catch {
            delete_commands.append(std.heap.c_allocator, curr_entity_collider.key_ptr.*) catch unreachable;
            continue;
        }).?;
        curr_entity_collider.value_ptr.matrix = matrix.*;
    }

    for (delete_commands.items) |deleted_id| {
        _ = entity_colliders.colliders.remove(deleted_id);
    }
}
pub fn health_die_system(q: *ecs.query(.{health_component}), delete_events: *msg(zeng.delete_event)) !void {
    var it = q.iterator();
    while (it.next()) |curr| {
        const entity_health = curr[0];

        if (entity_health.health <= 0.0) {
            delete_events.send(.{ .entity_id = it.HACKY_CURR_ENTITY_ID });
        }
    }
}
pub fn cast_system(mouse_state: *zeng.mouse_state_res, asset_reg: *zeng.asset_registry, res: *zeng.resources_t) !void {
    if (mouse_state.mouse_pressed) {
        if (zeng.get_key(.num_1)) {
            aud.play_sound(asset_reg.get("sounds/spell.wav", aud.audio_sample_info).*, .one_shot);
            playing_card_bounce_more_damage(res);
        } else if (zeng.get_key(.num_2)) {
            aud.play_sound(asset_reg.get("sounds/spell.wav", aud.audio_sample_info).*, .one_shot);
            playing_card_bouncify(res);
        } else if (zeng.get_key(.num_3)) {
            aud.play_sound(asset_reg.get("sounds/spell.wav", aud.audio_sample_info).*, .one_shot);
            playing_card_inc_lifetime(res);
        } else if (zeng.get_key(.num_3)) {
            aud.play_sound(asset_reg.get("sounds/spell.wav", aud.audio_sample_info).*, .one_shot);
            playing_card_inc_lifetime(res);
        } else {
            playing_card_fire_ball(res);
        }
    }
}
pub fn ghost_system(q: *ecs.query(.{ ghost_component, zeng.world_matrix }), world: *ecs.world_t, main_player: *zeng.main_player_res, time: *zeng.time_res, res: *zeng.resources_t) !void {
    const player_matrix = world.get(main_player.id, zeng.world_matrix).?;
    const player_pos = zeng.mat_position(player_matrix.*);

    var it = q.iterator();
    while (it.next()) |curr| {
        const entity_ghost: *ghost_component, const entity_matrix = curr;

        const ghost_pos = zeng.mat_position(entity_matrix.*);
        const delta = player_pos.sub(ghost_pos);

        entity_ghost.timer -= time.fixed_dt;
        if (entity_ghost.timer <= 0.0) {
            entity_ghost.timer = 3.0;

            const mypos = zeng.mat_position(entity_matrix.*);
            const delta_vec = delta.normalized();
            shoot_fireball(mypos.add(delta_vec.mult(2.0)), delta_vec.mult(10.0), res, .{ .velocity = undefined, .bouncy = true, .time_till_despawn = 5.0 });
        }

        entity_matrix.* = zeng.mat_rebasis(entity_matrix.*, zeng.vec3.UP.cross(delta.neg().slide(zeng.vec3.UP)).normalized(), zeng.vec3.UP, delta.neg().slide(zeng.vec3.UP).normalized());
        entity_matrix.* = zeng.mat_tran(entity_matrix.*, delta.normalized().mult(time.fixed_dt));
    }
}
