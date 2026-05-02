const std = @import("std");
const zeng = @import("zeng");

export fn on_button_pressed(res: *zeng.resources_t) callconv(.c) void {
    const _player = get_item(res, zeng.player_distinguishing_res);
    const world = get_item(res, zeng.ecs.world_t);
    const player = world.get(_player.main_player_id, zeng.player_module.player_component).?;
    player.velocity.y += 10;
    player.grounded = false;
}

pub const hot_reload_procedures = extern struct {
    on_button_pressed: *const @TypeOf(on_button_pressed),
};
pub fn get_item(res: *zeng.resources_t, T: type) *T {
    var it = res.map.iterator();
    while (it.next()) |curr| {
        const ptr = @as([*:0]const u8, @ptrFromInt(curr.key_ptr.*));
        if (std.mem.eql(u8, ptr[0..std.mem.len(ptr)], @typeName(T))) {
            return @as(*T, @ptrCast(@alignCast(curr.value_ptr.*)));
        }
    }
    unreachable;
}
pub export fn get_game_api() *const hot_reload_procedures {
    return &.{
        .on_button_pressed = on_button_pressed,
    };
}
