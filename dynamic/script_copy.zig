export fn on_button_pressed(res: *zeng.resources_t) callconv(.c) void {
    const _player = dll_compatible_res_get(res, zeng.main_player_res);
    const world = dll_compatible_res_get(res, zeng.ecs.world);
    const player = world.get(_player.id, zeng.Player.player).?;
    player.velocity.y += 10;
    if (true) {}
}
const std = @import("std");
const zeng = @import("zeng");

pub const hot_reload_procedures = extern struct {
    update: *const @TypeOf(update),
    on_button_pressed: *const @TypeOf(on_button_pressed),
};

pub fn dll_compatible_res_get(res: *zeng.resources_t, T: type) *T {
    var it = res.map.iterator();
    while (it.next()) |curr| {
        const ptr = @as([*:0]const u8, @ptrFromInt(curr.key_ptr.*));
        if (std.mem.eql(u8, ptr[0..std.mem.len(ptr)], @typeName(T))) {
            return @as(*T, @ptrCast(@alignCast(curr.value_ptr.*)));
        }
    }
    unreachable;
}

export fn update(res: *zeng.resources_t) callconv(.c) void {
    _ = res;
}

pub export fn get_game_api() *const hot_reload_procedures {
    return &.{
        .update = update,
        .on_button_pressed = on_button_pressed,
    };
}
