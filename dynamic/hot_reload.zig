const std = @import("std");
const zeng = @import("zeng");

pub const HotReloadAPI = extern struct {
    update: *const @TypeOf(update),
    render: *const @TypeOf(render),
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

export fn update(res: *zeng.resources_t) callconv(.c) void {
    const player = get_item(res, zeng.main_player_res);
    const world = get_item(res, zeng.ecs.world);
    const M = world.get(player.id, zeng.world_matrix).?;

    M.* = zeng.mat_tran(M.*, .{ .x = 0.02 });
}

export fn render() callconv(.c) void {
    std.debug.print("render rand\n", .{});
}

pub export fn get_game_api() *const HotReloadAPI {
    return &.{
        .update = update,
        .render = render,
    };
}
