const std = @import("std");
const zeng = @import("zeng.zig");
const ECS = @import("main.zig").ECS;

pub fn TestNetMessage(f: f32, world: *ECS.World) void {
    var t = zeng.identity_matrix();
    t[13] = f;

    _ = world.spawn(.{
        t,
    }) catch unreachable;
    std.debug.print("i spawned a guy\n", .{});
}
