pub const rpc = @import("rpc.zig");
pub const std = @import("std");
pub const gentype = struct {
    Type: std.builtin.Type,
    size: usize,
};
pub const zeng = @import("zeng.zig");

pub const net_event_types = [_]type{
    rpc.speed_client_up,
    rpc.state_correction,
    rpc.player_spawn_message,
    rpc.world_update,
    rpc.client_tick,
    rpc.hitmarker,
    rpc.input_chunk,
    rpc.server_tick_offset,
    [3]zeng.vec3,
    zeng.delete_event,
    zeng.phy.debug_draw_stuff,
    rpc.got_hit,
};
