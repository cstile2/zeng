const rpc = @This();
const std = @import("std");
const zeng = @import("zeng.zig");
const main = @import("main.zig");
const net = @import("networking.zig");

pub const REMOTE_PROCEDURES = .{
    rpc.CTS_print_hello,
    rpc.CTS_print_int,
};

pub fn CTS_print_hello() void {
    std.debug.print("this is being printed on the server!\n", .{});
}

pub fn CTS_print_int(i: i32) void {
    std.debug.print("here: {}\n", .{i});
}

pub const player_spawn_message = struct {};
pub const snapshot_message = struct {
    position: zeng.vec3,
};
pub const input_message = struct {
    tick: isize,
    jump: bool,
    move_vect: zeng.vec2,
    dx: f64,
    dy: f64,
};
pub const state_correction = struct {
    tick: isize,
    state: main.player,
    world_matrix: zeng.world_matrix,
};
pub const client_tick = struct {
    time: f64,
};
pub const server_tick_offset = struct {
    server_time: f64,
    client_time: f64,
};
pub const missed_input = struct {};
pub const input_chunck = struct {
    arr: [30]input_message,
};
// client send sync to server
// server send sync to client

pub const REMOTE_MESSAGE_TYPES = .{
    player_spawn_message,
    snapshot_message,
    input_message,
    state_correction,
    client_tick,
    server_tick_offset,
    missed_input,
    input_chunck,
};
