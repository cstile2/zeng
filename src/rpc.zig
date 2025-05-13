const rpc = @This();
const std = @import("std");
const zeng = @import("zeng.zig");
const main = @import("main.zig");

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
