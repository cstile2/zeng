const std = @import("std");

pub export fn test_print(str: [*]const u8, str_len: usize) void {
    std.debug.print("this is from a DLL! : <{s}>\n", .{str[0..str_len]});
}
