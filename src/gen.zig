const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    var types = std.StringHashMap(void).init(gpa);
    defer types.deinit();

    try scanDir(gpa, ".", &types);

    try writeOutput(gpa, &types);
}

fn scanDir(alloc: Allocator, path: []const u8, types: *std.StringHashMap(void)) !void {
    var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .directory) {
            if (std.mem.eql(u8, entry.name, ".zig-cache")) continue;
            const sub_path = try std.fs.path.join(alloc, &[_][]const u8{ path, entry.name });
            defer alloc.free(sub_path);
            try scanDir(alloc, sub_path, types);
        } else if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".zig")) {
            const file_path = try std.fs.path.join(alloc, &[_][]const u8{ path, entry.name });
            defer alloc.free(file_path);
            try processFile(alloc, file_path, types);
        }
    }
}

fn processFile(alloc: Allocator, path: []const u8, types: *std.StringHashMap(void)) !void {
    // Skip gen.zig itself to avoid false positives
    if (std.mem.endsWith(u8, path, "gen.zig")) return;

    const source = try std.fs.cwd().readFileAlloc(alloc, path, 10 * 1024 * 1024);
    defer alloc.free(source);

    var i: usize = 0;
    while (std.mem.indexOfPos(u8, source, i, "net_events.send")) |pos| {
        i = pos + 1;
        const paren_pos = std.mem.indexOfPos(u8, source, pos, "(") orelse continue;
        const comma1 = std.mem.indexOfPos(u8, source, paren_pos, ",") orelse continue;
        const comma2 = std.mem.indexOfPos(u8, source, comma1 + 1, ",") orelse continue;
        const type_start = comma1 + 1;
        const type_end = comma2;
        const type_str = std.mem.trim(u8, source[type_start..type_end], " \t\n");
        var name = type_str;
        var is_literal = false;
        if (std.mem.indexOf(u8, type_str, "{")) |brace| {
            name = std.mem.trim(u8, type_str[0..brace], " \t\n");
            is_literal = true;
        } else if (std.mem.indexOf(u8, type_str, ".init")) |init_pos| {
            name = std.mem.trim(u8, type_str[0..init_pos], " \t\n");
            is_literal = true;
        } else {
            // assume variable, find its type
            if (findVarType(source, name)) |var_type| {
                name = var_type;
                is_literal = true;
            } else {
                // Skip unresolved variables - they're not actual types
                continue;
            }
        }
        if (name.len > 0 and is_literal) {
            _ = try types.put(try alloc.dupe(u8, name), {});
        }
    }
}

fn findVarType(source: []const u8, var_name: []const u8) ?[]const u8 {
    const gpa = std.heap.page_allocator;

    const const_pattern = std.fmt.allocPrint(gpa, "const {s}:", .{var_name}) catch return null;
    defer gpa.free(const_pattern);
    if (std.mem.indexOf(u8, source, const_pattern)) |pos| {
        const colon_pos = pos + const_pattern.len - 1;
        const end_pos = std.mem.indexOfPos(u8, source, colon_pos, "=") orelse source.len;
        const type_part = std.mem.trim(u8, source[colon_pos + 1 .. end_pos], " \t\n,");
        // take until space, comma, or end
        var space_pos = std.mem.indexOf(u8, type_part, " ") orelse type_part.len;
        const comma_pos = std.mem.indexOf(u8, type_part, ",") orelse type_part.len;
        if (comma_pos < space_pos) space_pos = comma_pos;
        if (space_pos == 0) return null;
        const result = type_part[0..space_pos];
        return if (result.len > 0) result else null;
    }

    const var_pattern = std.fmt.allocPrint(gpa, "var {s}:", .{var_name}) catch return null;
    defer gpa.free(var_pattern);
    if (std.mem.indexOf(u8, source, var_pattern)) |pos| {
        const colon_pos = pos + var_pattern.len - 1;
        const end_pos = std.mem.indexOfPos(u8, source, colon_pos, "=") orelse source.len;
        const type_part = std.mem.trim(u8, source[colon_pos + 1 .. end_pos], " \t\n,");
        var space_pos = std.mem.indexOf(u8, type_part, " ") orelse type_part.len;
        const comma_pos = std.mem.indexOf(u8, type_part, ",") orelse type_part.len;
        if (comma_pos < space_pos) space_pos = comma_pos;
        if (space_pos == 0) return null;
        const result = type_part[0..space_pos];
        return if (result.len > 0) result else null;
    }
    return null;
}

fn writeOutput(_: Allocator, types: *std.StringHashMap(void)) !void {
    var file = try std.fs.cwd().createFile("src/engine/generated_types.zig", .{});
    defer file.close();

    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    var writer = fbs.writer();

    try writer.writeAll("pub const rpc = @import(\"rpc.zig\");\n");
    try writer.writeAll("pub const std = @import(\"std\");\n");

    try writer.writeAll(
        \\pub const gentype = struct {
        \\  Type: std.builtin.Type,
        \\  size: usize,
        \\  
        \\};
        \\
        \\
    );

    try writer.writeAll("pub const net_event_types = [_]type{\n");

    var it = types.iterator();
    while (it.next()) |entry| {
        try writer.print("    {s},\n", .{entry.key_ptr.*});
    }

    try writer.writeAll("};\n");

    try file.writeAll(buf[0..fbs.pos]);
}
