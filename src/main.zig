const std = @import("std");
const zeng = @import("zeng");
const ecs = zeng.ecs;
const rpc = zeng.rpc;
const phy = zeng.phy;
const aud = zeng.aud;
const util = zeng.utils;
const ui = zeng.ui;
const net = zeng.net;
const gl = zeng.gl;
const c = zeng.c;
const vec2 = zeng.vec2;
const n = ui.n;
const dungeon_deck = zeng.dungeon_deck;
const msg = zeng.msg;
const hot_reload = @import("hot_reload");

pub fn page_widget(wooden_tex: u32, noise_tex: u32, w: f32, h: f32, split_t: f32) *ui.ui_node {
    _ = wooden_tex;
    _ = noise_tex;

    return n(.{ .width = w, .height = h, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = .CLEAR }, &.{
        n(.{ .width = @round(split_t * w), .width_size_mode = .fixed, .height_size_mode = .grow, .color = .CLEAR, .direction = .bottom_to_top }, &.{
            n(.{ .width_size_mode = .grow, .height_size_mode = .grow, .radius = 8, .color = .CLEAR, .id = "left", .padding = 8, .gap = 8 }, &.{}),
            n(.{ .height = 90, .width = 100, .width_size_mode = .grow, .height_size_mode = .fixed, .color = .BLACK, .align_children_perp = 1, .align_children_para = 1, .gap = 4 }, &.{
                button_widget(20, 20, null),
                button_widget(40, 40, "play_button"),
                button_widget(20, 20, null),
            }),
        }),
    });
}
pub fn button_widget(w: f32, h: f32, id: ?[]const u8) *ui.ui_node {
    return n(.{ .width = w, .width_size_mode = .fixed, .height = h, .height_size_mode = .fixed, .radius = 4, .color = zeng.render.color.GRAY, .id = id }, &.{});
}
pub fn menu_widget(tex: u32) *ui.ui_node {
    return n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .gap = 3, .radius = 3, .padding = 3, .color = zeng.render.color.GRAY, .direction = .bottom_to_top }, &.{
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
        n(.{ .height_size_mode = .fit, .width_size_mode = .fit, .radius = 3, .padding = 3, .gap = 3 }, &.{
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
            n(.{ .width = 15, .height = 15, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = zeng.render.color.LIME, .radius = 3, .img = tex }, &.{}),
        }),
    });
}
pub fn card_widget(cover: u32) *ui.ui_node {
    return n(.{ .width_size_mode = .fixed, .height_size_mode = .fixed, .width = 100, .height = 140, .color = zeng.render.color.GRAY, .direction = .bottom_to_top, .radius = 8, .padding = 4, .gap = 4 }, &.{
        n(.{ .width_size_mode = .grow, .height_size_mode = .match, .img = cover, .radius = 8 }, &.{}),
        n(.{ .width_size_mode = .grow, .height_size_mode = .fixed, .height = 10, .color = zeng.render.color.BLACK, .radius = 3 }, &.{}),
    });
}
var global_num: usize = 0;
pub fn block_widget(B: *cb_code_block, pos: vec2) *ui.ui_node {
    var total_width: usize = 0;
    var buff = zeng.global_allocator.alloc(u8, 255) catch unreachable;
    for (B.params) |param| {
        if (param == .Label) {
            @memcpy(buff[total_width .. total_width + param.Label.len], param.Label);
            total_width += param.Label.len;
        }
        if (param == .String) {
            // @memcpy(buff[total .. total + param.String.len], param.String.str[0..param.String.len]);
            @memset(buff[total_width .. total_width + @max(1, param.String.len)], ' ');

            total_width += @max(1, param.String.len);
        }
    }

    var children = std.ArrayList(*ui.ui_node).initCapacity(zeng.global_allocator, 0) catch unreachable;
    defer children.deinit(zeng.global_allocator);

    var rolling_width: usize = 0;
    for (B.params) |*param| {
        if (param.* == .Label) {
            rolling_width += param.Label.len;
        }
        if (param.* == .String) {
            children.append(zeng.global_allocator, parameter_widget(&param.String, .{ .x = pos.x + @as(f32, @floatFromInt(rolling_width * 12)), .y = pos.y })) catch unreachable;
            rolling_width += @max(1, param.String.len);
        }
    }

    const start = global_num;
    for (B.children.items) |b_child| {
        const delta = global_num - start;
        const new_x = pos.x + 18;
        const new_y = pos.y + @as(f32, @floatFromInt((delta + 1) * 18));
        children.append(zeng.global_allocator, block_widget(b_child, .{ .x = new_x, .y = new_y })) catch unreachable;
    }

    global_num += 1;
    if (B.children.items.len > 0) {
        global_num += 1;
        children.append(zeng.global_allocator, n(
            .{ .color = B.color, .pos = .{ .x = pos.x, .y = pos.y }, .width = 18, .height = @floatFromInt((global_num - start) * 18), .width_size_mode = .fixed, .height_size_mode = .fixed, .pos_absolute = true },
            &.{},
        )) catch unreachable;
        children.append(zeng.global_allocator, n(
            .{ .color = B.color, .pos = .{ .x = pos.x, .y = pos.y + @as(f32, @floatFromInt((global_num - start - 1) * 18)) }, .width = @floatFromInt(total_width * 12), .height = 18, .width_size_mode = .fixed, .height_size_mode = .fixed, .pos_absolute = true },
            &.{},
        )) catch unreachable;
    }
    children.append(zeng.global_allocator, n(
        .{ .color = .{ .r = 0, .g = 0, .b = 0, .a = 0 }, .pos = .{ .x = pos.x, .y = pos.y }, .width = 18, .height = 18, .width_size_mode = .fixed, .height_size_mode = .fixed, .pos_absolute = true, .text = buff[0..total_width] },
        &.{},
    )) catch unreachable;
    return n(.{
        .width = @floatFromInt(total_width * 12),
        .height = 18,
        .color = B.color,
        .width_size_mode = .fixed,
        .height_size_mode = .fixed,
        .pos = pos,
        .pos_absolute = true,
    }, children.items);
}
pub fn parameter_widget(str_dyn: *cb_parameter.StringDynamic, pos: vec2) *ui.ui_node {
    return n(.{
        .pos = pos,
        .width = @as(f32, @floatFromInt(@max(1, str_dyn.len) * 12)),
        .width_size_mode = .fixed,
        .height = 18,
        .height_size_mode = .fixed,
        .color = .{ .r = 0.5, .g = 0.5, .b = 0.5 },
        .radius = 5,
        .text = str_dyn.str[0..str_dyn.len],
        .data_ptr = str_dyn,
        .pos_absolute = true,
    }, &.{});
}
pub const graph_render = struct {
    pub const edge = struct {
        from: usize,
        to: usize,
    };
    pub const render_info = struct {
        nodes: []vec2,
        edges: []graph_render.edge,
        map: std.AutoHashMap(*const ui.ui_node, usize),
    };
};
pub fn visualize_graph(allocator: std.mem.Allocator, start: *ui.ui_node) graph_render.render_info {
    var visited = std.AutoHashMap(*const ui.ui_node, void).init(allocator);
    defer visited.deinit();

    var ptr_to_idx = std.AutoHashMap(*const ui.ui_node, usize).init(allocator);
    defer ptr_to_idx.deinit();

    var nodes = std.ArrayList(vec2).initCapacity(allocator, 0) catch unreachable;
    // defer nodes.deinit(allocator);

    var edges = std.ArrayList(graph_render.edge).initCapacity(allocator, 0) catch unreachable;
    // defer edges.deinit(allocator);

    var stack = std.ArrayList(*const ui.ui_node).initCapacity(allocator, 0) catch unreachable;
    defer stack.deinit(allocator);

    stack.append(allocator, start) catch unreachable;

    while (stack.pop()) |curr| {
        if (visited.contains(curr)) continue;
        visited.put(curr, {}) catch unreachable;
        ptr_to_idx.put(curr, nodes.items.len) catch unreachable;

        nodes.append(allocator, undefined) catch unreachable;

        for ((curr.children orelse continue).items) |nbr| {
            stack.append(allocator, nbr) catch unreachable;
        }
    }
    stack.clearRetainingCapacity();
    stack.append(allocator, start) catch unreachable;
    visited.clearRetainingCapacity();
    nodes.items[ptr_to_idx.get(start).?] = vec2{ .x = 0, .y = 0 };
    while (stack.pop()) |curr| {
        if (visited.contains(curr)) continue;
        visited.put(curr, {}) catch unreachable;

        const curr_pos = nodes.items[ptr_to_idx.get(curr).?];
        var angle: f32 = 0;
        for ((curr.children orelse continue).items) |nbr| {
            defer angle += 0.5;
            nodes.items[ptr_to_idx.get(nbr).?] = curr_pos.add(vec2{ .x = 50 * @cos(angle), .y = 50 * @sin(angle) });
            edges.append(allocator, graph_render.edge{ .from = ptr_to_idx.get(curr).?, .to = ptr_to_idx.get(nbr).? }) catch unreachable;
            stack.append(allocator, nbr) catch unreachable;
        }
    }

    return .{ .nodes = nodes.items, .edges = edges.items, .map = ptr_to_idx };
}

pub const hot_reloader = struct {
    const DLL_LOCATION = "zig-out/bin/hot_reload_copy.dll";

    dll_hinstance: ?[*c]c.struct_HINSTANCE__ = null,
    last_dll_write_time: i128 = undefined,

    pub fn load_api(this: *@This()) ?hot_reload.hot_reload_procedures {
        const game_lib = zeng.c.LoadLibraryA(DLL_LOCATION);
        this.dll_hinstance = game_lib;
        if (game_lib == null) return null;
        const ptr = c.GetProcAddress(game_lib, "get_game_api");
        const dynamic_get_game_api: *const @TypeOf(hot_reload.get_game_api) = @ptrCast(ptr orelse return null);
        std.debug.print("script was hot reloaded!\n", .{});
        return dynamic_get_game_api().*;
    }
    pub fn hot_reload_api(this: *@This()) !hot_reload.hot_reload_procedures {
        var src_dir = std.fs.cwd().openDir(".", .{}) catch unreachable;
        defer src_dir.close();

        if (this.dll_hinstance != null) _ = zeng.c.FreeLibrary(this.dll_hinstance.?);

        try std.fs.Dir.copyFile(src_dir, "zig-out/bin/hot_reload.dll", src_dir, "zig-out/bin/hot_reload_copy.dll", .{});
        return this.load_api() orelse unreachable;
    }
    pub fn store_last_dll_write_time(this: *@This(), path: []const u8) void {
        this.last_dll_write_time = _get_last_dll_write_time(path);
    }
    pub fn _get_last_dll_write_time(path: []const u8) i128 {
        var file = std.fs.cwd().openFile(path, .{}) catch unreachable;
        defer file.close();

        const info = file.stat() catch unreachable;
        return info.mtime;
    }
    pub fn insert_text_into_file(path: []const u8, offset: usize, insert: []const u8, gpa: std.mem.Allocator) !void {

        // Open for read/write
        var file = try std.fs.cwd().openFile(path, .{ .mode = .read_write });
        defer file.close();

        // Read existing file size
        const stat = try file.stat();
        if (offset > stat.size) return error.OffsetOutOfRange;

        // Read tail of file into memory
        const tail_len = stat.size - offset;
        const tail = try gpa.alloc(u8, tail_len);
        defer gpa.free(tail);

        try file.seekTo(offset);
        _ = try file.readAll(tail);

        // Write insertion
        try file.seekTo(offset);
        try file.writeAll(insert);

        // Write original tail
        try file.writeAll(tail);
    }
    pub fn compile_dll(allocator: std.mem.Allocator) !void {
        var child = std.process.Child.init(&.{ "zig", "build", "hot" }, allocator);
        child.stdin_behavior = .Ignore;
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        try child.spawn();

        const stdout = try child.stdout.?.readToEndAlloc(allocator, 16 * 1024);
        defer allocator.free(stdout);

        const stderr = try child.stderr.?.readToEndAlloc(allocator, 16 * 1024);
        defer allocator.free(stderr);

        const term = try child.wait();
        std.debug.print("{s}\n{s}\n", .{ stdout, stderr });

        if (term != .Exited) {
            return error.CompilationFailed;
        }
    }
    pub fn create_script_zig_file(B: *cb_code_block, allocator: std.mem.Allocator) void {
        const dir = std.fs.cwd().openDir("dynamic", .{}) catch unreachable;
        std.fs.Dir.copyFile(dir, "script_master.zig", dir, "script_copy.zig", .{}) catch unreachable;

        var buff: [4000]u8 = undefined;
        var buff_len: usize = 0;
        B.print_code.?(B, buff[0..], &buff_len);

        insert_text_into_file("dynamic/script_copy.zig", 0, buff[0..buff_len], allocator) catch unreachable;
        compile_dll(allocator) catch unreachable;
    }

    pub fn try_new_api(this: *@This(), game_api: *hot_reload.hot_reload_procedures) void {
        const new_write_time = _get_last_dll_write_time("zig-out/bin/hot_reload.dll");
        if (new_write_time != this.last_dll_write_time) blk: {
            game_api.* = this.hot_reload_api() catch |err| {
                std.debug.print("error: {}\n", .{err});
                break :blk;
            };
            this.last_dll_write_time = new_write_time;
        }
    }
};

const cb_parameter = union(enum) {
    pub const StringDynamic = struct { str: []u8, len: usize };
    Label: []const u8,
    String: StringDynamic,
    code_block: *cb_code_block,
};
const cb_block_shape = enum {
    start,
    instruction,
};
const cb_code_block = struct {
    params: []cb_parameter,
    __params_rects: []cb_rect,
    children: std.ArrayList(*cb_code_block),
    shape: cb_block_shape,
    __height: f32 = 0,
    __rect: cb_rect = cb_rect{},

    print_code: ?*const fn (*cb_code_block, []u8, *usize) void,
    color: zeng.render.color,
};
const cb_value = union(enum) {
    player: *zeng.Player.player,
    int: *usize,
    float: *f32,
};
const cb_script_context = struct {
    map: std.StringHashMap(cb_value),
    arena: std.heap.ArenaAllocator,
    arena_allocator: std.mem.Allocator,
    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.arena = std.heap.ArenaAllocator.init(allocator);
        this.arena_allocator = this.arena.allocator();
        this.map = std.StringHashMap(cb_value).init(allocator);
    }
    pub fn deinit(this: *@This()) void {
        this.arena.deinit();
    }
    pub fn add(this: *@This(), str: []const u8, v: cb_value) void {
        this.map.put(str, v) catch unreachable;
    }
    pub fn get(this: *@This(), str: []const u8) cb_value {
        return this.map.get(str).?;
    }
    pub fn reset(this: *@This()) void {
        _ = this.arena.reset(.free_all);
        this.map.clearRetainingCapacity();
    }
};
const cb_rect = struct {
    pos: vec2 = vec2.ZERO,
    size: vec2 = vec2.ZERO,
};

pub fn CODEBLOCKPRINT_event(_B: *const cb_code_block, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..],
        \\export fn on_button_pressed(res: *zeng.resources_t) callconv(.c) void {{
        \\  const _player = get_item(res, zeng.main_player_res);
        \\  const world = get_item(res, zeng.ecs.world);
        \\  const player = world.get(_player.id, zeng.Player.player).?;
    , .{}) catch unreachable;
    buffer_len.* += new.len;

    for (_B.children.items) |C| {
        (C.print_code orelse continue)(C, buffer, buffer_len);
    }

    const new2 = std.fmt.bufPrint(buffer[buffer_len.*..], "}}\n", .{}) catch unreachable;
    buffer_len.* += new2.len;
}
pub fn CODEBLOCKPRINT_if(_B: *const cb_code_block, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "if ({s}) {{", .{_B.params[1].String.str[0.._B.params[1].String.len]}) catch unreachable;
    buffer_len.* += new.len;

    for (_B.children.items) |C| {
        (C.print_code orelse continue)(C, buffer, buffer_len);
    }

    const new2 = std.fmt.bufPrint(buffer[buffer_len.*..], "}}\n", .{}) catch unreachable;
    buffer_len.* += new2.len;
}
pub fn CODEBLOCKPRINT_blank(_B: *const cb_code_block, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "{s}", .{_B.params[0].String.str[0.._B.params[0].String.len]}) catch unreachable;
    buffer_len.* += new.len;
}
pub fn CODEBLOCKPRINT_jump(_B: *const cb_code_block, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "player.velocity.y += ", .{}) catch unreachable;
    buffer_len.* += new.len;

    const new2 = std.fmt.bufPrint(buffer[buffer_len.*..], "{s};\n", .{_B.params[1].String.str[0.._B.params[1].String.len]}) catch unreachable;
    buffer_len.* += new2.len;
}
pub fn micro_mouse_over(r: cb_rect, m: vec2) bool {
    return (m.x > r.pos.x and m.x < r.pos.x + r.size.x and m.y > r.pos.y and m.y < r.pos.y + r.size.y);
}
pub fn get_mouseover(B: *cb_code_block, parent: ?*cb_code_block, index: ?usize, mouse_pos: zeng.vec2) struct { ?*cb_code_block, ?usize, ?*cb_code_block, ?*cb_parameter } {
    if (micro_mouse_over(B.__rect, mouse_pos)) {
        for (B.params, B.__params_rects) |*p, pr| {
            if (micro_mouse_over(pr, mouse_pos)) {
                return .{ parent, index, B, p };
            }
        }
        return .{ parent, index, B, null };
    }
    for (B.children.items, 0..) |child, i| {
        const res_p, const res_c, const res_C, _ = get_mouseover(child, B, i, mouse_pos);
        if (res_C != null) {
            return .{ res_p, res_c, res_C, null };
        }
    }

    return .{ null, null, null, null };
}
pub fn CB(allocator: std.mem.Allocator, params: []const cb_parameter, color: zeng.render.color, children: []const *cb_code_block, shape: cb_block_shape, print_fn: ?*const @TypeOf(CODEBLOCKPRINT_event)) *cb_code_block {
    const ptr = allocator.create(cb_code_block) catch unreachable;
    const _params = allocator.alloc(cb_parameter, params.len) catch unreachable;
    for (params, _params) |param, *_param| {
        _param.* = param;
    }
    ptr.params = _params;

    ptr.children = std.ArrayList(*cb_code_block).initCapacity(allocator, 0) catch unreachable;
    for (children) |child| {
        ptr.children.append(allocator, child) catch unreachable;
    }

    ptr.shape = shape;
    ptr.print_code = print_fn;
    ptr.color = color;
    ptr.__params_rects = allocator.alloc(cb_rect, params.len) catch unreachable;
    for (ptr.__params_rects) |*v| {
        v.* = cb_rect{};
    }

    return ptr;
}
pub fn remove_code_block_ptr(list: *std.ArrayList(*cb_code_block), cb: *cb_code_block) void {
    for (list.items, 0..) |curr, i| {
        if (curr == cb) {
            _ = list.swapRemove(i);
            return;
        }
    }
}

pub fn print_entity_hierarchy(e_id: ecs.entity_id, world: *ecs.world, tabs: usize) void {
    var buffer: [32]ecs.entity_id = undefined;
    var extra_children = std.ArrayList(ecs.entity_id).initBuffer(buffer[0..]);
    for (0..tabs) |_| {
        std.debug.print(" - ", .{});
    }
    std.debug.print("#{}: ", .{e_id});
    if (world.get(e_id, zeng.name_component)) |name| {
        std.debug.print("\"{s}\" ", .{name.*});
    }
    if (world.get(e_id, zeng.skinned_mesh)) |sm| {
        std.debug.print("skinned_mesh ", .{});
        extra_children.appendAssumeCapacity(sm.skeleton);
    }
    if (world.get(e_id, zeng.mesh)) |_| {
        std.debug.print("static_mesh ", .{});
    }
    if (world.get(e_id, zeng.world_matrix)) |_| {
        std.debug.print("world_matrix ", .{});
    }
    if (world.get(e_id, zeng.local_matrix)) |_| {
        std.debug.print("local_matrix ", .{});
    }
    if (world.get(e_id, zeng.skeleton)) |_| {
        std.debug.print("skeleton ", .{});
    }
    std.debug.print("\n", .{});

    for (extra_children.items) |i| {
        print_entity_hierarchy(i, world, tabs + 1);
    }
    const children = world.get(e_id, zeng.children) orelse return;
    for (children.items) |i| {
        print_entity_hierarchy(i, world, tabs + 1);
    }
}

pub fn mouse_position_to_vec3s(main_camera_camera: zeng.camera, main_camera_matrix: zeng.world_matrix, mouse_state: zeng.vec2, graphics: zeng.graphics_t) struct { zeng.vec3, zeng.vec3 } {
    const inverted_pv_matrix = zeng.mat_invert(zeng.mat_mult(main_camera_camera.projection_matrix, zeng.mat_invert(main_camera_matrix)));

    const ndc_x: f32 = mouse_state.x / @as(f32, @floatFromInt(graphics.width)) * 2 - 1;
    const ndc_y: f32 = mouse_state.y / @as(f32, @floatFromInt(graphics.height)) * 2 - 1;

    const _near = zeng.mat_mult_vec4(inverted_pv_matrix, .{ .x = ndc_x, .y = -ndc_y, .z = -1, .w = 1.0 });
    const near = _near.div(_near.w).to_vec3();
    const _far = zeng.mat_mult_vec4(inverted_pv_matrix, .{ .x = ndc_x, .y = -ndc_y, .z = 1, .w = 1.0 });
    const far = _far.div(_far.w).to_vec3();

    return .{ near, far };
}

pub const sprite3D_mesh_res = struct {
    mesh: zeng.mesh,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    ui.global_node_allocator = arena_allocator;

    zeng.global_allocator = allocator;
    zeng.key_press_messages = std.ArrayList(u8).initCapacity(allocator, 0) catch unreachable;
    defer zeng.key_press_messages.deinit(allocator);

    var graphics: zeng.graphics_t = undefined;
    graphics.init();
    var res: zeng.resources_t = undefined;
    res.init(arena_allocator, &graphics);
    var box_drawer: ui.box_drawer_t = undefined;
    box_drawer.init(arena_allocator);
    ui.ui_id_map.init(arena_allocator);
    var asset_reg = zeng.asset_registry{ .map = std.StringHashMap(*anyopaque).init(allocator) };
    defer asset_reg.map.deinit();
    var world: ecs.world = ecs.world.init(allocator);
    defer world.deinit() catch unreachable;
    var fet: zeng.resource_fetcher = .{ .world = &world, .res = &res, .allocator = arena_allocator };
    var main_hot_reloader = hot_reloader{};

    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});
    zeng.global_world_ptr = &world;

    // CLI connection mode
    var is_server: bool = true;
    {
        std.debug.print("\nselect network mode:\n", .{});
        var stdin = std.fs.File.stdin().readerStreaming(&.{});
        var buff: [1024]u8 = undefined;
        _ = stdin.read(&buff) catch unreachable;
        if (std.mem.eql(u8, buff[0..1], "s")) {
            std.debug.print("using server mode...\n", .{});
        } else if (std.mem.eql(u8, buff[0..1], "c")) {
            std.debug.print("using client mode...\n", .{});
            is_server = false;
        }
    }

    // UDP multiplayer setup (the system simulates network latency and packet loss)
    const main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, is_server) catch unreachable;
    defer zeng.net.undo_setup(main_socket);
    const server_address = net.peer_info_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };

    // engine-wide resources
    const triangle_vao, const triangle_vbo = zeng.loader.create_triangle_mesh();
    const cube_vao, const cube_len = zeng.loader.create_cube_mesh_with_normals();
    const sky_shader = zeng.loader.load_shader(allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const rect_shader = zeng.loader.load_shader(allocator, "assets/shaders/protorect_vertex.shader", "assets/shaders/protorect_fragment.shader");
    const static_shader = zeng.loader.load_shader(allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.loader.load_shader(allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.loader.load_shader(allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.loader.load_texture("assets/images/uv_checker.png", true, false, false);
    const white_tex = zeng.loader.load_texture("assets/images/white.png", true, false, false);
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .parameter_map = blk: {
        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
        _value.put("albedo_texture", .{ .texture = uv_checker_tex }) catch unreachable;
        _value.put("albedo", .{ .float_3 = zeng.vec3.ONE }) catch unreachable;
        _value.put("metallic", .{ .float_1 = 0.0 }) catch unreachable;
        _value.put("roughness", .{ .float_1 = 0.5 }) catch unreachable;
        break :blk _value;
    } }, .vao_gpu = cube_vao };
    var gun_shot = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/gun_shot.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/gun_shot.wav", &gun_shot);
    var bell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/bell.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/bell.wav", &bell);
    var ahem = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/ahem.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/ahem.wav", &ahem);
    var fireball_sound = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/fireball.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/fireball.wav", &fireball_sound);
    var spell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/spell.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/spell.wav", &spell);
    var damage_sound = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/damage.wav", arena_allocator)) catch unreachable;
    asset_reg.put("sounds/damage.wav", &damage_sound);

    var top_children = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    defer top_children.deinit(arena_allocator);

    zeng.loader.generate_colliders_on_import = false;
    const pistol_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf", "pistol", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(arena_allocator, pistol_entity) catch unreachable;
    zeng.loader.generate_colliders_on_import = true;

    const map_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf", "dungeon", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    top_children.append(arena_allocator, map_entity) catch unreachable;

    const card_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf", "card", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    top_children.append(arena_allocator, card_entity) catch unreachable;

    const cube_entity = world.spawn(.{ cube_mesh, zeng.mat_tran(zeng.mat_identity, .{ .x = -7.0, .y = 2.0 }), zeng.floater_component{} });
    if (!is_server) world.add(zeng.snapshot_interpolator{ .buffer = undefined }, cube_entity);

    const test_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf/people", "KingShiny", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(arena_allocator, test_entity) catch unreachable;
    const test_random_skinned_mesh = zeng.find_component_of_type(&world, test_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
    const test_skeleton_entity = world.get(test_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, test_skeleton_entity);
    world.get(test_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(test_entity, zeng.world_matrix).?.*, .{ .x = 5 });

    const player_entity = zeng.Player.construct_main_server_player(&asset_reg, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);
    world.add(dungeon_deck.card_caster{ .fire_ball_prototype = dungeon_deck.fire_ball_component{ .velocity = undefined } }, player_entity);

    const ghost_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf", "ghost", skin_shader, static_shader, white_tex, arena_allocator);
    world.add(dungeon_deck.health_component{ .health = 100 }, ghost_entity);
    world.add(dungeon_deck.ghost_component{}, ghost_entity);
    top_children.append(allocator, ghost_entity) catch unreachable;

    var remote_player_entity: ecs.entity_id = undefined;
    var remote_player_skeleton_entity: ecs.entity_id = undefined;
    if (!is_server) {
        remote_player_entity = zeng.loader.auto_import(&asset_reg, &world, "assets/gltf", "static_test", skin_shader, static_shader, uv_checker_tex, arena_allocator);
        const remote_player_random_skinned_mesh = zeng.find_component_of_type(&world, remote_player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
        remote_player_skeleton_entity = world.get(remote_player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
        world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, remote_player_skeleton_entity);
        world.get(remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 10.0 });
        world.add(zeng.snapshot_interpolator{ .buffer = undefined }, remote_player_entity);
        top_children.append(arena_allocator, remote_player_entity) catch unreachable;
    }

    res.insert_ptr(&res);
    res.insert_ptr(&asset_reg);
    res.insert_ptr(&fet);
    var tracker = net.packet_ack_tracker_t{};
    res.insert_ptr(&tracker);
    res.insert(zeng.main_player_res{ .id = player_entity });
    const square_vao, const square_indices_length = zeng.loader.create_centered_square_mesh();
    res.insert(zeng.text_render_res{ .shader_program = zeng.loader.load_shader(allocator, "assets/shaders/text_vertex.shader", "assets/shaders/text_fragment.shader"), .texture = zeng.loader.load_texture("assets/images/sdf_font.png", false, false, false), .vao = box_drawer.vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands{ .random = res.get(std.Random.Xoshiro256).random(), .remote_messages_send_queue = undefined, .remote_messages_send_queue_len = 0, .allocator = allocator };
    defer commands.destroy();
    res.insert_ptr(&commands);
    res.insert(zeng.input_res{ .t_down_last_frame = false });
    res.insert_ptr(&graphics);
    res.insert_ptr(&world);
    res.insert(zeng.networking_res{ .main_socket = main_socket, .server_address = _server_address, .is_server = is_server });
    res.insert(zeng.rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });
    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, zeng.follow_component{ .target = player_entity, .anchor_point = zeng.mat_position(world.get(player_entity, zeng.world_matrix).?.*) } });
    zeng.global_camera_entity = main_camera;
    res.insert(zeng.debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = world.get(main_camera, zeng.camera).?.projection_matrix, .inv_camera_matrix = zeng.mat_invert(world.get(main_camera, zeng.world_matrix).?.*) });
    res.insert(zeng.main_camera_res{ .id = main_camera });
    world.get(player_entity, zeng.Player.player).?.camera = main_camera;
    zeng.window_resize_handler(graphics.width, graphics.height);
    var peer_map = std.AutoHashMap(net.peer_info_t, zeng.client_info).init(allocator);
    defer peer_map.deinit();
    var inverse_peer_map = std.AutoHashMap(ecs.entity_id, net.peer_info_t).init(allocator);
    defer inverse_peer_map.deinit();
    res.insert_ptr(&peer_map);
    res.insert_ptr(&inverse_peer_map);
    res.insert(zeng.shadow_map_res.init(allocator));
    res.insert(zeng.mouse_state_res{ .mouse_position = undefined, .mouse_pressed = undefined, .mouse_released = undefined });
    res.insert(dungeon_deck.playing_card_context{ .cube_mesh = cube_mesh });
    var entity_colliders: dungeon_deck.entity_collider_res = undefined;
    entity_colliders.init(allocator);
    defer entity_colliders.deinit();
    res.insert_ptr(&entity_colliders);
    entity_colliders.colliders.put(ghost_entity, .{ .data = undefined, .matrix = undefined, .support = phy.sphere, .tag = .support_based }) catch unreachable;
    var colliders = std.ArrayList(phy.convex_collider).initCapacity(allocator, 0) catch unreachable;
    defer colliders.deinit(allocator);
    res.insert_ptr(&colliders);
    var spatial_hash_grid = std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.convex_collider)).init(allocator);
    defer spatial_hash_grid.deinit();
    res.insert_ptr(&spatial_hash_grid);
    const fixed_rate: f64 = 60.0;
    const fixed_delta: f64 = 1.0 / fixed_rate;
    res.insert(zeng.time_res{ .delta_time = 0.006944, .dt = 0.006944, .fixed_delta_time = fixed_delta, .fixed_dt = @floatCast(fixed_delta) });
    res.get(zeng.time_res).fixed_dt = @floatCast(fixed_delta);
    res.insert(zeng.sprite3D_render_res{ .rect_mesh = zeng.mesh{ .indices_length = square_indices_length, .vao_gpu = square_vao, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = box_drawer.shader_program, .parameter_map = undefined } } });
    const my_font_info = ui.parse_font_descriptor("assets/fonts/mikado-medium-58b66e4a");
    const my_font_info2 = ui.parse_font_descriptor("assets/fonts/jetbrainsmono-medium-50b9ac8e");
    res.insert(sprite3D_mesh_res{ .mesh = zeng.loader.create_centered_square_mesh_formatted_for_sprite3D(.{ .shader_program = static_shader, .parameter_map = pblk: {
        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
        _value.put("albedo_texture", .{ .texture = zeng.loader.load_texture("assets/images/orb.png", true, false, true) }) catch unreachable;
        _value.put("albedo", .{ .float_3 = zeng.vec3.ONE }) catch unreachable;
        _value.put("metallic", .{ .float_1 = 0.0 }) catch unreachable;
        _value.put("roughness", .{ .float_1 = 0.5 }) catch unreachable;
        break :pblk _value;
    } }) });
    const sdf_text_rect_mesh = zeng.mesh{ .vao_gpu = square_vao, .indices_length = square_indices_length, .indices_type = zeng.gl.UNSIGNED_INT, .material = undefined };

    res.insert(msg([]const u8).init(allocator, false));
    res.insert(msg([3]zeng.vec3).init(allocator, false));
    res.insert(msg(rpc.player_spawn_message).init(allocator, true));
    res.insert(msg(rpc.state_correction).init(allocator, true));
    res.insert(msg(rpc.input_chunck).init(allocator, true));
    res.insert(msg(rpc.client_tick).init(allocator, true));
    res.insert(msg(rpc.server_tick_offset).init(allocator, true));
    res.insert(msg(rpc.missed_input).init(allocator, true));
    res.insert(msg(rpc.world_update).init(allocator, true));
    res.insert(msg(rpc.hitmarker).init(allocator, true));
    res.insert(msg(zeng.delete_event).init(allocator, false));

    { // add optimized static collision to the scene
        for (zeng.loader.global_colliders.?.items, zeng.loader.global_matrices.?.items) |_mesh, _matrix| {
            var curr_tri: usize = 0;
            while (curr_tri < _mesh.indices.len) {
                defer curr_tri += 3;
                const cool = arena_allocator.create(phy.mesh_triangle_data) catch unreachable;
                cool.positions = _mesh.positions;
                cool.indices = .{ _mesh.indices[curr_tri], _mesh.indices[curr_tri + 1], _mesh.indices[curr_tri + 2] };
                const collider = phy.convex_collider{ .matrix = _matrix, .support = phy.mesh_triangle, .tag = .support_based, .data = @ptrCast(cool) };
                colliders.append(allocator, collider) catch unreachable;
            }
        }
        phy.construct_spatial_hash_grid(colliders, &spatial_hash_grid, arena_allocator);
    }

    var accumulator: f64 = 0.0;
    var tick: isize = 0;

    var client_input_buffer: zeng.ring_buffer(rpc.input_message) = undefined;
    var synced_time: f64 = 0.0;
    var sync_clock_timescale: f64 = 1.0;
    var sim_timescale: f64 = 1.0;
    var buffer_time: f64 = 0.0;
    var buffer_velocity: f64 = 0.0;
    var buffer_cooldown: f64 = 0.0;
    var server_has_responded: bool = false;
    var exponential_rtt: f64 = 1.0;

    var draw_local_alignment: f64 = 0.0;
    var draw_time_alignment: f64 = 0.0;
    var draw_rtt: f64 = 0.0;
    var draw_resims: usize = 0;
    var draw_interpolated_tick_delta: f64 = 0.0;

    var render_info: ?graph_render.render_info = null;

    var game_api = main_hot_reloader.hot_reload_api() catch unreachable;
    main_hot_reloader.store_last_dll_write_time("zig-out/bin/hot_reload.dll");

    var key_pressed_fixed_update = false;
    var r_pressed_last_frame = false;
    const root_code_block = CB(arena_allocator, &.{cb_parameter{ .Label = "When button clicked (res) (player) (world)" }}, .{ .r = 0.8, .g = 0.2, .b = 0.1 }, &.{
        CB(arena_allocator, &.{ cb_parameter{ .Label = "Do super jump" }, cb_parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } } }, .{ .r = 0.1, .g = 0.3, .b = 0.8 }, &.{}, .instruction, CODEBLOCKPRINT_jump),
        CB(arena_allocator, &.{cb_parameter{ .Label = "Print 'HELLO WORLD!'" }}, .{ .r = 0.15, .g = 0.6, .b = 0.1 }, &.{}, .instruction, null),
        CB(arena_allocator, &.{ cb_parameter{ .Label = "If" }, cb_parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } } }, .RED, &.{}, .instruction, CODEBLOCKPRINT_if),
        CB(arena_allocator, &.{cb_parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } }}, .{ .r = 0.2, .g = 0.2, .b = 0.2 }, &.{}, .instruction, CODEBLOCKPRINT_blank),
    }, .start, CODEBLOCKPRINT_event);
    var dragging_code_block: ?*cb_code_block = null;
    var mouse_down_last_frame = false;
    var selected_parameter: ?*cb_parameter = null;

    var top_level_code_blocks = std.ArrayList(*cb_code_block).initCapacity(allocator, 0) catch unreachable;
    defer top_level_code_blocks.deinit(allocator);
    top_level_code_blocks.append(allocator, root_code_block) catch unreachable;
    var script_context: cb_script_context = undefined;
    script_context.init(allocator);
    defer script_context.deinit();

    if (!is_server) zeng.net.remote_event(&commands, &tracker, main_socket, server_address, rpc.player_spawn_message{}, .reliable);

    zeng.frame_timer_warmup();
    while (true) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);
        if (zeng.quit) break;
        commands.time += res.get(zeng.time_res).delta_time;
        zeng.net.recieve_net_messages(main_socket, &res, allocator, &tracker);

        main_hot_reloader.try_new_api(&game_api);

        const world_matrix_q = fet.fresh_query(.{zeng.world_matrix});
        const children_q = fet.fresh_query(.{zeng.children});
        const local_matrix_q = fet.fresh_query(.{zeng.local_matrix});

        const mouse_state = res.get(zeng.mouse_state_res);
        mouse_state.mouse_position = vec2{ .x = @floatFromInt(zeng.global_mouse_pos[0]), .y = @floatFromInt(zeng.global_mouse_pos[1]) };
        mouse_state.mouse_pressed = zeng.get_mouse_button(.left) and !mouse_down_last_frame;
        mouse_state.mouse_released = !zeng.get_mouse_button(.left) and mouse_down_last_frame;
        defer mouse_down_last_frame = zeng.get_mouse_button(.left);

        { // mouse locking/unlocking
            if (zeng.get_mouse_button(.left) and zeng.get_key(.k)) {
                zeng.lock_cursor_to_window(graphics.hwnd);
                zeng.hide_cursor();
            }
            if (zeng.get_key(.m)) {
                zeng.unlock_cursor();
                zeng.show_cursor();
            }
        }

        if (is_server) { // server communication
            const server_tick_offset_events = res.get(msg(rpc.server_tick_offset));
            for (server_tick_offset_events.items(), server_tick_offset_events.address_items()) |server_tick_offset_event, _| {
                const rtt = synced_time - server_tick_offset_event.client_time;
                draw_rtt = rtt;
                exponential_rtt = zeng.lerp(exponential_rtt, rtt, 0.08);

                const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
                draw_time_alignment = time_offset;

                if (@abs(time_offset) > 0.7) {
                    // std.debug.print("time jump\n", .{});
                    synced_time += time_offset;
                } else {
                    sync_clock_timescale = 1.0 + (time_offset / 50.0);
                }
                if (!server_has_responded) {
                    buffer_time = 0.4;
                }
                server_has_responded = true;
            }
            server_tick_offset_events.clear(allocator);
            const input_events = res.get(msg(rpc.input_chunck));
            for (input_events.items(), input_events.address_items()) |input_event, sockaddr| {
                const info = peer_map.getPtr(sockaddr) orelse break;
                for (input_event.arr) |_input_event| {
                    if (_input_event.tick >= tick) {
                        info.input_buffer.set(_input_event.tick, _input_event);
                    } // else discard late message
                }
            }
            input_events.clear(allocator);
            const player_join_events = res.get(msg(rpc.player_spawn_message));
            for (player_join_events.items(), player_join_events.address_items()) |_, sockaddr| {
                std.debug.print("player connected: \n", .{});

                const new_remote_player_entity = zeng.Player.create_player(&asset_reg, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator, .{ .net_id = zeng.get_new_netid(), .remote_peer = sockaddr });

                world.get(new_remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(new_remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 60.0 });
                world.get(new_remote_player_entity, zeng.Player.player).?.camera = world.spawn(.{zeng.mat_identity});

                peer_map.put(sockaddr, zeng.client_info{ .input_buffer = zeng.ring_buffer(rpc.input_message){ .arr = undefined }, .player = new_remote_player_entity }) catch unreachable;
                inverse_peer_map.put(new_remote_player_entity, sockaddr) catch unreachable;
            }
            player_join_events.clear(allocator);
            const client_tick_events = res.get(msg(rpc.client_tick));
            for (client_tick_events.items(), client_tick_events.address_items()) |client_tick_event, sockaddr| {
                // commands.remote_event(main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
                zeng.net.remote_event(&commands, &tracker, main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
            }
            client_tick_events.clear(allocator);
        }

        if (!is_server) { // client communication
            const hitmarker_events = res.get(msg(rpc.hitmarker));
            for (hitmarker_events.items(), hitmarker_events.address_items()) |_, _| {
                aud.play_sound(bell, .one_shot);
            }
            hitmarker_events.clear(allocator);
            const snap_events = res.get(msg(rpc.state_correction));
            for (snap_events.items(), snap_events.address_items()) |snap_event, _| {
                const P = world.get(player_entity, zeng.Player.player).?;
                const temp = P.animation_controller;
                const temp2 = P.camera;
                P.* = snap_event.state;
                P.animation_controller = temp;
                P.camera = temp2;
                world.get(player_entity, zeng.world_matrix).?.* = snap_event.world_matrix;

                var _tick = snap_event.tick + 1;
                while (_tick < tick) {
                    defer _tick += 1;

                    var im = client_input_buffer.get(_tick);
                    if (im.tick != _tick) {
                        // std.debug.print("missing buffered input for tick: {} {} {}\n", .{ im.tick, _tick, tick });
                        break;
                    }
                    zeng.Player.simulate_collision(world.get(player_entity, zeng.Player.player).?, world.get(player_entity, zeng.world_matrix).?, &spatial_hash_grid, res.get(msg([3]zeng.vec3)), res.get(zeng.debug_res));
                    zeng.Player.simulate_player(world.get(player_entity, zeng.Player.player).?, &im, world.get(player_entity, zeng.world_matrix).?, res.get(zeng.time_res));
                }
                draw_resims = @intCast(@max(tick - snap_event.tick, 0));

                zeng.sync_transforms_children(player_entity, world_matrix_q, children_q, local_matrix_q);
            }
            snap_events.clear(allocator);
            const missed_input_events = res.get(msg(rpc.missed_input));
            for (missed_input_events.items(), missed_input_events.address_items()) |_, _| {
                if (server_has_responded and buffer_cooldown <= 0.0) {
                    buffer_velocity = 0.04;
                    buffer_cooldown = 0.3;
                }
            }
            missed_input_events.clear(allocator);
            const world_update_events = res.get(msg(rpc.world_update));
            for (world_update_events.items(), world_update_events.address_items()) |world_update_event, _| {
                const SI = world.get(remote_player_entity, zeng.snapshot_interpolator).?;
                SI.buffer.set(world_update_event.tick, .{ .position = zeng.mat_position(world_update_event.server_player_matrix), .tick = world_update_event.tick });

                const SI2 = world.get(cube_entity, zeng.snapshot_interpolator).?;
                SI2.buffer.set(world_update_event.tick, .{ .position = world_update_event.cube_pos, .tick = world_update_event.tick });

                const target = @as(f64, @floatFromInt(world_update_event.tick - tick));
                draw_interpolated_tick_delta = target;
            }
            world_update_events.clear(allocator);
            if (!is_server) {
                const integer_tick_float: f32 = @floatFromInt(tick);
                const fractional_tick_float: f32 = @floatCast(accumulator * fixed_rate);
                const _tick_float = integer_tick_float + fractional_tick_float;
                const tick_float = zeng.lerp(@as(f32, @floatCast(synced_time * fixed_rate)), _tick_float, -1.0); // double check this is a good idea - it is technical scalable
                const start_A: isize = @intFromFloat(tick_float);
                const start_B: isize = @intFromFloat(tick_float + 1.0);

                const interp_q = fet.fresh_query(.{ zeng.snapshot_interpolator, zeng.world_matrix });
                var interp_it = interp_q.iterator();
                while (interp_it.next()) |curr| {
                    const SI, const M = curr;

                    var A: isize = start_A;
                    while (A > start_A - 50) {
                        if (SI.buffer.get(A).tick == A) break;
                        A -= 1;
                    }

                    var B: isize = start_B;
                    while (B < start_A + 50) {
                        if (SI.buffer.get(B).tick == B) break;
                        B += 1;
                    }

                    if (SI.buffer.get(A).tick == A and SI.buffer.get(B).tick == B) {
                        const float_A: f32 = @floatFromInt(A);
                        const float_B: f32 = @floatFromInt(B);

                        const t_value = zeng.inv_lerp(float_A, float_B, tick_float);

                        zeng.mat_position_set(M, SI.buffer.get(A).position.lerp(SI.buffer.get(B).position, t_value));
                    } else {
                        // std.debug.print("interp error\n", .{});
                    }
                }
            }
            if (server_has_responded) { // server-client clock sync algorithm
                buffer_cooldown -= res.get(zeng.time_res).delta_time;
                if (buffer_cooldown < -10.0) {
                    buffer_velocity = -0.005;
                } else if (buffer_cooldown < 0.0) {
                    buffer_velocity = 0.0;
                }
                // buffer_time += __resources.get(time_res).delta_time * buffer_velocity;
                buffer_time = (exponential_rtt * 0.5) * 1.6;

                const desired_time = synced_time + buffer_time;
                const my_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator;
                const offset = desired_time - my_time;
                draw_local_alignment = offset;
                if (@abs(offset) > 0.5) {
                    tick += @intFromFloat(offset * fixed_rate);
                } else {
                    sim_timescale = zeng.lerp(sim_timescale, 1.0 + offset / 5.0, 0.2);
                }
            }
        }

        fet.run_system(dungeon_deck.cast_system);

        synced_time += res.get(zeng.time_res).delta_time * sync_clock_timescale;
        accumulator += res.get(zeng.time_res).delta_time * sim_timescale;
        while (accumulator >= fixed_delta) {
            defer tick += 1;
            accumulator -= fixed_delta;

            if (is_server) { // APPLY INPUTS TO REMOTE PLAYERS
                const TARGET_TICK_EARLINESS = 2;
                var remote_players_iterator = peer_map.iterator();
                while (remote_players_iterator.next()) |remote_player_info| {
                    const current_frame_input = remote_player_info.value_ptr.input_buffer.get(tick);
                    if (current_frame_input.tick == tick) {
                        const ent = remote_player_info.value_ptr.player;
                        const remote_player_input = world.get(ent, rpc.input_message).?;
                        remote_player_input.* = current_frame_input;
                    } else {
                        // std.debug.print("missed input {}\n", .{tick});
                        aud.play_sound(ahem, .one_shot);
                    }
                    if (remote_player_info.value_ptr.input_buffer.get(tick + TARGET_TICK_EARLINESS).tick != tick + TARGET_TICK_EARLINESS) {
                        zeng.net.remote_event(&commands, &tracker, main_socket, remote_player_info.key_ptr.*, rpc.missed_input{}, .unreliable);
                    }
                }
            }
            var local_player_shoot = false;
            if (zeng.get_mouse_button(.left) and !key_pressed_fixed_update) {
                local_player_shoot = true;
                key_pressed_fixed_update = true;
            }
            if (!zeng.get_mouse_button(.left)) key_pressed_fixed_update = false;

            const local_player_input = world.get(player_entity, rpc.input_message).?;
            local_player_input.* = rpc.input_message{ .tick = tick, .jump = zeng.input_implement.default_jump(), .move_vect = zeng.input_implement.default_move_fn(), .rot_x = local_player_input.rot_x, .rot_y = local_player_input.rot_y, .shoot = local_player_shoot };
            if (!is_server) {
                client_input_buffer.set(tick, world.get(player_entity, rpc.input_message).?.*);
                var snd: rpc.input_chunck = undefined;
                var curr: usize = 0;
                while (curr < snd.arr.len) {
                    defer curr += 1;
                    snd.arr[curr] = client_input_buffer.get(tick - @as(isize, @intCast(curr)));
                }
                zeng.net.remote_event(&commands, &tracker, main_socket, server_address, snd, .unreliable);
            }
            if (is_server) {
                const floater_q = fet.fresh_query(.{ zeng.floater_component, zeng.world_matrix });
                var floater_it = floater_q.iterator();
                while (floater_it.next()) |curr| {
                    _, const M = curr;
                    zeng.mat_position_set(M, .{ .x = -7.0, .y = 2.0, .z = @mod(@as(f32, @floatFromInt(tick)) * 0.01, 1.0) * 20.0 - 5.0 });
                }
            }

            { // camera FOV modulation for zooming
                const cam = world.get(main_camera, zeng.camera).?;
                if (zeng.get_mouse_button(.right)) {
                    cam.fov = zeng.lerp(cam.fov, 1.0, 20 * res.get(zeng.time_res).fixed_dt);
                } else {
                    cam.fov = zeng.lerp(cam.fov, 1.5, 20 * res.get(zeng.time_res).fixed_dt);
                }
                cam.projection_matrix = zeng.mat_perspective_projection(cam.fov, @as(f32, @floatFromInt(graphics.width)) / @as(f32, @floatFromInt(graphics.height)), 0.01, 1000.0);
            }

            fet.run_system(camera_fly_system);
            fet.run_system(zeng.Player.player_collision_system);
            fet.run_system(zeng.Player.player_simulate_and_animate_system);
            fet.run_system(dungeon_deck.ghost_system);
            fet.run_system(dungeon_deck.fire_ball_system);
            fet.run_system(dungeon_deck.sync_entity_colliders_system);
            fet.run_system(dungeon_deck.health_die_system);
            animate_skeleton(test_skeleton_entity, "assets/gltf/people/KingShiny.gltf/animations/Idle", @floatCast(res.get(zeng.time_res).delta_time), &asset_reg, &world);
            fet.run_system(frame_interpolator_tick_store_system);

            if (is_server) { // periodic client/server communication
                var client_it = peer_map.iterator();
                while (client_it.next()) |thing| {
                    const P = world.get(thing.value_ptr.player, zeng.Player.player).?.*;
                    const M = world.get(thing.value_ptr.player, zeng.world_matrix).?.*;
                    if (@as(usize, @intCast(tick)) % 4 == 0) zeng.net.remote_event(&commands, &tracker, main_socket, thing.key_ptr.*, rpc.state_correction{ .tick = tick, .state = P, .world_matrix = M }, .unreliable);
                }

                client_it = peer_map.iterator();
                while (client_it.next()) |thing| {
                    if (@as(usize, @intCast(tick)) % 1 == 0) {
                        zeng.net.remote_event(&commands, &tracker, main_socket, thing.key_ptr.*, rpc.world_update{ .cube_pos = zeng.mat_position(world.get(cube_entity, zeng.world_matrix).?.*), .server_player_matrix = world.get(player_entity, zeng.world_matrix).?.*, .tick = tick }, .unreliable);
                    }
                }
            } else {
                if (@as(usize, @intCast(tick)) % 30 == 0) {
                    zeng.net.remote_event(&commands, &tracker, main_socket, server_address, rpc.client_tick{ .time = synced_time }, .unreliable);
                }
            }
        }

        for (top_children.items) |ch| { // sync all transforms TODO: make a "root" entity and use a children component to achieve this concept
            if (!world.is_alive(ch)) continue;
            zeng.sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
        }
        { // interpolate player and set camera to player; use players input component to apply rotation to the camera
            const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
            const player_interpolator = world.get(player_entity, zeng.frame_interpolator).?;
            const interpolated_matrix = player_interpolator.get_matrix(@as(f32, @floatCast(accumulator / fixed_delta)));
            const camera_target_position = zeng.mat_position(interpolated_matrix);
            const local_player_input = world.get(player_entity, rpc.input_message).?;
            zeng.matrix_use_rotations(cam_matrix, local_player_input.rot_x, local_player_input.rot_y);
            zeng.mat_position_set(cam_matrix, camera_target_position.add(zeng.vec3{ .y = 0.8 })); //.add(zeng.mat_forward(cam_matrix.*).mult(2.0)));

            for (world.get(player_entity, zeng.children).?.items) |child_item| {
                zeng.sync_transforms_recursive(interpolated_matrix, child_item, world_matrix_q, children_q, local_matrix_q);
            }

            zeng.mat_position_set(&(res.get(zeng.shadow_map_res).camera_matrix), zeng.mat_position(interpolated_matrix).add(.{ .y = 15, .z = 8 }));

            const card_position: zeng.vec3 = .{ .z = -0.2, .y = -0.1 };
            const mat = zeng.mat_mult(cam_matrix.*, zeng.mat_tran(zeng.mat_scal(zeng.mat_identity, zeng.vec3.ONE.mult(0.015)), card_position));

            world.get(card_entity, zeng.world_matrix).?.* = mat;

            zeng.sync_transforms_children(card_entity, world_matrix_q, children_q, local_matrix_q);
        }
        if (!is_server) { // animation host player on client
            const skel = world.get(remote_player_skeleton_entity, zeng.skeleton).?;
            const anim = world.get(remote_player_skeleton_entity, zeng.animation_component).?;
            const animation = asset_reg.get("assets/gltf/static_test.gltf/animations/idle", zeng.loader.animation);

            anim.time = 0.0;

            const rotations = allocator.alloc(zeng.quat, skel.bone_parent_indices.len) catch unreachable;
            const translations = allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
            const scales = allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
            zeng.get_animation_pose_with_weight(animation, anim.time, .{ rotations, translations, scales }, 1.0);
            zeng.apply_pose_to_skeleton(skel, .{ rotations, translations, scales });
            allocator.free(rotations);
            allocator.free(translations);
            allocator.free(scales);
        }
        { // rendering
            res.get(zeng.shadow_map_res).shadow_pass(fet.fresh_query(.{ zeng.world_matrix, zeng.mesh }));
            zeng.gl.viewport(0, 0, graphics.width, graphics.height);
            const camera_camera_ptr = world.get(main_camera, zeng.camera).?;
            const camera_matrix_ptr = world.get(main_camera, zeng.world_matrix).?;
            zeng.render.draw_sky(sky_shader, square_vao, square_indices_length, camera_matrix_ptr.*, camera_camera_ptr);
            fet.run_system(draw_mesh_system);
            fet.run_system(sprite3D_render_system);
        }
        { // debug triangle at mouse position in 3D space
            // const main_camera_camera = world.get(main_camera, zeng.camera).?;
            // const main_camera_matrix = world.get(main_camera, zeng.world_matrix).?;

            // resources.get(zeng.debug_res).inv_camera_matrix = zeng.mat_invert(main_camera_matrix.*);
            // resources.get(zeng.debug_res).projection_matrix = main_camera_camera.projection_matrix;

            // const near, const far = mouse_position_to_vec3s(main_camera_camera.*, main_camera_matrix.*, mouse_state.mouse_position, graphics);
            // const ray = far.sub(near).normalized();

            // const P = zeng.mat_position(main_camera_matrix.*).add(ray);
            // zeng.render.debug_draw_triangle(.{ P.add(.{ .y = 0.2 }), P.add(.{ .x = 0.2 }), P }, resources.get(zeng.debug_res).*);
        }
        { // makeshift UI debug HUD
            var buffer: [64]u8 = undefined;
            zeng.render.draw_sdf_font_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / res.get(zeng.time_res).delta_time}) catch unreachable, if (zeng.get_key(.p)) my_font_info else my_font_info2, sdf_text_rect_mesh, 0, 0, 0.35, graphics);

            if (!is_server) {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "synctime: {d:.6}", .{synced_time}) catch unreachable, res.get(zeng.text_render_res), 0, 80, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffer: {d:.6}", .{buffer_time}) catch unreachable, res.get(zeng.text_render_res), 0, 120, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "timescale: {d:.6}", .{sim_timescale}) catch unreachable, res.get(zeng.text_render_res), 0, 160, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffvel: {d:.6}", .{buffer_velocity}) catch unreachable, res.get(zeng.text_render_res), 0, 200, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "    rtt: {d:.6}", .{draw_rtt}) catch unreachable, res.get(zeng.text_render_res), 0, 240, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "exp rtt: {d:.6}", .{exponential_rtt}) catch unreachable, res.get(zeng.text_render_res), 0, 280, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "resims: {}", .{draw_resims}) catch unreachable, res.get(zeng.text_render_res), 0, 320, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "interp: {d:.6}", .{draw_interpolated_tick_delta}) catch unreachable, res.get(zeng.text_render_res), 0, 360, graphics);

                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), @as(f32, @floatCast(draw_time_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);

                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);
            } else {
                zeng.render.draw_sdf_font_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accumulator}) catch unreachable, if (zeng.get_key(.p)) my_font_info else my_font_info2, sdf_text_rect_mesh, 0, 40, 0.35, graphics);
            }

            var ticker_display_time: f64 = undefined;
            if (is_server) {
                ticker_display_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator;
            } else {
                ticker_display_time = synced_time;
            }
            // zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), @floatCast(@mod(ticker_display_time * 100.0, 100.0) - 400), 400, 10, 10, zeng.render.color.YELLOW);
            box_drawer.draw_img(graphics, @floatCast(@mod(ticker_display_time * 150.0, 150.0)), 0, 15, 15, .YELLOW, 0, 0);
            zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 0, 6, 6, zeng.render.color.BLACK);
            zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 0, 4, 4, zeng.render.color.WHITE);
        }
        if (false) { // block ui part 1
            zeng.set_cursor(.arrow);

            const page = page_widget(uv_checker_tex, uv_checker_tex, @floatFromInt(graphics.width), @floatFromInt(graphics.height), 0.3);
            ui.ui_layout(.{ .x = 0, .y = 0 }, page);
            // const nptr = recursive_mouse_over(page, &mouse_state);
            // zeng.set_cursor(.arrow);
            // if (nptr) |node_ptr| {
            //     if (node_ptr.color.?.a > 0.01) zeng.set_cursor(.pointer);
            // }
            const button = ui.ui_id_map.get("play_button");
            if (ui.mouse_over(button, mouse_state.mouse_position.x, mouse_state.mouse_position.y)) {
                zeng.set_cursor(.pointer);
                button.color = .WHITE;
                if (mouse_state.mouse_pressed) {
                    game_api.on_button_pressed(&res);
                }
            }
            ui.ui_draw(&box_drawer, graphics, page, res.get(zeng.text_render_res));

            if (render_info == null) {
                render_info = visualize_graph(arena_allocator, page);
            }

            for (render_info.?.nodes) |*node_pos| {
                var sum = vec2.ZERO;
                for (render_info.?.nodes) |*_node| {
                    if (node_pos == _node) continue;
                    sum = sum.add(_node.sub(node_pos.*).normalized());
                }
                sum = sum.normalized().mult(-0.5);
                node_pos.* = node_pos.add(sum);
            }
            for (render_info.?.edges) |*edge| {
                const pos_a = &render_info.?.nodes[edge.from];
                const pos_b = &render_info.?.nodes[edge.to];

                const delta = pos_a.sub(pos_b.*);
                const over_amt = delta.length() - 50;
                if (over_amt > 0) {
                    pos_a.* = pos_a.add(delta.normalized().mult(-over_amt * 0.5));
                    pos_b.* = pos_b.add(delta.normalized().mult(over_amt * 0.5));
                }
            }
            for (render_info.?.edges) |edge| {
                const pos_a = render_info.?.nodes[edge.from];
                const pos_b = render_info.?.nodes[edge.to];

                for (0..10) |i| {
                    const p = pos_a.lerp(pos_b, @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(10)));
                    box_drawer.draw_img(graphics, p.x, p.y, 6, 6, .WHITE, 0, 0);
                }
                box_drawer.draw_img(graphics, pos_a.x, pos_a.y, 15, 15, .BLACK, 0, 4);
                box_drawer.draw_img(graphics, pos_b.x, pos_b.y, 15, 15, .BLACK, 0, 4);
            }

            global_num = 0;
            const a = block_widget(root_code_block, .{});
            ui.ui_layout(.{ .x = 0, .y = 0 }, a);
            ui.ui_draw(&box_drawer, graphics, a, res.get(zeng.text_render_res));

            if (mouse_state.mouse_pressed) {
                selected_parameter = null;
            }
            const maybe_hovered_node = ui.recursive_mouse_over(a, mouse_state.mouse_position.x, mouse_state.mouse_position.y);
            if (maybe_hovered_node) |node_ptr| {
                if (node_ptr.data_ptr != null) {
                    zeng.set_cursor(.pointer);
                    if (mouse_state.mouse_pressed) {
                        selected_parameter = @ptrCast(@alignCast(node_ptr.data_ptr.?));
                    }
                }
            }
        }
        if (false) { // block ui part 2
            if (selected_parameter) |sp| {
                for (zeng.key_press_messages.items) |k| {
                    if (k == 8) {
                        const _len = &sp.String.len;
                        if (_len.* > 0) _len.* -= 1;
                    } else {
                        sp.String.str[sp.String.len] = k;
                        sp.String.len += 1;
                    }
                }
            }

            var hovered_p: ?*cb_code_block = null;
            var hovered_c: ?usize = null;
            var hovered_block: ?*cb_code_block = null;
            var hovered_param: ?*cb_parameter = null;

            // for (top_level_code_blocks.items) |code_block| {
            //     _ = get_height(code_block, 18);
            //     draw(code_block, 18, &__ui_box, &__graphics, &__resources, code_block.__rect.pos);
            // }
            for (top_level_code_blocks.items) |code_block| {
                hovered_p, hovered_c, hovered_block, hovered_param = get_mouseover(code_block, null, null, mouse_state.mouse_position);
                if (hovered_block != null) break;
            }
            if (mouse_state.mouse_pressed) {
                if (hovered_block != null) {
                    if (hovered_p != null and hovered_c != null) {
                        dragging_code_block = hovered_p.?.children.orderedRemove(hovered_c.?);
                        top_level_code_blocks.append(allocator, dragging_code_block.?) catch unreachable;
                    } else {
                        dragging_code_block = hovered_block;
                    }
                }
            }
            if (mouse_state.mouse_released) {
                if (hovered_block != null and dragging_code_block != null and hovered_block.? != dragging_code_block.?) {
                    if (hovered_p != null and hovered_c != null) {
                        hovered_p.?.children.insert(allocator, hovered_c.?, dragging_code_block.?) catch unreachable;
                        remove_code_block_ptr(&top_level_code_blocks, dragging_code_block.?);
                    } else {
                        hovered_block.?.children.insert(allocator, 0, dragging_code_block.?) catch unreachable;
                        remove_code_block_ptr(&top_level_code_blocks, dragging_code_block.?);
                    }
                }
            }
            if (!zeng.get_mouse_button(.left)) dragging_code_block = null;

            if (dragging_code_block) |cb| {
                cb.__rect.pos = mouse_state.mouse_position;
            }

            if (zeng.get_key(.r) and !r_pressed_last_frame) {
                hot_reloader.create_script_zig_file(root_code_block, allocator);
                script_context.reset();
            }
            r_pressed_last_frame = zeng.get_key(.r);

            // var buffer: [255]u8 = undefined;
            // zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], ":{} :{}", .{ top_level_code_blocks.items.len, top.children.items.len }) catch unreachable, __resources.get(zeng.text_render_res), 200, 40, __graphics);
        }
        { // enact queued messages + commands
            for (res.get(msg([]const u8)).items()) |str| {
                zeng.render.draw_text(str, res.get(zeng.text_render_res), 0, 0, graphics);
            }
            res.get(msg([]const u8)).clear(allocator);
            for (res.get(msg([3]zeng.vec3)).items()) |tri| {
                zeng.render.debug_draw_triangle(tri, res.get(zeng.debug_res).*);
            }
            res.get(msg([3]zeng.vec3)).clear(allocator);
            const delete_events = res.get(msg(zeng.delete_event));
            for (delete_events.items()) |curr_delete_event| {
                zeng.recursive_delete_entities(curr_delete_event.entity_id, &world);
            }
            delete_events.clear(allocator);
            commands.process_commands(&world);
            net.send_net_messages(&commands, res.get(zeng.time_res).delta_time, &tracker);
            zeng.key_press_messages.clearAndFree(allocator);
        }

        zeng.swap_buffers(graphics);
    }
}

/// Make all entities with a camera and a transform component fly around like a ghost, useful when pausing the simulation
pub fn camera_fly_system(cam: *zeng.main_camera_res, world: *ecs.world, q: *ecs.query(.{ zeng.world_matrix, zeng.fly_component })) !void {
    const cam_matrix = world.get(cam.id, zeng.world_matrix).?;

    var it = q.iterator();
    while (it.next()) |transform_flyer| {
        const transform, _ = transform_flyer;

        var speed: f32 = 0.2;
        if (zeng.get_key(.shift)) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (zeng.get_key(.a)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.d)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult(speed)));
        }
        if (zeng.get_key(.q)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.e)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult(speed)));
        }
        if (zeng.get_key(.w)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.s)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult(speed)));
        }
    }
}
/// Render all meshes and skinned meshes (if they also have a world_matrix component)
pub fn draw_mesh_system(world: *ecs.world, cam: *zeng.main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh }), shadow_map: *zeng.shadow_map_res) !void {
    const cam_matrix = world.get(cam.id, zeng.world_matrix).?;
    const cam_cam = world.get(cam.id, zeng.camera).?;

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam_matrix.*);
    const shadow_light_space_matrix = zeng.mat_mult(shadow_map.projection_matrix, zeng.mat_invert(shadow_map.camera_matrix));

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.render.draw_mesh(mesh.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix, zeng.mat_position(cam_matrix.*), shadow_light_space_matrix, shadow_map);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.render.draw_animated_skinned_mesh(world, skin.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix, zeng.mat_position(cam_matrix.*));
    }
}

pub fn sprite3D_render_system(q: *ecs.query(.{ zeng.world_matrix, zeng.sprite3D }), square_mesh: *sprite3D_mesh_res, world: *ecs.world, cam: *zeng.main_camera_res, shadow_map: *zeng.shadow_map_res) !void {
    const cam_matrix = world.get(cam.id, zeng.world_matrix).?;
    const cam_cam = world.get(cam.id, zeng.camera).?;

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam_matrix.*);
    const shadow_light_space_matrix = zeng.mat_mult(shadow_map.projection_matrix, zeng.mat_invert(shadow_map.camera_matrix));

    var it = q.iterator();
    while (it.next()) |curr| {
        const entity_matrix, _ = curr;

        const p = zeng.mat_position(entity_matrix.*);
        var m = cam_matrix.*;
        zeng.mat_position_set(&m, p);

        zeng.render.draw_mesh(square_mesh.mesh, m, cam_cam.projection_matrix, inv_camera_matrix, zeng.mat_position(cam_matrix.*), shadow_light_space_matrix, shadow_map);
    }
}

/// Buffers spatial data to be interpolated at a high framerate
pub fn frame_interpolator_tick_store_system(interp_q: *ecs.query(.{ zeng.frame_interpolator, zeng.world_matrix })) !void {
    var interp_it = interp_q.iterator();
    while (interp_it.next()) |curr| {
        const fi: *zeng.frame_interpolator, const wm: *zeng.world_matrix = curr;
        fi.store(wm.*);
    }
}
// Default animation behaviour for playing an animation on a skeleton
pub fn animate_skeleton(entity: ecs.entity_id, animation_name: []const u8, delta_time: f32, asset_reg: *zeng.asset_registry, world: *ecs.world) void {
    const animation: *zeng.loader.animation = asset_reg.get(animation_name, zeng.loader.animation);
    const anim = world.get(entity, zeng.animation_component).?;
    const skel = world.get(entity, zeng.skeleton).?;

    anim.time += delta_time / animation.duration;
    while (anim.time > 1.0) {
        anim.time -= 1.0;
    }
    var buffer: [4000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const pose = zeng.create_pose(fba.allocator(), skel.*);
    zeng.get_animation_pose_with_weight(animation, anim.time, pose, 1);
    zeng.apply_pose_to_skeleton(skel, pose);
    zeng.free_pose(fba.allocator(), pose);
}

// MISSING FEATURES:
// networked animations
// overhaul the way that events work
// add more flexible custom serialization functions to allow for dynamic data
// use dynamic data to send variable input messages
// use dynamic data to send snapshots for every replicated entity
// net message pooling
// hot reloading (the correct way)
// scripting language (my language)
// block-based scripting

// NECESSARY IMPROVEMENTS
// make sure audio is thread-safe, deal with multiple samplerates
// robust text rendering
// better rendering - lights
// test and overhaul the reliable message system
// clean up collision code
// add a physics library

// procedural generation for the dungeon
// inventory system
// abilities
// maybe magical programming system?
// A* pathfinding with a node-based system for enemies
// deckbuilder?

// bevy style programming is a must
// main-loop style programming is a must, and it must interact well with the infrastructure and bevy style
// i dont know how to do this, bevy style hides all the complexity much easier

// experimental: have a completely separate id mapping for entities for telling the ecs what id i want for an entity - useful for being able to fetch an entity by a comptime int
// check if performance can be at least as good as the default entity_id lookup
// edit: nah, we need a hashmap that gives us en entity_id
