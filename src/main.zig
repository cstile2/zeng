const std = @import("std");
const zeng = @import("zeng");
const ecs = zeng.ecs;
const rpc = zeng.rpc;
const phy = zeng.phy;
const aud = zeng.aud;
const util = zeng.utils;
const ui = zeng.ui;
const n = ui.n;
const net = zeng.net;
const gl = zeng.gl;
const c = zeng.c;
const vec2 = zeng.vec2;
const msg = zeng.msg;
const hot_reload = @import("hot_reload");

pub fn page_widget(w: f32, h: f32, split_t: f32, address_text: []const u8) *ui.ui_node {
    return n(.{ .width = w, .height = h, .width_size_mode = .fixed, .height_size_mode = .fixed, .color = .CLEAR }, &.{
        n(.{ .width = @round(split_t * w), .width_size_mode = .fixed, .height_size_mode = .grow, .color = .CLEAR, .direction = .bottom_to_top }, &.{
            n(.{ .width_size_mode = .grow, .height_size_mode = .grow, .radius = 8, .color = .CLEAR, .id = "left", .padding = 8, .gap = 8 }, &.{}),
            n(.{ .direction = .top_to_bottom, .height = 90, .width = 100, .width_size_mode = .grow, .height_size_mode = .fixed, .color = .BLACK, .align_children_perp = 1, .align_children_para = 1, .gap = 4 }, &.{
                n(.{ .width = 150, .width_size_mode = .fixed, .height = 20, .height_size_mode = .fixed, .text = address_text, .color = .BLUE, .id = "address_text_box" }, &.{}),
                button_widget(60, 20, "join", "join"),
                button_widget(60, 20, "host", "host"),
            }),
        }),
    });
}
pub fn button_widget(w: f32, h: f32, id: ?[]const u8, text: ?[]const u8) *ui.ui_node {
    return n(.{ .width = w, .width_size_mode = .fixed, .height = h, .height_size_mode = .fixed, .radius = 4, .color = zeng.render.color.GRAY, .id = id, .text = text }, &.{});
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
pub fn block_widget(B: *cb_code_block, pos: vec2, num: *usize) *ui.ui_node {
    var total_width: usize = 0;
    var buff = ui.global_node_allocator.alloc(u8, 255) catch unreachable;
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

    var children = std.ArrayList(*ui.ui_node).initCapacity(ui.global_node_allocator, 0) catch unreachable;
    defer children.deinit(ui.global_node_allocator);

    var rolling_width: usize = 0;
    for (B.params) |*param| {
        if (param.* == .Label) {
            rolling_width += param.Label.len;
        }
        if (param.* == .String) {
            children.append(ui.global_node_allocator, parameter_widget(&param.String, .{ .x = pos.x + @as(f32, @floatFromInt(rolling_width * 12)), .y = pos.y })) catch unreachable;
            rolling_width += @max(1, param.String.len);
        }
    }

    const start = num.*;
    for (B.children.items) |b_child| {
        const delta = num.* - start;
        const new_x = pos.x + 18;
        const new_y = pos.y + @as(f32, @floatFromInt((delta + 1) * 18));
        children.append(ui.global_node_allocator, block_widget(b_child, .{ .x = new_x, .y = new_y }, num)) catch unreachable;
    }

    num.* += 1;
    if (B.children.items.len > 0) {
        num.* += 1;
        children.append(ui.global_node_allocator, n(
            .{ .color = B.color, .pos = .{ .x = pos.x, .y = pos.y }, .width = 18, .height = @floatFromInt((num.* - start) * 18), .width_size_mode = .fixed, .height_size_mode = .fixed, .pos_absolute = true },
            &.{},
        )) catch unreachable;
        children.append(ui.global_node_allocator, n(
            .{ .color = B.color, .pos = .{ .x = pos.x, .y = pos.y + @as(f32, @floatFromInt((num.* - start - 1) * 18)) }, .width = @floatFromInt(total_width * 12), .height = 18, .width_size_mode = .fixed, .height_size_mode = .fixed, .pos_absolute = true },
            &.{},
        )) catch unreachable;
    }
    children.append(ui.global_node_allocator, n(
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

pub const hot_reloader = struct {
    const DLL_PATH = "zig-out/bin/hot_reload_copy.dll";

    dll_hinstance: ?[*c]c.struct_HINSTANCE__ = null,
    last_dll_write_time: i128 = undefined,

    pub fn load_api(this: *@This()) ?hot_reload.hot_reload_procedures {
        const game_lib = zeng.c.LoadLibraryA(DLL_PATH);
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
    player: *zeng.player_module.player_component,
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
        \\  const world = get_item(res, zeng.ecs.world_t);
        \\  const player = world.get(_player.id, zeng.player.player_component).?;
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
pub fn cb_micro_mouse_over(r: cb_rect, m: vec2) bool {
    return (m.x > r.pos.x and m.x < r.pos.x + r.size.x and m.y > r.pos.y and m.y < r.pos.y + r.size.y);
}
pub fn cb_get_mouseover_with_parent_info(B: *cb_code_block, parent: ?*cb_code_block, index: ?usize, mouse_pos: zeng.vec2) struct { ?*cb_code_block, ?usize, ?*cb_code_block, ?*cb_parameter } {
    if (cb_micro_mouse_over(B.__rect, mouse_pos)) {
        for (B.params, B.__params_rects) |*p, pr| {
            if (cb_micro_mouse_over(pr, mouse_pos)) {
                return .{ parent, index, B, p };
            }
        }
        return .{ parent, index, B, null };
    }
    for (B.children.items, 0..) |child, i| {
        const res_p, const res_c, const res_C, _ = cb_get_mouseover_with_parent_info(child, B, i, mouse_pos);
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

pub fn print_entity_hierarchy(e_id: ecs.entity_id, world: *ecs.world_t, tabs: usize) void {
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
    const children = world.get(e_id, zeng.children_component) orelse return;
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

pub fn create_tower(res: *zeng.resources_t, allocator: std.mem.Allocator) ecs.entity_id {
    const default = res.get(rendering_defaults_res);
    const root = zeng.loader.auto_import(res.get(zeng.asset_registry_t), res.get(ecs.world_t), "assets/gltf/people", "Female suit", default.skin_shader, default.static_shader, default.default_texture, allocator);
    const skinned = zeng.find_component_of_type_actual(res.get(ecs.world_t), root, zeng.skinned_mesh, res.get(zeng.resource_fetcher_t).fresh_query(.{zeng.children_component}));
    const sk = skinned.?.skeleton;

    res.get(ecs.world_t).add(zeng.auto_animate_component{}, sk);
    res.get(ecs.world_t).add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, sk);
    return root;
}

pub const rendering_defaults_res = struct {
    skin_shader: u32,
    static_shader: u32,
    default_texture: u32,
};

pub fn auto_animate_system(q: *ecs.query(.{ zeng.skeleton, zeng.animation_component, zeng.auto_animate_component }), time: *zeng.time_res, asset_reg: *zeng.asset_registry_t) !void {
    var it = q.iterator();
    while (it.next()) |curr| {
        const skel, const anim, _ = curr;

        const animation: *zeng.loader.animation = asset_reg.get("assets/gltf/people/KingShiny.gltf/animations/Idle", zeng.loader.animation);

        anim.time += time.fixed_delta_time / animation.duration;
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
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const fba_backing_memory = allocator.alloc(u8, 10000) catch unreachable;
    defer allocator.free(fba_backing_memory);
    var fba = std.heap.FixedBufferAllocator.init(fba_backing_memory);
    ui.global_node_allocator = fba.allocator();

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
    var asset_registry = zeng.asset_registry_t{ .map = std.StringHashMap(*anyopaque).init(allocator) };
    defer asset_registry.map.deinit();
    var world: ecs.world_t = ecs.world_t.init(allocator);
    defer world.deinit();
    var fet: zeng.resource_fetcher_t = .{ .world = &world, .res = &res, .allocator = arena_allocator };
    // var main_hot_reloader = hot_reloader{};

    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});
    zeng.global_world_ptr = &world;

    // engine-wide resources
    const triangle_vao, const triangle_vbo = zeng.loader.create_debug_triangle_mesh();
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
    asset_registry.put("sounds/gun_shot.wav", &gun_shot);
    var bell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/bell.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/bell.wav", &bell);
    var ahem = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/ahem.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/ahem.wav", &ahem);
    var fireball_sound = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/fireball.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/fireball.wav", &fireball_sound);
    var spell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/spell.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/spell.wav", &spell);
    var damage_sound = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/damage.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/damage.wav", &damage_sound);
    var step_sound = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/foot_step.wav", arena_allocator)) catch unreachable;
    asset_registry.put("sounds/foot_step.wav", &step_sound);

    var top_children = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    defer top_children.deinit(allocator);

    zeng.loader.generate_colliders_on_import = false;
    const pistol_entity = zeng.loader.auto_import(&asset_registry, &world, "assets/gltf", "pistol", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(allocator, pistol_entity) catch unreachable;

    const cube_entity = world.spawn(.{ cube_mesh, zeng.mat_tran(zeng.mat_identity, .{ .x = -7.0, .y = 2.0 }), zeng.floater_component{} });

    const test_entity = zeng.loader.auto_import(&asset_registry, &world, "assets/gltf/people", "KingShiny", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(allocator, test_entity) catch unreachable;
    const test_random_skinned_mesh = zeng.find_component_of_type(&world, test_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component})).?;
    const test_skeleton_entity = world.get(test_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, test_skeleton_entity);
    world.get(test_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(test_entity, zeng.world_matrix).?.*, .{ .x = 5 });
    world.add(zeng.auto_animate_component{}, test_skeleton_entity);

    const player_entity = zeng.player_module.construct_local_player(&asset_registry, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);

    zeng.loader.generate_colliders_on_import = true;
    const map_entity = zeng.loader.auto_import(&asset_registry, &world, "assets/gltf", "outdoor_map_6_8_25", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    top_children.append(allocator, map_entity) catch unreachable;

    // const new = zeng.loader.auto_import(&asset_registry, &world, "assets/gltf", "modular_building_set", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    // top_children.append(allocator, new) catch unreachable;
    // world.get(new, zeng.world_matrix).?.* = zeng.mat_scal_aligned(world.get(new, zeng.world_matrix).?.*, zeng.vec3.ONE.mult_scalar(2.0));

    res.insert(rendering_defaults_res{ .default_texture = uv_checker_tex, .skin_shader = skin_shader, .static_shader = static_shader });
    res.insert_ptr(&res);
    res.insert_ptr(&asset_registry);
    res.insert_ptr(&fet);
    var tracker = net.packet_ack_tracker_t{};
    res.insert_ptr(&tracker);
    res.insert(zeng.player_distinguishing_res{ .main_player_id = player_entity });
    const square_vao, const square_indices_length = zeng.loader.create_centered_square_mesh();
    res.insert(zeng.text_render_res{ .shader_program = zeng.loader.load_shader(allocator, "assets/shaders/text_vertex.shader", "assets/shaders/text_fragment.shader"), .texture = zeng.loader.load_texture("assets/images/sdf_font.png", false, false, false), .vao = box_drawer.vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands_t{ .random = res.get(std.Random.Xoshiro256).random(), .remote_messages_send_queue = undefined, .remote_messages_send_queue_len = 0, .allocator = allocator };
    defer commands.destroy();
    res.insert_ptr(&commands);
    res.insert_ptr(&graphics);
    res.insert_ptr(&world);
    res.insert(zeng.rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });
    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, zeng.follow_component{ .target = player_entity, .anchor_point = zeng.mat_position(world.get(player_entity, zeng.world_matrix).?.*) } });
    zeng.global_camera_entity = main_camera;
    res.insert(zeng.debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader });
    res.insert(zeng.main_camera_res{ .id = main_camera });
    world.get(player_entity, zeng.player_module.player_component).?.camera = main_camera;
    zeng.window_resize_handler(graphics.width, graphics.height);
    var peer_map = std.AutoHashMap(net.peer_info_t, zeng.client_info).init(allocator);
    defer peer_map.deinit();
    var inverse_peer_map = std.AutoHashMap(ecs.entity_id, net.peer_info_t).init(allocator);
    defer inverse_peer_map.deinit();
    res.insert_ptr(&peer_map);
    res.insert_ptr(&inverse_peer_map);
    res.insert(zeng.shadow_map_res.init(allocator));
    res.insert(zeng.mouse_state_res{ .mouse_position = undefined, .mouse_pressed = undefined, .mouse_released = undefined });
    var colliders = std.ArrayList(phy.convex_collider).initCapacity(allocator, 0) catch unreachable;
    defer colliders.deinit(allocator);
    res.insert_ptr(&colliders);
    var spatial_hash_grid = std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.convex_collider)).init(allocator);
    defer spatial_hash_grid.deinit();
    res.insert_ptr(&spatial_hash_grid);
    const fixed_rate: f64 = 60.0;
    const fixed_delta: f64 = 1.0 / fixed_rate;
    res.insert(zeng.time_res{ .delta_time_f64 = 0.006944, .delta_time = 0.006944, .fixed_delta_time_f64 = fixed_delta, .fixed_delta_time = @floatCast(fixed_delta) });
    res.insert(zeng.sprite3D_render_res{ .rect_mesh = zeng.mesh{ .indices_length = square_indices_length, .vao_gpu = square_vao, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = box_drawer.shader_program, .parameter_map = undefined } } });
    const font_mikado = ui.parse_font_descriptor("assets/fonts/mikado-medium-58b66e4a");
    const font_jetbrains = ui.parse_font_descriptor("assets/fonts/jetbrainsmono-medium-50b9ac8e");
    res.insert(sprite3D_mesh_res{ .mesh = zeng.loader.create_centered_square_mesh_formatted_for_sprite3D(.{ .shader_program = static_shader, .parameter_map = pblk: {
        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
        _value.put("albedo_texture", .{ .texture = zeng.loader.load_texture("assets/images/orb.png", true, false, true) }) catch unreachable;
        _value.put("albedo", .{ .float_3 = zeng.vec3.ONE }) catch unreachable;
        _value.put("metallic", .{ .float_1 = 0.0 }) catch unreachable;
        _value.put("roughness", .{ .float_1 = 0.5 }) catch unreachable;
        break :pblk _value;
    } }) });
    const sdf_text_rect_mesh = zeng.mesh{ .vao_gpu = square_vao, .indices_length = square_indices_length, .indices_type = zeng.gl.UNSIGNED_INT, .material = undefined };
    var events = zeng.events_t{ .commands = &commands, .tracker = &tracker, .res = &res };
    res.insert_ptr(&events);
    var collision_space = zeng.collision_space_t.init(allocator);
    defer collision_space.deinit();
    res.insert_ptr(&collision_space);
    inline for (zeng.generated_types.net_event_types) |T| res.insert(msg(T).init(allocator, true));

    top_children.append(allocator, create_tower(&res, arena_allocator)) catch unreachable;

    { // construct spatial hash grid
        for (zeng.loader.global_collider_meshes.?.items, zeng.loader.global_matrices.?.items) |_mesh, _matrix| {
            var curr_tri: usize = 0;
            while (curr_tri < _mesh.indices.len) { // turn each mesh into a bunch of individual triangle colliders
                defer curr_tri += 3;
                const cool = arena_allocator.create(phy.mesh_triangle_data) catch unreachable;
                cool.positions = _mesh.positions;
                cool.indices = .{ _mesh.indices[curr_tri], _mesh.indices[curr_tri + 1], _mesh.indices[curr_tri + 2] };
                const collider = phy.convex_collider{ .matrix = _matrix, .support = phy.mesh_triangle, .tag = .support_based, .data = @ptrCast(cool) };
                colliders.append(allocator, collider) catch unreachable;
            }
        }
        // center grid cell is centered at (0,0)
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
    // var selected_parameter: ?*cb_parameter = null;

    var proc_anim: proc = .{ .res = &res };

    var address_text = std.ArrayList(u8).initCapacity(allocator, 0) catch unreachable;
    defer address_text.deinit(allocator);
    address_text.appendSlice(allocator, "127.0.0.1") catch unreachable;

    var active_text: ?*std.ArrayList(u8) = null;
    var multiplayer_selector_ui: bool = true;

    var top_level_code_blocks = std.ArrayList(*cb_code_block).initCapacity(allocator, 0) catch unreachable;
    defer top_level_code_blocks.deinit(allocator);
    top_level_code_blocks.append(allocator, root_code_block) catch unreachable;
    var script_context: cb_script_context = undefined;
    script_context.init(allocator);
    defer script_context.deinit();

    var hit_indicator_direction: zeng.vec3 = .ZERO;
    var hit_indicator_timer: f32 = 0.0;

    // var game_api = main_hot_reloader.hot_reload_api() catch unreachable;
    // main_hot_reloader.store_last_dll_write_time("zig-out/bin/hot_reload.dll");

    // var positions: [1028]zeng.vec3 = undefined;
    // var positions_len: usize = 0;

    var result: ?phy.raycast_result_t = null;

    zeng.frame_timer_warmup();
    while (true) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);
        if (zeng.quit) break;
        commands.time += res.get(zeng.time_res).delta_time_f64;
        if (res.get_maybe(zeng.multiplayer_res)) |mr| {
            zeng.net.recieve_net_messages(mr.main_socket, &res, allocator, &tracker);
        }

        // main_hot_reloader.try_new_api(&game_api);

        const world_matrix_q = fet.fresh_query(.{zeng.world_matrix});
        const children_q = fet.fresh_query(.{zeng.children_component});
        const local_matrix_q = fet.fresh_query(.{zeng.local_matrix});

        const mouse_state = res.get(zeng.mouse_state_res);
        mouse_state.mouse_position = vec2{ .x = @floatFromInt(zeng.global_mouse_screen_pos[0]), .y = @floatFromInt(zeng.global_mouse_screen_pos[1]) };
        mouse_state.mouse_pressed = zeng.get_mouse_button(.left) and !mouse_down_last_frame;
        mouse_state.mouse_released = !zeng.get_mouse_button(.left) and mouse_down_last_frame;
        defer mouse_down_last_frame = zeng.get_mouse_button(.left);

        { // mouse locking/unlocking
            if (zeng.get_mouse_button(.left) and zeng.get_key(.k)) {
                zeng.lock_cursor_to_window(graphics.hwnd);
                zeng.hide_cursor();
            }
            if (zeng.get_key(.escape)) {
                zeng.unlock_cursor();
                zeng.show_cursor();
            }
        }

        const multiplayer = res.get_maybe(zeng.multiplayer_res);
        if (multiplayer != null) {
            if (multiplayer.?.is_server) { // server frame communication
                var input_events = res.get(msg(rpc.input_chunk)).iterator();
                while (input_events.iterate()) |input_event| {
                    const info = peer_map.getPtr(input_events.get_sender()) orelse break;
                    for (input_event.arr) |_input_event| {
                        if (_input_event.tick >= tick) {
                            info.input_buffer.set(_input_event.tick, _input_event);
                        } // else discard late message
                    }
                }
                var player_join_events = res.get(msg(rpc.player_spawn_message)).iterator();
                while (player_join_events.iterate()) |_| {
                    std.debug.print("player connected: \n", .{});

                    const new_remote_player_entity = zeng.player_module.create_player(&asset_registry, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator, .{ .net_id = zeng.get_new_netid(), .remote_peer = player_join_events.get_sender() });

                    world.get(new_remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(new_remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 60.0 });
                    world.get(new_remote_player_entity, zeng.player_module.player_component).?.camera = world.spawn(.{zeng.mat_identity});

                    peer_map.put(player_join_events.get_sender(), zeng.client_info{ .input_buffer = zeng.ring_buffer(rpc.input_message){ .arr = undefined }, .player = new_remote_player_entity }) catch unreachable;
                    inverse_peer_map.put(new_remote_player_entity, player_join_events.get_sender()) catch unreachable;
                }
                var client_tick_events = res.get(msg(rpc.client_tick)).iterator();
                while (client_tick_events.iterate()) |client_tick_event| {
                    events.send_remote(client_tick_events.get_sender(), rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
                }
            }

            if (!multiplayer.?.is_server) { // client frame communication + synchronization with server
                var server_tick_offset_events = res.get(msg(rpc.server_tick_offset)).iterator();
                while (server_tick_offset_events.iterate()) |server_tick_offset_event| {
                    const rtt = synced_time - server_tick_offset_event.client_time;
                    draw_rtt = rtt;
                    exponential_rtt = zeng.lerp(exponential_rtt, rtt, 0.08);

                    const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
                    draw_time_alignment = time_offset;
                    std.debug.print("offset: {}\n", .{time_offset});

                    if (@abs(time_offset) > 0.7) { // time offset is too far, simply jump forward in time
                        synced_time += time_offset;
                    } else { // adjust client simulation speed to smoothly sync with server
                        sync_clock_timescale = 1.0 + (time_offset / 50.0);
                    }
                    if (!server_has_responded) {
                        buffer_time = 0.4;
                    }
                    server_has_responded = true;
                }
                var state_correction_events = res.get(msg(rpc.state_correction)).iterator();
                while (state_correction_events.iterate()) |snap_event| {
                    const P = world.get(player_entity, zeng.player_module.player_component).?;
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
                        zeng.player_module.simulate_player(world.get(player_entity, zeng.player_module.player_component).?, &im, world.get(player_entity, zeng.world_matrix).?, res.get(zeng.time_res));
                        zeng.player_module.simulate_collision(world.get(player_entity, zeng.player_module.player_component).?, world.get(player_entity, zeng.world_matrix).?, &spatial_hash_grid, res.get(msg([3]zeng.vec3)));
                    }
                    draw_resims = @intCast(@max(tick - snap_event.tick, 0));

                    zeng.sync_transforms_children(player_entity, world_matrix_q, children_q, local_matrix_q);
                }
                var missed_input_events = res.get(msg(rpc.speed_client_up)).iterator();
                while (missed_input_events.iterate()) |_| {
                    if (server_has_responded and buffer_cooldown <= 0.0) {
                        buffer_velocity = 0.04;
                        buffer_cooldown = 0.3;
                        std.debug.print("hello\n", .{});
                    }
                }
                var world_update_events = res.get(msg(rpc.world_update)).iterator();
                while (world_update_events.iterate()) |world_update_event| {
                    const SI = world.get(multiplayer.?.remote_player_entity, zeng.snapshot_interpolator).?;
                    SI.buffer.set(world_update_event.tick, .{ .position = zeng.mat_position(world_update_event.server_player_matrix), .tick = world_update_event.tick });

                    const SI2 = world.get(cube_entity, zeng.snapshot_interpolator).?;
                    SI2.buffer.set(world_update_event.tick, .{ .position = world_update_event.cube_pos, .tick = world_update_event.tick });

                    const target = @as(f64, @floatFromInt(world_update_event.tick - tick));
                    draw_interpolated_tick_delta = target;
                }
                { // snapshot interpolation
                    const integer_tick_float: f32 = @floatFromInt(tick);
                    const fractional_tick_float: f32 = @floatCast(accumulator * fixed_rate);
                    const _tick_float = integer_tick_float + fractional_tick_float;
                    var tick_float = zeng.lerp(@as(f32, @floatCast(synced_time * fixed_rate)), _tick_float, -1.0); // double check this is a good idea - it is technical scalable
                    tick_float = @min(tick_float, _tick_float - 4.0);
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
                            std.debug.assert(t_value <= 1.0 and t_value >= 0.0);

                            zeng.mat_position_set(M, SI.buffer.get(A).position.lerp(SI.buffer.get(B).position, t_value));
                        } else {
                            std.debug.print("interp error: me: {} interpolation: {}\n", .{ _tick_float, tick_float });
                        }
                    }
                }
                if (server_has_responded) { // server-client clock sync algorithm
                    buffer_cooldown -= res.get(zeng.time_res).delta_time_f64;
                    if (buffer_cooldown < -10.0) {
                        buffer_velocity = -0.005;
                    } else if (buffer_cooldown < 0.0) {
                        buffer_velocity = 0.0;
                    }
                    // buffer_time += res.get(zeng.time_res).delta_time_f64 * buffer_velocity;
                    buffer_time = (exponential_rtt * 0.5) * 1.6 + 0.015; // this is a made up heuristic

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
        }

        synced_time += res.get(zeng.time_res).delta_time_f64 * sync_clock_timescale;
        accumulator += res.get(zeng.time_res).delta_time_f64 * sim_timescale;
        while (accumulator >= fixed_delta) { // fixed timestep loop
            defer tick += 1;
            accumulator -= fixed_delta;

            if (multiplayer != null and multiplayer.?.is_server) { // use collected input messages, apply them to players
                var remote_players_iterator = peer_map.iterator();
                while (remote_players_iterator.next()) |remote_player_entry| {
                    const current_frame_input = remote_player_entry.value_ptr.input_buffer.get(tick);
                    if (current_frame_input.tick == tick) { // check if we have this tick for this player, otherwise player state becomes incorrect
                        const ent = remote_player_entry.value_ptr.player;
                        const remote_player_input = world.get(ent, rpc.input_message).?;
                        remote_player_input.* = current_frame_input;
                    } else {
                        std.debug.print("missed input for tick: {}\n", .{tick});
                    }
                    const TARGET_TICK_EARLINESS = 2;
                    if (remote_player_entry.value_ptr.input_buffer.get(tick + TARGET_TICK_EARLINESS).tick != tick + TARGET_TICK_EARLINESS) { // also check if this client is ready for 2 TICKS IN THE FUTURE. we want a small grace window in case network spikes
                        events.send_remote(remote_player_entry.key_ptr.*, rpc.speed_client_up{}, .unreliable);
                    }
                }
            }
            { // apply input to local player
                var local_player_shoot = false;
                if (zeng.get_mouse_button(.left) and !key_pressed_fixed_update) {
                    local_player_shoot = true;
                    key_pressed_fixed_update = true;
                }
                if (!zeng.get_mouse_button(.left)) key_pressed_fixed_update = false;
                const local_player_input = world.get(player_entity, rpc.input_message).?;
                local_player_input.* = rpc.input_message{ .tick = tick, .jump = zeng.input_implement.default_jump(), .sprint = zeng.get_key(.shift), .move_vect = zeng.input_implement.default_move_fn(), .rot_x = local_player_input.rot_x, .rot_y = local_player_input.rot_y, .shoot = local_player_shoot, .aiming = zeng.get_mouse_button(.right), .shoot_origin = zeng.mat_position(world.get(main_camera, zeng.world_matrix).?.*) };
            }
            if (multiplayer != null and !multiplayer.?.is_server) { // send input chunk from last several input ticks to server
                client_input_buffer.set(tick, world.get(player_entity, rpc.input_message).?.*);
                var snd: rpc.input_chunk = undefined;
                var curr: usize = 0;
                while (curr < snd.arr.len) {
                    defer curr += 1;
                    snd.arr[curr] = client_input_buffer.get(tick - @as(isize, @intCast(curr)));
                }
                events.send_remote(res.get(zeng.multiplayer_res).server_peer, snd, .unreliable);
            }
            if (multiplayer != null and multiplayer.?.is_server) { // on server, move the cube in sync with the time
                const floater_q = fet.fresh_query(.{ zeng.floater_component, zeng.world_matrix });
                var floater_it = floater_q.iterator();
                while (floater_it.next()) |curr| {
                    _, const M = curr;
                    zeng.mat_position_set(M, .{ .x = -7.0, .y = 2.0, .z = @mod(@as(f32, @floatFromInt(tick)) * 0.01, 1.0) * 20.0 - 5.0 });
                }
            }

            fet.run_system(camera_fly_system);
            fet.run_system(zeng.player_module.player_simulate_and_animate_system);
            fet.run_system(zeng.player_module.player_collision_system);
            fet.run_system(auto_animate_system);
            fet.run_system(frame_interpolator_tick_store_system);
            fet.run_system(zeng.player_module.shoot_system);

            if (multiplayer != null) {
                if (multiplayer.?.is_server) { // periodic client/server communication
                    var client_it = peer_map.iterator();
                    while (client_it.next()) |thing| {
                        const P = world.get(thing.value_ptr.player, zeng.player_module.player_component).?.*;
                        const M = world.get(thing.value_ptr.player, zeng.world_matrix).?.*;
                        if (@as(usize, @intCast(tick)) % 4 == 0) events.send_remote(thing.key_ptr.*, rpc.state_correction{ .tick = tick, .state = P, .world_matrix = M }, .unreliable);
                    }

                    client_it = peer_map.iterator();
                    while (client_it.next()) |thing| {
                        if (@as(usize, @intCast(tick)) % 1 == 0) {
                            events.send_remote(thing.key_ptr.*, rpc.world_update{ .cube_pos = zeng.mat_position(world.get(cube_entity, zeng.world_matrix).?.*), .server_player_matrix = world.get(player_entity, zeng.world_matrix).?.*, .tick = tick }, .unreliable);
                        }
                    }
                } else {
                    if (@as(usize, @intCast(tick)) % 30 == 0) {
                        events.send_remote(multiplayer.?.server_peer, rpc.client_tick{ .time = synced_time }, .unreliable);
                    }
                }
            }
        }

        { // player frame interpolation + camera + procedural animation
            const player_interpolator = world.get(player_entity, zeng.frame_interpolator).?;
            const interpolated_matrix = player_interpolator.get_matrix(@as(f32, @floatCast(accumulator / fixed_delta)));
            zeng.mat_position_set(&(res.get(zeng.shadow_map_res).camera_matrix), zeng.mat_position(interpolated_matrix).add(.{ .y = 15, .z = 8 }));

            proc_anim.procedural_animation(interpolated_matrix, pistol_entity, world_matrix_q, children_q, local_matrix_q);
            // _ = &proc_anim;

            result = null;
        }
        for (top_children.items) |ch| { // sync all transforms TODO: make a "root" entity and use a children component to achieve this concept
            if (!world.is_alive(ch)) continue;
            zeng.sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
        }
        if (multiplayer != null and !multiplayer.?.is_server) { // animation host player on client
            const skel = world.get(multiplayer.?.replicated_player_skeleton_entity, zeng.skeleton).?;
            const anim = world.get(multiplayer.?.replicated_player_skeleton_entity, zeng.animation_component).?;
            const animation = asset_registry.get("assets/gltf/people/KingShiny.gltf/animations/Idle", zeng.loader.animation);

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

            // var dds = res.get(msg(zeng.phy.debug_draw_stuff)).iterator();
            // while (dds.iterate()) |dds_msg| {
            //     if (dds_msg.clear) {
            //         positions_len = 0;
            //     } else if (dds_msg.add_position.length() > 0.000001) {
            //         positions[positions_len] = dds_msg.add_position;
            //         positions_len += 1;
            //     }
            // }
            // for (positions[0..positions_len]) |pos| {
            //     zeng.render.debug_draw_triangle(.{ pos.add(.{ .y = 0.2 }), pos.add(.{ .x = 0.2 }), pos }, &res);
            // }
            if (result) |R| {
                const R_pos_b = R.position.add(R.normal.normalized());
                zeng.render.debug_draw_triangle(.{ R.position, R_pos_b.add(.{ .x = 0.05 }), R_pos_b }, &res);
            }
        }
        { // debug triangle at mouse position in 3D space
            // const main_camera_camera = world.get(main_camera, zeng.camera).?;
            // const main_camera_matrix = world.get(main_camera, zeng.world_matrix).?;

            // const near, const far = mouse_position_to_vec3s(main_camera_camera.*, main_camera_matrix.*, mouse_state.mouse_position, graphics);
            // const ray = far.sub(near).normalized();

            // const P = zeng.mat_position(main_camera_matrix.*).add(ray);
            // zeng.render.debug_draw_triangle(.{ P.add(.{ .y = 0.2 }), P.add(.{ .x = 0.2 }), P }, &res);
        }
        { // makeshift UI debug HUD
            var buffer: [64]u8 = undefined;
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / res.get(zeng.time_res).delta_time_f64}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 0, 0.35, graphics);

            if (multiplayer != null and !multiplayer.?.is_server) {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "synctime: {d:.6}", .{synced_time}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 50, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffer: {d:.6}", .{buffer_time}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 75, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "timescale: {d:.6}", .{sim_timescale}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 100, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffvel: {d:.6}", .{buffer_velocity}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 125, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "    rtt: {d:.6}", .{draw_rtt}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 150, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "exp rtt: {d:.6}", .{exponential_rtt}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 175, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "resims: {}", .{draw_resims}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 200, 0.35, graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "interp: {d:.6}", .{draw_interpolated_tick_delta}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 225, 0.35, graphics);

                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), @as(f32, @floatCast(draw_time_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);

                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);
            } else {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accumulator}) catch unreachable, if (zeng.get_key(.p)) font_mikado else font_jetbrains, sdf_text_rect_mesh, 0, 25, 0.35, graphics);
            }

            var ticker_display_time: f64 = undefined;
            if (multiplayer != null and multiplayer.?.is_server) {
                ticker_display_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator;
            } else {
                ticker_display_time = synced_time;
            }
            // zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), @floatCast(@mod(ticker_display_time * 100.0, 100.0) - 400), 400, 10, 10, zeng.render.color.YELLOW);
            box_drawer.draw_img(graphics, @floatCast(@mod(ticker_display_time * 150.0, 150.0)), 0, 15, 15, .YELLOW, 0, 0);
            zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 0, 6, 6, zeng.render.color.BLACK);
            zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), 0, 0, 4, 4, zeng.render.color.WHITE);
        }
        if (multiplayer_selector_ui) { // new ui system
            zeng.set_cursor(.arrow);

            const page = page_widget(@floatFromInt(graphics.width), @floatFromInt(graphics.height), 0.3, address_text.items);
            ui.ui_layout(.{ .x = 0, .y = 0 }, page);
            const nptr = ui.recursive_mouse_over(page, mouse_state.mouse_position.x, mouse_state.mouse_position.y);
            if (mouse_state.mouse_pressed) {
                active_text = null;
            }
            if (nptr) |node_ptr| {
                if (node_ptr.text != null) {
                    zeng.set_cursor(.pointer);
                    if (mouse_state.mouse_pressed) {
                        if (node_ptr.id != null) {
                            if (std.mem.eql(u8, node_ptr.id.?, "address_text_box")) {
                                active_text = &address_text;
                                aud.play_sound(asset_registry.get("sounds/bell.wav", aud.audio_sample_info).*, .one_shot);
                            } else if (std.mem.eql(u8, node_ptr.id.?, "join")) {
                                res.insert(zeng.multiplayer_res{ .is_server = false, .main_socket = undefined, .remote_player_entity = undefined, .replicated_player_skeleton_entity = undefined, .server_peer = undefined });

                                const _mr = res.get(zeng.multiplayer_res);

                                _mr.main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, false) catch unreachable;
                                _mr.server_peer = net.peer_info_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };

                                events.send_remote(res.get(zeng.multiplayer_res).server_peer, rpc.player_spawn_message{}, .reliable);

                                _mr.remote_player_entity = zeng.player_module.construct_replicated_player(&asset_registry, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);
                                const remote_player_random_skinned_mesh = zeng.find_component_of_type(&world, _mr.remote_player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children_component})).?;
                                _mr.replicated_player_skeleton_entity = world.get(remote_player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;

                                world.add(zeng.snapshot_interpolator{ .buffer = undefined }, cube_entity);

                                multiplayer_selector_ui = false;
                            } else if (std.mem.eql(u8, node_ptr.id.?, "host")) {
                                multiplayer_selector_ui = false;

                                res.insert(zeng.multiplayer_res{ .is_server = true, .main_socket = undefined, .remote_player_entity = undefined, .replicated_player_skeleton_entity = undefined, .server_peer = undefined });

                                const _mr = res.get(zeng.multiplayer_res);

                                _mr.main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, true) catch unreachable;
                                _mr.server_peer = net.peer_info_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };
                            }
                        }
                    }
                }
            }
            if (active_text) |tb| {
                for (zeng.key_press_messages.items) |k| {
                    if (k == 8) {
                        _ = tb.pop();
                    } else {
                        tb.append(allocator, k) catch unreachable;
                    }
                }
            }

            // const button = ui.ui_id_map.get("play_button");
            // if (ui.mouse_over(button, mouse_state.mouse_position.x, mouse_state.mouse_position.y)) {
            //     zeng.set_cursor(.pointer);
            //     button.color = .WHITE;
            //     if (mouse_state.mouse_pressed) {
            //         // game_api.on_button_pressed(&res);
            //     }
            // }
            ui.ui_draw(&box_drawer, graphics, page, sdf_text_rect_mesh, font_jetbrains);

            // var _num: usize = 0;
            // const a = block_widget(root_code_block, .{}, &_num);
            // ui.ui_layout(.{ .x = 0, .y = 0 }, a);
            // ui.ui_draw(&box_drawer, graphics, a, sdf_text_rect_mesh, font_jetbrains);

            // if (mouse_state.mouse_pressed) {
            //     selected_parameter = null;
            // }
            // const maybe_hovered_node = ui.recursive_mouse_over(a, mouse_state.mouse_position.x, mouse_state.mouse_position.y);
            // if (maybe_hovered_node) |node_ptr| {
            //     if (node_ptr.data_ptr != null) {
            //         zeng.set_cursor(.pointer);
            //         if (mouse_state.mouse_pressed) {
            //             selected_parameter = @ptrCast(@alignCast(node_ptr.data_ptr.?));
            //         }
            //     }
            // }
            // if (selected_parameter) |sp| {
            //     for (zeng.key_press_messages.items) |k| {
            //         if (k == 8) {
            //             const _len = &sp.String.len;
            //             if (_len.* > 0) _len.* -= 1;
            //         } else {
            //             sp.String.str[sp.String.len] = k;
            //             sp.String.len += 1;
            //         }
            //     }
            // }
        }
        if (false) { // block ui part 2

            var hovered_p: ?*cb_code_block = null;
            var hovered_c: ?usize = null;
            var hovered_block: ?*cb_code_block = null;
            var hovered_param: ?*cb_parameter = null;

            for (top_level_code_blocks.items) |code_block| {
                hovered_p, hovered_c, hovered_block, hovered_param = cb_get_mouseover_with_parent_info(code_block, null, null, mouse_state.mouse_position);
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
            var got_hit_events = res.get(msg(rpc.got_hit)).iterator();
            while (got_hit_events.iterate()) |got_hit_event| {
                aud.play_sound(damage_sound, .one_shot);
                const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
                const delta = got_hit_event.source_position.sub(zeng.mat_position(cam_matrix.*)).normalized();
                hit_indicator_direction = delta;
                hit_indicator_timer = 3.0;

                // std.debug.print("{}\n}", .{got_hit_event.source_position});

            }
            var hitmarker_events = res.get(msg(rpc.hitmarker)).iterator();
            while (hitmarker_events.iterate()) |_| {
                aud.play_sound(bell, .one_shot);
            }
            var delete_events = res.get(msg(zeng.delete_event)).iterator();
            while (delete_events.iterate()) |curr_delete_event| {
                zeng.recursive_delete_entities(curr_delete_event.entity_id, &world);
            }
            commands.process_commands(&world);
            net.send_net_messages(&commands, res.get(zeng.time_res).delta_time_f64, &tracker);
            zeng.key_press_messages.clearAndFree(allocator);
        }
        if (hit_indicator_timer > 0.0) {
            const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
            const right = zeng.mat_right(cam_matrix.*).slide(.UP).normalized();
            const fwd = zeng.mat_right(cam_matrix.*).cross(.UP).normalized();

            const __x = hit_indicator_direction.dot(right) * 220;
            const __y = hit_indicator_direction.dot(fwd) * -220;
            zeng.render.draw_rect(graphics, res.get(zeng.rect_render_res), __x, __y, 25, 25, .RED);
        }
        hit_indicator_timer -= res.get(zeng.time_res).delta_time;

        fba.reset();

        zeng.swap_buffers(graphics);
    }
}

/// Make all entities with a camera and a transform component fly around like a ghost, useful when pausing the simulation
pub fn camera_fly_system(cam: *zeng.main_camera_res, world: *ecs.world_t, q: *ecs.query(.{ zeng.world_matrix, zeng.fly_component })) !void {
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
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult_scalar(-speed)));
        }
        if (zeng.get_key(.d)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult_scalar(speed)));
        }
        if (zeng.get_key(.q)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult_scalar(-speed)));
        }
        if (zeng.get_key(.e)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult_scalar(speed)));
        }
        if (zeng.get_key(.w)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult_scalar(-speed)));
        }
        if (zeng.get_key(.s)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult_scalar(speed)));
        }
    }
}
/// Render all meshes and skinned meshes (if they also have a world_matrix component)
pub fn draw_mesh_system(world: *ecs.world_t, cam: *zeng.main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh }), shadow_map: *zeng.shadow_map_res) !void {
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

pub fn sprite3D_render_system(q: *ecs.query(.{ zeng.world_matrix, zeng.sprite3D }), square_mesh: *sprite3D_mesh_res, world: *ecs.world_t, cam: *zeng.main_camera_res, shadow_map: *zeng.shadow_map_res) !void {
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
        m = zeng.mat_scal(m, zeng.vec3.ONE.mult_scalar(0.5));

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
pub fn animate_skeleton(entity: ecs.entity_id, animation_name: []const u8, delta_time: f32, asset_reg: *zeng.asset_registry_t, world: *ecs.world_t) void {
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

// TODO:
// collision depenetration via EPA or tootbird or similar
// networked animations
// add more flexible custom serialization functions to allow for dynamic data
// use dynamic data to send variable input messages
// use dynamic data to send snapshots for every replicated entity
// net message pooling
// block-based scripting
// better rendering - lights
// test and overhaul the reliable message system
// add an additional physics library

// To make this engine worth using:
// make asset handling easier and automatic

pub const proc = struct {
    pistol_lerp_amount: f32 = 0.0,

    pistol_recoil_rotation: f32 = 0.0,
    pistol_recoil_rotation_speed: f32 = 0.0,

    camera_last_frame_rotations: zeng.vec2 = zeng.vec2.ZERO,
    camera_rotational_velocity: zeng.vec2 = zeng.vec2.ZERO,
    camera_last_rotational_velocity: zeng.vec2 = zeng.vec2.ZERO,

    camera_recoil: f32 = 0.0,
    lerped_camera_recoil: f32 = 0.0,

    pistol_noise_position: zeng.vec2 = zeng.vec2.ZERO,
    camera_shake_position: vec2 = zeng.vec2.ZERO,
    camera_shake_amount: f32 = 0.0,

    player_step_phase: f32 = 0.0,

    pistol_lerped_bob: zeng.vec3 = .ZERO,

    res: *zeng.resources_t,
    pub fn procedural_animation(this: *@This(), interpolated_matrix: [16]f32, pistol_entity: ecs.entity_id, world_matrix_q: anytype, children_q: anytype, local_matrix_q: anytype) void {
        const delta_time = this.res.get(zeng.time_res).delta_time;
        const world = this.res.get(ecs.world_t);
        const main_camera = this.res.get(zeng.main_camera_res).id;
        const graphics = this.res.get(zeng.graphics_t);
        const player_entity = this.res.get(zeng.player_distinguishing_res).main_player_id;
        const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
        const cam = world.get(main_camera, zeng.camera).?;
        const asset_registry = this.res.get(zeng.asset_registry_t);

        if (zeng.get_mouse_button(.right)) {
            cam.fov = zeng.lerp(cam.fov, 1.0, 18 * delta_time);
        } else {
            cam.fov = zeng.lerp(cam.fov, 1.5, 18 * delta_time);
        }
        cam.projection_matrix = zeng.mat_perspective_projection(cam.fov, @as(f32, @floatFromInt(graphics.width)) / @as(f32, @floatFromInt(graphics.height)), 0.01, 1000.0);

        const camera_target_position = zeng.mat_position(interpolated_matrix);
        const local_player_input = world.get(player_entity, rpc.input_message).?;

        // cam_matrix.* = zeng.mat_mult(
        //     zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(local_player_input.rot_x)),
        //     zeng.mat_mult(zeng.mat_axis_angle(zeng.vec3.RIGHT, @as(f32, @floatCast(local_player_input.rot_y)) + this.lerped_camera_recoil), zeng.mat_identity),
        // );
        // cam_matrix.* = zeng.mat_mult(zeng.mat_axis_angle(zeng.mat_forward(cam_matrix.*), @sin(2.0 * 3.1415 * this.player_step_phase)), cam_matrix.*);
        // zeng.mat_position_set(cam_matrix, camera_target_position.add(zeng.vec3{ .y = 0.8 }));

        const player_c = world.get(player_entity, zeng.player_module.player_component).?;

        if (player_c.velocity.length() < 0.0001 or !player_c.grounded) {
            if (this.player_step_phase > 0.6) {
                aud.play_sound(asset_registry.get("sounds/foot_step.wav", aud.audio_sample_info).*, .one_shot);
            }
            this.player_step_phase = 0.5;
        } else {
            const old_phase = this.player_step_phase;
            this.player_step_phase += player_c.velocity.length() * delta_time * 0.3;

            if (old_phase < 0.5 and this.player_step_phase >= 0.5 or old_phase < 1.0 and this.player_step_phase >= 1.0) {
                aud.play_sound(asset_registry.get("sounds/foot_step.wav", aud.audio_sample_info).*, .one_shot);
            }
        }
        while (this.player_step_phase > 1.0) {
            this.player_step_phase -= 1.0;
        }

        cam_matrix.* = zeng.mat_mult(
            zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(local_player_input.rot_x)),
            zeng.mat_mult(zeng.mat_axis_angle(zeng.vec3.RIGHT, @as(f32, @floatCast(local_player_input.rot_y)) + this.lerped_camera_recoil), zeng.mat_identity),
        );
        cam_matrix.* = zeng.mat_mult(zeng.mat_axis_angle(zeng.vec3.FORWARD, util.perlin(this.camera_shake_position) * 0.05 * this.camera_shake_amount), cam_matrix.*);
        cam_matrix.* = zeng.mat_mult(zeng.mat_axis_angle(zeng.mat_forward(cam_matrix.*), 0.01 * @sin(2.0 * 3.1415 * this.player_step_phase)), cam_matrix.*);
        zeng.mat_position_set(cam_matrix, camera_target_position.add(zeng.vec3{ .y = 0.8 + 0.02 * @sin(4.0 * 3.1415 * this.player_step_phase) }));

        const pistol_local_position: zeng.vec3 = .{ .z = -0.17, .x = 0.13, .y = -0.15 };
        const pistol_local_position2: zeng.vec3 = .{ .z = -0.17, .y = -0.11 };

        var pistol_effect_multiplier: f32 = 1.0;
        if (zeng.get_mouse_button(.right)) {
            this.pistol_lerp_amount = zeng.lerp(this.pistol_lerp_amount, 1.0, 25.0 * delta_time);
            zeng.global_mouse_sensitivity = 0.0005;
            pistol_effect_multiplier = 0.3;
        } else {
            this.pistol_lerp_amount = zeng.lerp(this.pistol_lerp_amount, 0.0, 25.0 * delta_time);
            zeng.global_mouse_sensitivity = 0.001;
        }

        if (zeng.player_module.shoot_presentation_trigger) {
            this.pistol_recoil_rotation_speed = 12.0;
            this.camera_recoil += 0.08;
            this.camera_shake_amount = 1.0;
            aud.play_sound(asset_registry.get("sounds/gun_shot.wav", aud.audio_sample_info).*, .one_shot);
        }
        this.camera_shake_amount = zeng.lerp(this.camera_shake_amount, 0.0, 10 * delta_time);
        zeng.player_module.shoot_presentation_trigger = false;

        const estimated_rotational_velocity = zeng.vec2.sub(.{ .x = @floatCast(local_player_input.rot_x), .y = @floatCast(local_player_input.rot_y) }, this.camera_last_frame_rotations).div_scalar(delta_time);

        this.camera_rotational_velocity = this.camera_rotational_velocity.lerp(estimated_rotational_velocity.mult_scalar(0.003), 13.0 * delta_time);

        const shake_multiplier: f32 = 1.0;
        // if (zeng.get_mouse_button(.right)) shake_multiplier = 0.1;
        const offset_player_step_phase = this.player_step_phase + 0.25;

        var bob_lerp_target: zeng.vec3 = .ZERO;
        if (player_c.velocity.length() < 0.0001 or !player_c.grounded) {} else {
            bob_lerp_target = zeng.vec3.mult_scalar(.{ .x = @sin(2.0 * 3.1415 * offset_player_step_phase), .y = @cos(4.0 * 3.1415 * offset_player_step_phase) * 0.1 }, 0.02);
        }
        this.pistol_lerped_bob = this.pistol_lerped_bob.lerp(bob_lerp_target, 10 * delta_time);
        var pistol_lerped_location = pistol_local_position.lerp(pistol_local_position2, this.pistol_lerp_amount);
        const pistol_effect: zeng.vec3 = zeng.vec3.ZERO.add(.{ .x = this.camera_rotational_velocity.x, .y = -this.camera_rotational_velocity.y }).add(zeng.vec3.mult_scalar(.{ .x = util.perlin(this.pistol_noise_position), .y = util.perlin(this.pistol_noise_position.add(.{ .y = 1.0 })), .z = util.perlin(this.pistol_noise_position.add(.{ .y = 2.0 })) }, 0.01 * shake_multiplier)).add(this.pistol_lerped_bob).mult_scalar(pistol_effect_multiplier);
        pistol_lerped_location = pistol_lerped_location.add(pistol_effect);

        this.pistol_noise_position = this.pistol_noise_position.add(zeng.vec2.mult_scalar(.{ .x = 1.0 }, 0.25 * delta_time));
        this.camera_shake_position = this.camera_shake_position.add(zeng.vec2.mult_scalar(.{ .x = 1.0 }, 20.0 * delta_time));

        this.camera_last_frame_rotations = .{ .x = @floatCast(local_player_input.rot_x), .y = @floatCast(local_player_input.rot_y) };
        this.camera_last_rotational_velocity = estimated_rotational_velocity;

        this.pistol_recoil_rotation_speed -= 200.0 * delta_time;
        this.pistol_recoil_rotation += this.pistol_recoil_rotation_speed * delta_time;
        this.pistol_recoil_rotation = @max(0, this.pistol_recoil_rotation);

        const new_pistol_mat = zeng.mat_mult(cam_matrix.*, zeng.mat_tran(zeng.mat_axis_angle(zeng.vec3.RIGHT, this.pistol_recoil_rotation), pistol_lerped_location));

        world.get(pistol_entity, zeng.world_matrix).?.* = new_pistol_mat;
        zeng.sync_transforms_children(pistol_entity, world_matrix_q, children_q, local_matrix_q);

        this.camera_recoil -= 0.5 * delta_time;
        if (this.camera_recoil < 0.0) this.camera_recoil = 0.0;
        this.lerped_camera_recoil = zeng.lerp(this.lerped_camera_recoil, this.camera_recoil, 14.0 * delta_time);

        for (world.get(player_entity, zeng.children_component).?.items) |child_item| {
            zeng.sync_transforms_recursive(interpolated_matrix, child_item, world_matrix_q, children_q, local_matrix_q);
        }
    }
};

// play test ready:
// a decent map w/ decent graphics + navmesh
// lag compensation
// zombies with A* pathfinding
// probably a cool hook to get jensen interested

// =========================================================================================

// world_colliders: hashmap(client_id, convex_collider)
// spatial_hash_grid: hashmap(cell, array_hashmap(client_id, void))

// entity stores a spatial_client_component: .{ client_id: client_id_t }

// for a whole scene, mesh primitives are spawned as mesh entities
// those mesh entities keep a list of client_components, which are all the triangle colliders from that mesh
// 1 entity -> 1 mesh_primitive, 1 mesh_primitive -> many collider_data, 1 collider_data -> 1 client so, 1 entity -> many clients

// convex_collider: simply data defining the shape/size of this collider (and maybe include matrix)
// client: a key to a convex_collider entry in the SHG, used to add/remove that collider from the grid

// if convex collider does not contain its own matrix state, then when it is used by the SHG system, the matrix of the entity will need to be fetched via world.get(id, zeng.world_matrix);
// this kind of sucks, but it frees us from having to keep the matrices in sync, and reduces memory usage.
// but it also probably prevents the cache from going fast?

// after its all said and done, importing process will be like this:
// gltf_import will spawn mesh entities that also (depending on import options) have a client_list component, and its mesh triangles will be as colliders in the SHG.
// when the entity moves, grid.update(client_id) should be called for each client_id in client_list
// so when the map is imported, and then resized, we would need to call grid.update(client_id) for each client_id in the client_list of each mesh entity seen when traversing from the root scene entity.

// we will also be able to support dynamic entities as well. prob just need to consolidate ray casting triangles with gjk shapecasting
