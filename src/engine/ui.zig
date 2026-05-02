const std = @import("std");
const zeng = @import("zeng.zig");
const vec2 = zeng.vec2;
const ui = @This();

pub var ui_id_map: ui_id_string_map = undefined;
pub const ui_id_string_map = struct {
    map: std.AutoHashMap([*]const u8, *ui.ui_node),

    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.map = std.AutoHashMap([*]const u8, *ui.ui_node).init(allocator);
    }
    pub fn get(this: *@This(), key: [*]const u8) *ui.ui_node {
        return this.map.get(key).?;
    }
    pub fn put(this: *@This(), key: [*]const u8, value: *ui.ui_node) void {
        return this.map.put(key, value) catch unreachable;
    }
};

pub const box_drawer_t = struct {
    shader_program: u32,
    vao: u32,
    indices_len: c_int,

    pub fn init(this: *@This(), io: std.Io, allocator: std.mem.Allocator) void {
        this.vao, this.indices_len = zeng.loader.create_cornered_square_mesh();
        this.shader_program = zeng.loader.load_shader(io, allocator, "assets/shaders/rectangle_vertex.shader", "assets/shaders/rectangle_fragment.shader");
    }

    pub fn draw_img(self: *const @This(), ctx: zeng.graphics_t, x: f32, y: f32, w: f32, h: f32, _color: zeng.render.color, texture: u32, radius: f32) void {
        zeng.gl.useProgram(self.shader_program);
        zeng.gl.bindVertexArray(self.vao);
        zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, texture);

        const screen_res_location = zeng.gl.getUniformLocation(self.shader_program, "screen_res");
        const pos_location = zeng.gl.getUniformLocation(self.shader_program, "screen_pos");
        const size_location = zeng.gl.getUniformLocation(self.shader_program, "dims");
        const color_location = zeng.gl.getUniformLocation(self.shader_program, "_color");
        const image_bool_location = zeng.gl.getUniformLocation(self.shader_program, "image");
        const radius_location = zeng.gl.getUniformLocation(self.shader_program, "radius");
        zeng.gl.uniform2f(screen_res_location, @floatFromInt(ctx.width), @floatFromInt(ctx.height));
        zeng.gl.uniform2f(pos_location, x, y);
        zeng.gl.uniform2f(size_location, w, h);
        zeng.gl.uniform4f(color_location, _color.r, _color.g, _color.b, _color.a);
        zeng.gl.uniform1i(image_bool_location, @intCast(texture));
        zeng.gl.uniform1f(radius_location, radius);

        zeng.gl.disable(zeng.gl.DEPTH_TEST);
        zeng.gl.drawElements(zeng.gl.TRIANGLES, 6, zeng.gl.UNSIGNED_INT, null);
        zeng.gl.enable(zeng.gl.DEPTH_TEST);

        zeng.gl_log_errors();
    }
    // pub fn draw_text(string: []const u8, ui_ren: *@import("main.zig").text_render_res, x: f32, y: f32) void {
    //     zeng.gl.disable(zeng.gl.DEPTH_TEST);
    //     defer zeng.gl.enable(zeng.gl.DEPTH_TEST);

    //     zeng.gl.useProgram(ui_ren.shader_program);
    //     zeng.gl.bindVertexArray(ui_ren.vao);
    //     zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ui_ren.texture);

    //     zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "dims"), 0.02, 0.05);

    //     var horizontal: usize = 0;
    //     for (string) |char| {
    //         zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos"), @as(f32, @floatFromInt(horizontal)) * 0.038 + x, y);
    //         zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "image_point"), @as(f32, @floatFromInt((char - 32) % 16)), @as(f32, @floatFromInt((char - 32) / 16)));
    //         zeng.gl.drawElements(zeng.gl.TRIANGLES, ui_ren.indices_len, zeng.gl.UNSIGNED_INT, null);
    //         horizontal += 1;
    //     }
    // }
};

pub const ui_node = struct {
    children: ?std.ArrayList(*ui_node) = null,
    growable: ?std.ArrayList(*ui_node) = null,
    width: f32 = 0,
    height: f32 = 0,
    pos: vec2 = .{},
    img: ?u32 = null,
    color: ?zeng.render.color = null,
    align_children_perp: u8 = 0,
    align_children_para: u8 = 0,
    padding: f32 = 0,
    gap: f32 = 0,
    radius: f32 = 0,
    width_size_mode: SizeMode = .fit,
    height_size_mode: SizeMode = .fit,
    pos_absolute: bool = false,
    direction: Direction = .left_to_right,
    id: ?[]const u8 = null,
    text: ?[]const u8 = null,
    data_ptr: ?*anyopaque = null,
};
pub const Direction = enum {
    left_to_right,
    right_to_left,
    top_to_bottom,
    bottom_to_top,
};
pub const SizeMode = enum {
    fit,
    fixed,
    grow,
    match,
};

pub fn ui_hug_children(node: *ui.ui_node) void {
    const d = get_node_direction_value(node);
    for (node.children.?.items) |child| {
        ui_hug_children(child);
    }
    if ((node.width_size_mode == .fit and d == false) or (node.height_size_mode == .fit and d == true)) {
        get_node_dimension_ptr(node, d).* = node.padding * 2;
        for (node.children.?.items, 0..) |child, i| {
            get_node_dimension_ptr(node, d).* += get_node_dimension_ptr(child, d).*;
            if (i >= 1) get_node_dimension_ptr(node, d).* += node.gap;
        }
    }
    if ((node.width_size_mode == .fit and d == true) or (node.height_size_mode == .fit and d == false)) {
        get_node_dimension_ptr(node, !d).* = 0;
        for (node.children.?.items) |child| {
            get_node_dimension_ptr(node, !d).* = @max(get_node_dimension_ptr(node, !d).*, get_node_dimension_ptr(child, !d).*);
        }
        get_node_dimension_ptr(node, !d).* += node.padding * 2;
    }
}
pub fn ui_grow(node: *ui.ui_node) void {
    const d = get_node_direction_value(node);
    if (node.growable.?.items.len > 0) {
        var remaining_width = get_node_dimension_ptr(node, d).*;
        remaining_width -= 2 * node.padding;

        for (node.children.?.items) |child| {
            remaining_width -= get_node_dimension_ptr(child, d).*;
        }
        remaining_width -= @as(f32, @floatFromInt(@max(1, node.children.?.items.len) - 1)) * node.gap;

        while (remaining_width > 0.00001) {
            var smallest: f32 = get_node_dimension_ptr(node.growable.?.items[0], d).*;
            var second_smallest = std.math.floatMax(f32);
            var width_to_add = remaining_width;

            for (node.growable.?.items) |child| {
                if (get_node_dimension_ptr(child, d).* < smallest) {
                    second_smallest = smallest;
                    smallest = get_node_dimension_ptr(child, d).*;
                }
                if (get_node_dimension_ptr(child, d).* > smallest) {
                    second_smallest = @min(second_smallest, get_node_dimension_ptr(child, d).*);
                    width_to_add = second_smallest - smallest;
                }
            }

            width_to_add = @min(width_to_add, remaining_width / @as(f32, @floatFromInt(node.growable.?.items.len)));

            for (node.growable.?.items) |child| {
                if (get_node_dimension_ptr(child, d).* == smallest) {
                    get_node_dimension_ptr(child, d).* += width_to_add;
                    remaining_width -= width_to_add;
                }
            }
        }
    }
    var remaining_height = get_node_dimension_ptr(node, !d).*;
    remaining_height -= 2 * node.padding;

    // handle matching aspect ratio
    if (node.width_size_mode == .match and node.height_size_mode != .match) node.width = node.height;
    if (node.width_size_mode != .match and node.height_size_mode == .match) node.height = node.width;
    if (node.width_size_mode == .match and node.height_size_mode == .match) unreachable;

    for (node.children.?.items) |child| {
        if (node_size_mode_direction_value(child, !d) == .grow) {
            get_node_dimension_ptr(child, !d).* += remaining_height - get_node_dimension_ptr(child, !d).*;
        }
        ui_grow(child);
    }
}
pub fn ui_pos(pos: vec2, node: *ui.ui_node) void {
    const d = get_node_direction_value(node);
    if (!node.pos_absolute) node.pos = pos;
    const padded_size = vec2{ .x = node.width - node.padding * 2, .y = node.height - node.padding * 2 };

    var total: f32 = 0;
    for (node.children.?.items, 0..) |child, i| {
        if (i > 0) total += node.gap;
        total += get_node_dimension_ptr(child, d).*;
    }
    var centered_pos_0: f32 = 0;
    if (node.align_children_para == 1) centered_pos_0 = (node_position_coord_value(padded_size, d) - total) / 2;
    if (node.align_children_para == 2) centered_pos_0 = (node_position_coord_value(padded_size, d) - total);
    const _offset: f32 = node.padding + centered_pos_0;
    var offset: f32 = _offset;

    for (node.children.?.items) |child| {
        var centered_pos: f32 = 0;
        if (node.align_children_perp == 1) {
            centered_pos = (node_position_coord_value(padded_size, !d) - get_node_dimension_ptr(child, !d).*) / 2;
        } else if (node.align_children_perp == 2) {
            centered_pos = node_position_coord_value(padded_size, !d) - get_node_dimension_ptr(child, !d).*;
        }

        var v: vec2 = undefined;
        vector_coord_ptr(&v, d).* = node_position_coord_ptr(node, d).* + offset;
        vector_coord_ptr(&v, !d).* = node_position_coord_ptr(node, !d).* + centered_pos + node.padding;
        ui_pos(v, child);

        offset += get_node_dimension_ptr(child, d).* + node.gap;
    }
}
pub fn ui_draw(drawer: *const box_drawer_t, ctx: zeng.graphics_t, node: *ui.ui_node, mesh: zeng.mesh, font: font_info) void {
    const color = node.color orelse zeng.render.color.WHITE;

    drawer.draw_img(ctx, node.pos.x, node.pos.y, node.width, node.height, color, node.img orelse 0, node.radius);
    if (node.text) |t| {
        // zeng.render.draw_text(t, text_renderer, node.pos.x, node.pos.y, ctx);
        zeng.render.draw_text(t, font, mesh, node.pos.x, node.pos.y - 5, 0.42, ctx);
    }

    if (node.children != null) {
        for (node.children.?.items) |child| {
            ui_draw(drawer, ctx, child, mesh, font);
        }
    }
}
pub fn ui_layout(pos: vec2, node: *ui.ui_node) void {
    ui_hug_children(node);
    ui_grow(node);
    ui_pos(pos, node);
}

// helper functions to aid the logic when x and y are handled the same way
pub fn get_node_dimension_ptr(node: *ui.ui_node, d: bool) *f32 {
    return if (d) &node.height else &node.width;
}
pub fn get_node_direction_value(node: *ui.ui_node) bool {
    if (node.direction == .bottom_to_top or node.direction == .top_to_bottom) return true;
    return false;
}
pub fn node_position_coord_ptr(node: *ui.ui_node, d: bool) *f32 {
    return if (d) &node.pos.y else &node.pos.x;
}
pub fn node_position_coord_value(v: vec2, d: bool) f32 {
    return if (d) v.y else v.x;
}
pub fn vector_coord_ptr(v: *vec2, d: bool) *f32 {
    return if (d) &v.y else &v.x;
}
pub fn node_size_mode_direction_value(node: *ui.ui_node, d: bool) ui.SizeMode {
    return if (d) node.height_size_mode else node.width_size_mode;
}

pub fn mouse_over(node: *const ui.ui_node, mx: f32, my: f32) bool {
    return (mx > node.pos.x and mx < node.pos.x + node.width and my > node.pos.y and my < node.pos.y + node.height);
}
pub fn recursive_mouse_over(node: *ui.ui_node, mx: f32, my: f32) ?*ui.ui_node {
    for (node.children.?.items) |a| {
        const rec = recursive_mouse_over(a, mx, my);
        if (rec != null) return rec;
    }

    return if (mouse_over(node, mx, my)) node else null;
}

pub var global_node_allocator: std.mem.Allocator = undefined;
pub fn n(node: ui.ui_node, children: []const *ui.ui_node) *ui.ui_node {
    const ptr = global_node_allocator.create(ui.ui_node) catch unreachable;
    ptr.* = node;
    ptr.children = std.ArrayList(*ui.ui_node).initCapacity(global_node_allocator, children.len) catch unreachable;
    ptr.growable = std.ArrayList(*ui.ui_node).initCapacity(global_node_allocator, children.len) catch unreachable;

    if (ptr.id != null) {
        ui.ui_id_map.put(ptr.id.?.ptr, ptr);
    }

    for (children) |child| {
        ptr.children.?.append(global_node_allocator, child) catch unreachable;
        if ((get_node_direction_value(ptr) == false and child.width_size_mode == .grow) or (get_node_direction_value(ptr) == true and child.height_size_mode == .grow)) ptr.growable.?.append(global_node_allocator, child) catch unreachable;
    }
    return ptr;
}

pub const font_character_info = struct {
    id: usize,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    xoffset: f32,
    yoffset: f32,
    xadvance: f32,
};
pub const font_info = struct {
    character_infos: [95]font_character_info,
    tex: u32,
    tex_width: usize,
    tex_height: usize,
    shader_program: u32,
};
pub fn parse_font_descriptor(path: []const u8, io: std.Io, alloactor: std.mem.Allocator) font_info {
    var buffer: [256]u8 = undefined;
    const fnt_file_path = std.fmt.bufPrint(buffer[0..], "{s}.fnt", .{path}) catch unreachable;
    const data = zeng.loader.get_file_bytes(fnt_file_path, alloactor, io);

    const png_file_path = std.fmt.bufPrint(buffer[0..], "{s}.png\x00", .{path}) catch unreachable;

    var curr: usize = 0;

    const scaleW = parse_helper("scaleW=", data, &curr, usize).?;
    const scaleH = parse_helper("scaleH=", data, &curr, usize).?;

    var fcis: [95]font_character_info = undefined;

    var count: usize = 0;
    while (true) {
        defer count += 1;
        const id = parse_helper("char id=", data, &curr, usize) orelse break;
        const x = parse_helper("x=", data, &curr, usize) orelse break;
        const y = parse_helper("y=", data, &curr, usize) orelse break;
        const width = parse_helper("width=", data, &curr, usize) orelse break;
        const height = parse_helper("height=", data, &curr, usize) orelse break;
        const xoffset = parse_helper("xoffset=", data, &curr, f32) orelse break;
        const yoffset = parse_helper("yoffset=", data, &curr, f32) orelse break;
        const xadvance = parse_helper("xadvance=", data, &curr, f32) orelse break;

        fcis[count] = font_character_info{ .id = id, .x = x, .y = y, .width = width, .height = height, .xoffset = xoffset, .yoffset = yoffset, .xadvance = xadvance };
    }

    return font_info{ .character_infos = fcis, .tex = zeng.loader.load_texture_options(png_file_path, .{ .min_filter = .linear, .mag_filter = .linear }), .shader_program = zeng.loader.load_shader(io, std.heap.c_allocator, "assets/shaders/sdf_text_vertex.shader", "assets/shaders/sdf_text_fragment.shader"), .tex_width = scaleW, .tex_height = scaleH };
}

pub fn parse_helper(str: []const u8, data: []const u8, curr: *usize, T: type) ?T {
    var thing_start: ?usize = null;
    var thing_end: ?usize = null;
    while (curr.* < data.len - str.len) {
        defer curr.* += 1;
        if (std.mem.eql(u8, data[curr.* .. curr.* + str.len], str)) {
            curr.* += str.len;
            thing_start = curr.*;
            break;
        }
    }

    while (curr.* < data.len) {
        defer curr.* += 1;
        if (data[curr.*] == ' ') {
            thing_end = curr.*;
            break;
        }
    }

    if (@typeInfo(T) == .float) {
        return std.fmt.parseFloat(T, data[thing_start orelse return null .. thing_end orelse return null]) catch unreachable;
    } else {
        return std.fmt.parseInt(T, data[thing_start orelse return null .. thing_end orelse return null], 10) catch unreachable;
    }
}
