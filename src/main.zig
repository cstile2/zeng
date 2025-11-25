const std = @import("std");
const zeng = @import("zeng");
const ecs = zeng.ecs;
const rpc = zeng.rpc;
const phy = zeng.phy;
const aud = zeng.aud;
const util = zeng.utils;
const net = zeng.net;
const gl = zeng.gl;
const c = zeng.c;
const vec2 = zeng.vec2;

pub const __ui_box_module = struct {
    const color = zeng.render.color;
    shader_program: u32,
    vao: u32,
    indices_len: c_int,

    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.vao, this.indices_len = zeng.loader.create_square_mesh2();
        this.shader_program = zeng.loader.load_shader(allocator, "assets/shaders/rectangle_vertex.shader", "assets/shaders/rectangle_fragment.shader");
    }

    pub fn draw_img(self: *const @This(), ctx: zeng.__graphics_module, x: f32, y: f32, w: f32, h: f32, _color: color, texture: u32, radius: f32) void {
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

var global_id_map: id_string_map = undefined;
pub const id_string_map = struct {
    map: std.AutoHashMap([*]const u8, *Node),

    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.map = std.AutoHashMap([*]const u8, *Node).init(allocator);
    }
    pub fn get(this: *@This(), key: [*]const u8) *Node {
        return this.map.get(key).?;
    }
    pub fn put(this: *@This(), key: [*]const u8, value: *Node) void {
        return this.map.put(key, value) catch unreachable;
    }
};

pub const mouse_state_t = struct {
    pos: vec2,
};
pub const Node = struct {
    children: ?std.ArrayList(*Node) = null,
    growable: ?std.ArrayList(*Node) = null,
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

pub fn dim(node: *Node, d: bool) *f32 {
    return if (d) &node.height else &node.width;
}
pub fn get_dim(node: *Node) bool {
    if (node.direction == .bottom_to_top or node.direction == .top_to_bottom) return true;
    return false;
}
pub fn pim(node: *Node, d: bool) *f32 {
    return if (d) &node.pos.y else &node.pos.x;
}
pub fn vim(v: vec2, d: bool) f32 {
    return if (d) v.y else v.x;
}
pub fn Vim(v: *vec2, d: bool) *f32 {
    return if (d) &v.y else &v.x;
}
pub fn mim(node: *Node, d: bool) SizeMode {
    return if (d) node.height_size_mode else node.width_size_mode;
}

pub fn ui_hug_children(node: *Node) void {
    const d = get_dim(node);
    for (node.children.?.items) |child| {
        ui_hug_children(child);
    }
    if ((node.width_size_mode == .fit and d == false) or (node.height_size_mode == .fit and d == true)) {
        dim(node, d).* = node.padding * 2;
        for (node.children.?.items, 0..) |child, i| {
            dim(node, d).* += dim(child, d).*;
            if (i >= 1) dim(node, d).* += node.gap;
        }
    }
    if ((node.width_size_mode == .fit and d == true) or (node.height_size_mode == .fit and d == false)) {
        dim(node, !d).* = 0;
        for (node.children.?.items) |child| {
            dim(node, !d).* = @max(dim(node, !d).*, dim(child, !d).*);
        }
        dim(node, !d).* += node.padding * 2;
    }
}
pub fn ui_grow(node: *Node) void {
    const d = get_dim(node);
    if (node.growable.?.items.len > 0) {
        var remaining_width = dim(node, d).*;
        remaining_width -= 2 * node.padding;

        for (node.children.?.items) |child| {
            remaining_width -= dim(child, d).*;
        }
        remaining_width -= @as(f32, @floatFromInt(@max(1, node.children.?.items.len) - 1)) * node.gap;

        while (remaining_width > 0.00001) {
            var smallest: f32 = dim(node.growable.?.items[0], d).*;
            var second_smallest = std.math.floatMax(f32);
            var width_to_add = remaining_width;

            for (node.growable.?.items) |child| {
                if (dim(child, d).* < smallest) {
                    second_smallest = smallest;
                    smallest = dim(child, d).*;
                }
                if (dim(child, d).* > smallest) {
                    second_smallest = @min(second_smallest, dim(child, d).*);
                    width_to_add = second_smallest - smallest;
                }
            }

            width_to_add = @min(width_to_add, remaining_width / @as(f32, @floatFromInt(node.growable.?.items.len)));

            for (node.growable.?.items) |child| {
                if (dim(child, d).* == smallest) {
                    dim(child, d).* += width_to_add;
                    remaining_width -= width_to_add;
                }
            }
        }
    }
    var remaining_height = dim(node, !d).*;
    remaining_height -= 2 * node.padding;

    // handle matching aspect ratio
    if (node.width_size_mode == .match and node.height_size_mode != .match) node.width = node.height;
    if (node.width_size_mode != .match and node.height_size_mode == .match) node.height = node.width;
    if (node.width_size_mode == .match and node.height_size_mode == .match) unreachable;

    for (node.children.?.items) |child| {
        if (mim(child, !d) == .grow) {
            dim(child, !d).* += remaining_height - dim(child, !d).*;
        }
        ui_grow(child);
    }
}
pub fn ui_pos(pos: vec2, node: *Node) void {
    const d = get_dim(node);
    if (!node.pos_absolute) node.pos = pos;
    const padded_size = vec2{ .x = node.width - node.padding * 2, .y = node.height - node.padding * 2 };

    var total: f32 = 0;
    for (node.children.?.items, 0..) |child, i| {
        if (i > 0) total += node.gap;
        total += dim(child, d).*;
    }
    var centered_pos_0: f32 = 0;
    if (node.align_children_para == 1) centered_pos_0 = (vim(padded_size, d) - total) / 2;
    if (node.align_children_para == 2) centered_pos_0 = (vim(padded_size, d) - total);
    const _offset: f32 = node.padding + centered_pos_0;
    var offset: f32 = _offset;

    for (node.children.?.items) |child| {
        var centered_pos: f32 = 0;
        if (node.align_children_perp == 1) {
            centered_pos = (vim(padded_size, !d) - dim(child, !d).*) / 2;
        } else if (node.align_children_perp == 2) {
            centered_pos = vim(padded_size, !d) - dim(child, !d).*;
        }

        var v: vec2 = undefined;
        Vim(&v, d).* = pim(node, d).* + offset;
        Vim(&v, !d).* = pim(node, !d).* + centered_pos + node.padding;
        ui_pos(v, child);

        offset += dim(child, d).* + node.gap;
    }
}
pub fn ui_draw(drawer: *const __ui_box_module, ctx: zeng.__graphics_module, node: *Node, text_renderer: *zeng.text_render_res) void {
    const color = node.color orelse zeng.render.color.WHITE;

    drawer.draw_img(ctx, node.pos.x, node.pos.y, node.width, node.height, color, node.img orelse 0, node.radius);
    if (node.text) |t| {
        zeng.render.draw_text(t, text_renderer, node.pos.x, node.pos.y, ctx);
    }

    if (node.children != null) {
        for (node.children.?.items) |child| {
            ui_draw(drawer, ctx, child, text_renderer);
        }
    }
}
pub fn ui_layout(pos: vec2, node: *Node) void {
    ui_hug_children(node);
    ui_grow(node);
    ui_pos(pos, node);
}

pub fn mouse_over(node: *const Node, s: *const mouse_state_t) bool {
    return (s.pos.x > node.pos.x and s.pos.x < node.pos.x + node.width and s.pos.y > node.pos.y and s.pos.y < node.pos.y + node.height);
}
pub fn recursive_mouse_over(node: *Node, s: *const mouse_state_t) ?*Node {
    for (node.children.?.items) |a| {
        const rec = recursive_mouse_over(a, s);
        if (rec != null) return rec;
    }

    return if (mouse_over(node, s)) node else null;
}

var global_node_allocator: std.mem.Allocator = undefined;
pub fn n(node: Node, children: []const *Node) *Node {
    const ptr = global_node_allocator.create(Node) catch unreachable;
    ptr.* = node;
    ptr.children = std.ArrayList(*Node).initCapacity(global_node_allocator, children.len) catch unreachable;
    ptr.growable = std.ArrayList(*Node).initCapacity(global_node_allocator, children.len) catch unreachable;

    if (ptr.id != null) {
        global_id_map.put(ptr.id.?.ptr, ptr);
    }

    for (children) |child| {
        ptr.children.?.append(global_node_allocator, child) catch unreachable;
        if ((get_dim(ptr) == false and child.width_size_mode == .grow) or (get_dim(ptr) == true and child.height_size_mode == .grow)) ptr.growable.?.append(global_node_allocator, child) catch unreachable;
    }
    return ptr;
}
pub fn page_widget(wooden_tex: u32, noise_tex: u32, w: f32, h: f32, split_t: f32) *Node {
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
pub fn button_widget(w: f32, h: f32, id: ?[]const u8) *Node {
    return n(.{ .width = w, .width_size_mode = .fixed, .height = h, .height_size_mode = .fixed, .radius = 4, .color = zeng.render.color.GRAY, .id = id }, &.{});
}
pub fn menu_widget(tex: u32) *Node {
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
pub fn card_widget(cover: u32) *Node {
    return n(.{ .width_size_mode = .fixed, .height_size_mode = .fixed, .width = 100, .height = 140, .color = zeng.render.color.GRAY, .direction = .bottom_to_top, .radius = 8, .padding = 4, .gap = 4 }, &.{
        n(.{ .width_size_mode = .grow, .height_size_mode = .match, .img = cover, .radius = 8 }, &.{}),
        n(.{ .width_size_mode = .grow, .height_size_mode = .fixed, .height = 10, .color = zeng.render.color.BLACK, .radius = 3 }, &.{}),
    });
}

pub fn parameter_widget(str_dyn: *Parameter.StringDynamic, pos: vec2) *Node {
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

var global_num: usize = 0;
pub fn block_widget(B: *CodeBlock, pos: vec2) *Node {
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

    var children = std.ArrayList(*Node).initCapacity(zeng.global_allocator, 0) catch unreachable;
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

pub const graph_render = struct {
    pub const edge = struct {
        from: usize,
        to: usize,
    };
    pub const render_info = struct {
        nodes: []vec2,
        edges: []graph_render.edge,
        map: std.AutoHashMap(*const Node, usize),
    };
};
pub fn increment_and_return_unincremented(x: *usize) usize {
    const temp = x.*;
    x.* += 1;
    return temp;
}
pub fn visualize_graph(allocator: std.mem.Allocator, start: *Node) graph_render.render_info {
    var visited = std.AutoHashMap(*const Node, void).init(allocator);
    defer visited.deinit();

    var ptr_to_idx = std.AutoHashMap(*const Node, usize).init(allocator);
    defer ptr_to_idx.deinit();

    var nodes = std.ArrayList(vec2).initCapacity(allocator, 0) catch unreachable;
    // defer nodes.deinit(allocator);

    var edges = std.ArrayList(graph_render.edge).initCapacity(allocator, 0) catch unreachable;
    // defer edges.deinit(allocator);

    var stack = std.ArrayList(*const Node).initCapacity(allocator, 0) catch unreachable;
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

pub fn example(datablob: *zeng.resources_t) void {
    const _player = void{};
    const world = datablob.get("world", ecs.world);
    const time = datablob.get("time", zeng.time_res);

    const run_animation = datablob.get("assets/gltfguy.gltf/run", zeng.loader.animation);
    const walk_animation = datablob.get("assets/gltfguy.gltf/walk", zeng.loader.animation);

    const anim = world.get(world.animation_controller, zeng.animation_component).?;
    const skel = world.get(_player.animation_controller, zeng.skeleton).?;
    const blend = _player.velocity.div(3.0).clamp(1.0).length();

    anim.time += time.fixed_dt / zeng.lerp(run_animation.duration, walk_animation.duration, blend);
    while (anim.time > 1.0) {
        anim.time -= 1.0;
    }

    const pose: zeng.skeleton_pose = undefined;
    zeng.get_animation_pose_with_weight(run_animation, anim.time, pose, blend);
    zeng.add_animation_pose_with_weight(walk_animation, anim.time, pose, 1.0 - blend);
    zeng.normalize_pose_quaternions(pose);
    zeng.apply_pose_to_skeleton(skel, pose);
}

const hot_reload = @import("hot_reload");
const DLL_LOCATION = "zig-out/bin/hot_reload_copy.dll";
var HINSTANCE: ?[*c]c.struct_HINSTANCE__ = null;
pub fn hot_reload_api() ?hot_reload.hot_reload_procedures {
    const game_lib = zeng.c.LoadLibraryA(DLL_LOCATION);
    HINSTANCE = game_lib;
    if (game_lib == null) return null;
    const ptr = c.GetProcAddress(game_lib, "get_game_api");
    const dynamic_get_game_api: *const @TypeOf(hot_reload.get_game_api) = @ptrCast(ptr orelse return null);
    std.debug.print("script was hot reloaded!\n", .{});
    return dynamic_get_game_api().*;
}
pub fn HOT_RELOAD_API() !hot_reload.hot_reload_procedures {
    var src_dir = std.fs.cwd().openDir(".", .{}) catch unreachable;
    defer src_dir.close();

    if (HINSTANCE != null) {
        _ = zeng.c.FreeLibrary(HINSTANCE.?);
    }

    try std.fs.Dir.copyFile(src_dir, "zig-out/bin/hot_reload.dll", src_dir, "zig-out/bin/hot_reload_copy.dll", .{});
    return hot_reload_api() orelse unreachable;
}
fn get_last_write_time(path: []const u8) !i128 {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const info = try file.stat();
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
pub fn create_script_zig_file(B: *CodeBlock, allocator: std.mem.Allocator) void {
    const dir = std.fs.cwd().openDir("dynamic", .{}) catch unreachable;
    std.fs.Dir.copyFile(dir, "script_master.zig", dir, "script_copy.zig", .{}) catch unreachable;

    var buff: [4000]u8 = undefined;
    var buff_len: usize = 0;
    B.print_code.?(B, buff[0..], &buff_len);

    insert_text_into_file("dynamic/script_copy.zig", 0, buff[0..buff_len], allocator) catch unreachable;
    compile_dll(allocator) catch unreachable;
}

// const Block = union(enum) {
//     Add: struct { a: f64, b: f64 },
//     Subtract: struct { a: f64, b: f64 },
//     Multiply: struct { a: f64, b: f64 },
//     String: []const u8,
// };
// const BlockNode = struct {
//     block: Block,
//     children: ?[]const *BlockNode,
//     height: f32 = 0,
// };

const Parameter = union(enum) {
    pub const StringDynamic = struct { str: []u8, len: usize };
    Label: []const u8,
    String: StringDynamic,
    code_block: *CodeBlock,
};
const BlockShape = enum {
    start,
    instruction,
};
const ExecuteFN = *const fn (*const anyopaque, *zeng.resources_t, *ScriptContext) void;
const CodeBlock = struct {
    params: []Parameter,
    __params_rects: []Rect,
    children: std.ArrayList(*CodeBlock),
    shape: BlockShape,
    __height: f32 = 0,
    __rect: Rect = Rect{},

    execute: ?ExecuteFN = null,
    print_code: ?*const fn (*CodeBlock, []u8, *usize) void,
    color: zeng.render.color,
};
const Value = union(enum) {
    player: *zeng.Player.player,
    int: *usize,
    float: *f32,
};
const ScriptContext = struct {
    map: std.StringHashMap(Value),
    arena: std.heap.ArenaAllocator,
    arena_allocator: std.mem.Allocator,
    pub fn init(this: *@This(), allocator: std.mem.Allocator) void {
        this.arena = std.heap.ArenaAllocator.init(allocator);
        this.arena_allocator = this.arena.allocator();
        this.map = std.StringHashMap(Value).init(allocator);
    }
    pub fn deinit(this: *@This()) void {
        this.arena.deinit();
    }
    pub fn add(this: *@This(), str: []const u8, v: Value) void {
        this.map.put(str, v) catch unreachable;
    }
    pub fn get(this: *@This(), str: []const u8) Value {
        return this.map.get(str).?;
    }
    pub fn reset(this: *@This()) void {
        _ = this.arena.reset(.free_all);
        this.map.clearRetainingCapacity();
    }
};
const Rect = struct {
    pos: vec2 = vec2.ZERO,
    size: vec2 = vec2.ZERO,
};

pub fn get_height(B: *CodeBlock, thickness: f32) f32 {
    var height: f32 = if (B.children.items.len > 0) 2 * thickness else thickness;

    for (B.children.items) |ch| {
        height += get_height(ch, thickness);
    }
    B.__height = height;

    return height;
}

pub fn draw(B: *CodeBlock, thickness: f32, __ui_box: *__ui_box_module, __graphics: *zeng.__graphics_module, __resources: *zeng.resources_t, pos: vec2) void {
    var LENGTH: f32 = 0;
    for (B.params) |param| {
        if (param == .Label) {
            LENGTH += 12 * @as(f32, @floatFromInt(param.Label.len + 1));
        } else if (param == .String) {
            LENGTH += 12 * @as(f32, @floatFromInt(param.String.len + 1));
        }
    }
    __ui_box.draw_img(__graphics.*, pos.x, pos.y, LENGTH, thickness, B.color, 0, 5);
    B.__rect.pos = pos;
    B.__rect.size = .{ .x = LENGTH, .y = thickness };

    if (B.children.items.len > 0) {
        __ui_box.draw_img(__graphics.*, pos.x, pos.y, thickness, B.__height, B.color, 0, 5);
        __ui_box.draw_img(__graphics.*, pos.x, pos.y + B.__height - thickness, LENGTH, thickness, B.color, 0, 5);

        var _y: f32 = thickness;
        for (B.children.items) |child| {
            const np = zeng.vec2{ .x = pos.x + thickness, .y = pos.y + _y };
            draw(child, thickness, __ui_box, __graphics, __resources, np);
            _y += child.__height;
        }
    }

    var cursor: f32 = 0;
    for (B.params, 0..) |param, i| {
        if (param == .Label) {
            zeng.render.draw_text(param.Label, __resources.get(zeng.text_render_res), pos.x + cursor, pos.y, __graphics.*);
            cursor += 12 * @as(f32, @floatFromInt(param.Label.len + 1));
        } else if (param == .String) {
            __ui_box.draw_img(__graphics.*, pos.x + cursor, pos.y, 12 * @as(f32, @floatFromInt(param.String.len)), thickness, zeng.render.color.GRAY, 0, 5);
            B.__params_rects[i].pos = .{ .x = pos.x + cursor, .y = pos.y };
            B.__params_rects[i].size = .{ .x = 12 * @as(f32, @floatFromInt(param.String.len)), .y = thickness };
            zeng.render.draw_text(param.String.str[0..param.String.len], __resources.get(zeng.text_render_res), pos.x + cursor, pos.y, __graphics.*);
            cursor += 12 * @as(f32, @floatFromInt(param.String.len + 1));
        }
    }
}

pub fn CODEBLOCKEXECUTE_start(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));
    std.debug.print("start running...\n", .{});
    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}
pub fn CODEBLOCKEXECUTE_test(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));

    for (B.params) |param| {
        if (param == .String) {
            std.debug.print("string: '{s}'\n", .{param.String.str[0..param.String.len]});
        }
    }
    const fet = res.get(zeng.resource_fetcher);
    const q = fet.fresh_query(.{zeng.Player.player});
    var it = q.iterator();
    const p: *zeng.Player.player = it.next().?[0];
    p.velocity.y += 10.0;
    p.grounded = false;

    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}
pub fn CODEBLOCKEXECUTE_hello_world(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));

    std.debug.print("HELLO WORLD!\n", .{});

    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}
pub fn CODERBLOCKEXECUTE_make_var(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));

    const ptr = ctx.arena_allocator.create(f32) catch unreachable;
    ptr.* = 3.14159;
    ctx.add("pi", .{ .float = ptr });

    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}
pub fn CODERBLOCKEXECUTE_print_var(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));

    const val = ctx.get("pi");
    std.debug.print("value: {}\n", .{val.float.*});

    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}
pub fn CODEBLOCKEXECUTE_get_player(_B: *const anyopaque, res: *zeng.resources_t, ctx: *ScriptContext) void {
    const B: *const CodeBlock = @ptrCast(@alignCast(_B));

    const fet = res.get(zeng.resource_fetcher);
    const q = fet.fresh_query(.{zeng.Player.player});
    var it = q.iterator();
    const p: *zeng.Player.player = it.next().?[0];
    ctx.add("player", .{ .player = p });

    for (B.children.items) |child| {
        (child.execute orelse continue)(child, res, ctx);
    }
}

pub fn CODEBLOCKPRINT_event(_B: *const CodeBlock, buffer: []u8, buffer_len: *usize) void {
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

pub fn CODEBLOCKPRINT_if(_B: *const CodeBlock, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "if ({s}) {{", .{_B.params[1].String.str[0.._B.params[1].String.len]}) catch unreachable;
    buffer_len.* += new.len;

    for (_B.children.items) |C| {
        (C.print_code orelse continue)(C, buffer, buffer_len);
    }

    const new2 = std.fmt.bufPrint(buffer[buffer_len.*..], "}}\n", .{}) catch unreachable;
    buffer_len.* += new2.len;
}

pub fn CODEBLOCKPRINT_blank(_B: *const CodeBlock, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "{s}", .{_B.params[0].String.str[0.._B.params[0].String.len]}) catch unreachable;
    buffer_len.* += new.len;
}

pub fn CODEBLOCKPRINT_jump(_B: *const CodeBlock, buffer: []u8, buffer_len: *usize) void {
    const new = std.fmt.bufPrint(buffer[buffer_len.*..], "player.velocity.y += ", .{}) catch unreachable;
    buffer_len.* += new.len;

    const new2 = std.fmt.bufPrint(buffer[buffer_len.*..], "{s};\n", .{_B.params[1].String.str[0.._B.params[1].String.len]}) catch unreachable;
    buffer_len.* += new2.len;
}

pub fn micro_mouse_over(r: Rect, m: vec2) bool {
    return (m.x > r.pos.x and m.x < r.pos.x + r.size.x and m.y > r.pos.y and m.y < r.pos.y + r.size.y);
}

pub fn get_mouseover(B: *CodeBlock, parent: ?*CodeBlock, index: ?usize, mouse_pos: zeng.vec2) struct { ?*CodeBlock, ?usize, ?*CodeBlock, ?*Parameter } {
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

pub fn CB(allocator: std.mem.Allocator, params: []const Parameter, color: zeng.render.color, children: []const *CodeBlock, shape: BlockShape, execute_fn: ?ExecuteFN, print_fn: ?*const @TypeOf(CODEBLOCKPRINT_event)) *CodeBlock {
    const ptr = allocator.create(CodeBlock) catch unreachable;
    const _params = allocator.alloc(Parameter, params.len) catch unreachable;
    for (params, _params) |param, *_param| {
        _param.* = param;
    }
    ptr.params = _params;

    ptr.children = std.ArrayList(*CodeBlock).initCapacity(allocator, 0) catch unreachable;
    for (children) |child| {
        ptr.children.append(allocator, child) catch unreachable;
    }

    ptr.shape = shape;
    ptr.execute = execute_fn;
    ptr.print_code = print_fn;
    ptr.color = color;
    ptr.__params_rects = allocator.alloc(Rect, params.len) catch unreachable;
    for (ptr.__params_rects) |*v| {
        v.* = Rect{};
    }

    return ptr;
}

pub fn remove_code_block_ptr(list: *std.ArrayList(*CodeBlock), cb: *CodeBlock) void {
    for (list.items, 0..) |curr, i| {
        if (curr == cb) {
            _ = list.swapRemove(i);
            return;
        }
    }
}

pub fn recurse_children(e_id: ecs.entity_id, world: *ecs.world, tabs: usize) void {
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
        recurse_children(i, world, tabs + 1);
    }
    const children = world.get(e_id, zeng.children) orelse return;
    for (children.items) |i| {
        recurse_children(i, world, tabs + 1);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    global_node_allocator = arena_allocator;

    zeng.global_allocator = allocator;
    zeng.key_press_messages = std.ArrayList(u8).initCapacity(allocator, 0) catch unreachable;
    defer zeng.key_press_messages.deinit(allocator);

    var __graphics: zeng.__graphics_module = undefined;
    __graphics.init();
    var __resources: zeng.resources_t = undefined;
    __resources.init(arena_allocator, &__graphics);
    var __ui_box: __ui_box_module = undefined;
    __ui_box.init(arena_allocator);
    global_id_map.init(arena_allocator);

    var datablob = zeng.Datablob{ .map = std.StringHashMap(*anyopaque).init(allocator) };
    defer datablob.map.deinit();

    datablob.map.put("graphics", &__graphics) catch unreachable;
    datablob.map.put("ui", &__ui_box) catch unreachable;
    __resources.insert_ptr(&datablob);

    var world: ecs.world = ecs.world.init(allocator);
    defer world.deinit() catch unreachable;
    var fet: zeng.resource_fetcher = .{ .world = &world, .res = &__resources, .allocator = arena_allocator };
    __resources.insert_ptr(&fet);

    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});
    zeng.global_world_ptr = &world;

    // CLI
    var is_server: bool = true;
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

    // UDP multiplayer setup (the system simulates network latency and packet loss)
    var main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, is_server) catch unreachable;
    defer zeng.net.undo_setup(main_socket);
    const server_address = net.peer_info_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };
    var tracker = net.packet_ack_tracker_t{};
    __resources.insert_ptr(&tracker);
    datablob.put("main_socket", @ptrCast(&main_socket));

    // engine-wide resources
    const triangle_vao, const triangle_vbo = zeng.loader.create_triangle_mesh();
    const cube_vao, const cube_len = zeng.loader.create_cube_mesh_with_normals();
    var cube_collider_pos, var cube_collider_indices = zeng.loader.create_cube_mesh_collider();
    const sky_shader = zeng.loader.load_shader(allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const rect_shader = zeng.loader.load_shader(allocator, "assets/shaders/protorect_vertex.shader", "assets/shaders/protorect_fragment.shader");
    const static_shader = zeng.loader.load_shader(allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.loader.load_shader(allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.loader.load_shader(allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.loader.load_texture("assets/images/uv_checker.png", true, false);
    const white_tex = zeng.loader.load_texture("assets/images/white.png", true, false);
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .parameter_map = blk: {
        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
        _value.put("albedo_texture", .{ .texture = uv_checker_tex }) catch unreachable;
        _value.put("albedo", .{ .float_3 = zeng.vec3.ONE }) catch unreachable;
        _value.put("metallic", .{ .float_1 = 0.0 }) catch unreachable;
        _value.put("roughness", .{ .float_1 = 0.5 }) catch unreachable;
        break :blk _value;
    } }, .vao_gpu = cube_vao };
    var gun_shot = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/gun_shot.wav", arena_allocator)) catch unreachable;
    datablob.map.put("sounds/gun_shot.wav", &gun_shot) catch unreachable;
    var bell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/bell.wav", arena_allocator)) catch unreachable;
    datablob.map.put("sounds/bell.wav", &bell) catch unreachable;
    var ahem = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/ahem.wav", arena_allocator)) catch unreachable;
    datablob.map.put("sounds/ahem.wav", &ahem) catch unreachable;

    var top_children = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    defer top_children.deinit(arena_allocator);

    const pistol_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "pistol", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(arena_allocator, pistol_entity) catch unreachable;
    const map_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "outdoor_map_6_8_25", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    top_children.append(arena_allocator, map_entity) catch unreachable;
    const cube_entity = world.spawn(.{ cube_mesh, zeng.mat_tran(zeng.mat_identity, .{ .x = -7.0, .y = 2.0 }), zeng.floater_component{} });
    if (!is_server) world.add(zeng.snapshot_interpolator{ .buffer = undefined }, cube_entity);
    const test_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf/people", "KingShiny", skin_shader, static_shader, white_tex, arena_allocator);
    top_children.append(arena_allocator, test_entity) catch unreachable;
    const test_random_skinned_mesh = zeng.find_component_of_type(&world, test_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
    const test_skeleton_entity = world.get(test_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, test_skeleton_entity);
    world.get(test_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(test_entity, zeng.world_matrix).?.*, .{ .x = 5 });
    recurse_children(test_entity, &world, 0);

    const player_entity = zeng.Player.construct_main_server_player(&datablob, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);

    var remote_player_entity: ecs.entity_id = undefined;
    var remote_player_skeleton_entity: ecs.entity_id = undefined;
    if (!is_server) {
        remote_player_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "static_test", skin_shader, static_shader, uv_checker_tex, arena_allocator);
        const remote_player_random_skinned_mesh = zeng.find_component_of_type(&world, remote_player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
        remote_player_skeleton_entity = world.get(remote_player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
        world.add(zeng.animation_component{ .time = 0.0, .current_animation = 0 }, remote_player_skeleton_entity);
        world.get(remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 10.0 });
        world.add(zeng.snapshot_interpolator{ .buffer = undefined }, remote_player_entity);
        top_children.append(arena_allocator, remote_player_entity) catch unreachable;
    }

    __resources.insert(zeng.main_player_res{ .id = player_entity });

    const square_vao, const square_indices_length = zeng.loader.create_square_mesh();
    __resources.insert(zeng.text_render_res{ .shader_program = zeng.loader.load_shader(allocator, "assets/shaders/text_vertex.shader", "assets/shaders/text_fragment.shader"), .texture = zeng.loader.load_texture("assets/images/sdf_font.png", false, false), .vao = __ui_box.vao, .indices_len = square_indices_length });
    __resources.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands{ .random = __resources.get(std.Random.Xoshiro256).random(), .remote_messages_send_queue = undefined, .remote_messages_send_queue_len = 0, .allocator = allocator };
    defer commands.destroy();
    __resources.insert_ptr(&commands);
    __resources.insert(zeng.input_res{ .t_down_last_frame = false });
    __resources.insert_ptr(&__graphics);
    __resources.insert_ptr(&world);
    __resources.insert(zeng.networking_res{ .main_socket = main_socket, .server_address = _server_address, .is_server = is_server });
    __resources.insert(zeng.rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });
    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, zeng.follow_component{ .target = player_entity, .anchor_point = zeng.mat_position(world.get(player_entity, zeng.world_matrix).?.*) } });
    zeng.global_camera_entity = main_camera;
    __resources.insert(zeng.debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = world.get(main_camera, zeng.camera).?.projection_matrix, .inv_camera_matrix = zeng.mat_invert(world.get(main_camera, zeng.world_matrix).?.*) });
    __resources.insert(zeng.main_camera_res{ .id = main_camera });
    world.get(player_entity, zeng.Player.player).?.camera = main_camera;
    zeng.window_resize_handler(__graphics.width, __graphics.height);
    var peer_map = std.AutoHashMap(net.peer_info_t, zeng.client_info).init(allocator);
    defer peer_map.deinit();
    var inverse_peer_map = std.AutoHashMap(ecs.entity_id, net.peer_info_t).init(allocator);
    defer inverse_peer_map.deinit();
    __resources.insert_ptr(&peer_map);
    __resources.insert_ptr(&inverse_peer_map);

    zeng.loader.global_colliders.?.append(arena_allocator, zeng.cpu_mesh{ .indices = cube_collider_indices[0..], .positions = util.convert_float_slice_to_vec_slice(cube_collider_pos[0..]) }) catch unreachable;
    zeng.loader.global_matrices.?.append(arena_allocator, zeng.mat_identity) catch unreachable;
    var colliders = std.ArrayList(phy.collider_info).initCapacity(allocator, 0) catch unreachable;
    defer colliders.deinit(allocator);
    __resources.insert_ptr(&colliders);
    for (zeng.loader.global_colliders.?.items, zeng.loader.global_matrices.?.items) |_mesh, _matrix| {
        var curr_tri: usize = 0;
        while (curr_tri < _mesh.indices.len) {
            defer curr_tri += 3;
            const cool = arena_allocator.create(phy.mesh_triangle_data) catch unreachable;
            cool.positions = _mesh.positions;
            cool.indices = .{ _mesh.indices[curr_tri], _mesh.indices[curr_tri + 1], _mesh.indices[curr_tri + 2] };
            const collider = phy.collider_info{ .matrix = _matrix, .support = phy.mesh_triangle, .tag = .support_based, .data = @ptrCast(cool) };
            colliders.append(allocator, collider) catch unreachable;
        }
    }
    var spatial_hash_grid = std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)).init(allocator);
    defer spatial_hash_grid.deinit();
    __resources.insert_ptr(&spatial_hash_grid);
    phy.construct_spatial_hash_grid(colliders, &spatial_hash_grid, arena_allocator);

    // initialze all the event types
    var str_events = zeng.events([]const u8).init(allocator, false);
    defer str_events.deinit(allocator);
    __resources.insert_ptr(&str_events);
    var tri_events = zeng.events([3]zeng.vec3).init(allocator, false);
    defer tri_events.deinit(allocator);
    __resources.insert_ptr(&tri_events);
    var player_join_events = zeng.events(rpc.player_spawn_message).init(allocator, true);
    defer player_join_events.deinit(allocator);
    __resources.insert_ptr(&player_join_events);
    var snap_events = zeng.events(rpc.state_correction).init(allocator, true);
    defer snap_events.deinit(allocator);
    __resources.insert_ptr(&snap_events);
    var input_events = zeng.events(rpc.input_chunck).init(allocator, true);
    defer input_events.deinit(allocator);
    __resources.insert_ptr(&input_events);
    var client_tick_events = zeng.events(rpc.client_tick).init(allocator, true);
    defer client_tick_events.deinit(allocator);
    __resources.insert_ptr(&client_tick_events);
    var server_tick_offset_events = zeng.events(rpc.server_tick_offset).init(allocator, true);
    defer server_tick_offset_events.deinit(allocator);
    __resources.insert_ptr(&server_tick_offset_events);
    var missed_input_events = zeng.events(rpc.missed_input).init(allocator, true);
    defer missed_input_events.deinit(allocator);
    __resources.insert_ptr(&missed_input_events);
    var world_update_events = zeng.events(rpc.world_update).init(allocator, true);
    defer world_update_events.deinit(allocator);
    __resources.insert_ptr(&world_update_events);
    var hitmarker_events = zeng.events(rpc.hitmarker).init(allocator, true);
    defer hitmarker_events.deinit(allocator);
    __resources.insert_ptr(&hitmarker_events);

    if (!is_server) {
        zeng.net.remote_event(&commands, &tracker, main_socket, server_address, rpc.player_spawn_message{}, .reliable);
    }

    const fixed_rate: f64 = 60.0;
    const fixed_delta: f64 = 1.0 / fixed_rate;
    __resources.insert(zeng.time_res{ .delta_time = 0.006944, .dt = 0.006944, .fixed_delta_time = fixed_delta, .fixed_dt = @floatCast(fixed_delta) });
    __resources.get(zeng.time_res).fixed_dt = @floatCast(fixed_delta);

    var accumulator: f64 = 0.0;
    var tick: isize = 0;

    var client_input_buffer: zeng.ring_buffer(rpc.input_message) = undefined;
    var synced_time: f64 = 0.0;
    var timescale: f64 = 1.0;
    var sim_timescale: f64 = 1.0;
    var buffer_time: f64 = 0.0;
    var buffer_velocity: f64 = 0.0;
    var buffer_cooldown: f64 = 0.0;
    var server_has_responded: bool = false;

    var draw_local_alignment: f64 = 0.0;
    var draw_time_alignment: f64 = 0.0;
    var draw_rtt: f64 = 0.0;
    var draw_resims: usize = 0;
    var interpolated_tick_delta: f64 = 0.0;

    var exponential_rtt: f64 = 1.0;

    // var render_info: ?graph_render.render_info = null;

    var done = false;

    var game_api = HOT_RELOAD_API() catch unreachable;
    var last_write_time = try get_last_write_time("zig-out/bin/hot_reload.dll");

    // var r_pressed_last_frame = false;
    const top = CB(arena_allocator, &.{Parameter{ .Label = "When button clicked (res) (player) (world)" }}, .{ .r = 0.8, .g = 0.2, .b = 0.1 }, &.{
        CB(arena_allocator, &.{ Parameter{ .Label = "Do super jump" }, Parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } } }, .{ .r = 0.1, .g = 0.3, .b = 0.8 }, &.{}, .instruction, CODEBLOCKEXECUTE_test, CODEBLOCKPRINT_jump),
        CB(arena_allocator, &.{Parameter{ .Label = "Print 'HELLO WORLD!'" }}, .{ .r = 0.15, .g = 0.6, .b = 0.1 }, &.{}, .instruction, CODEBLOCKEXECUTE_hello_world, null),
        CB(arena_allocator, &.{ Parameter{ .Label = "If" }, Parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } } }, .RED, &.{}, .instruction, null, CODEBLOCKPRINT_if),
        CB(arena_allocator, &.{Parameter{ .String = .{ .str = arena_allocator.alloc(u8, 255) catch unreachable, .len = 0 } }}, .{ .r = 0.2, .g = 0.2, .b = 0.2 }, &.{}, .instruction, CODEBLOCKEXECUTE_hello_world, CODEBLOCKPRINT_blank),

        // CB(arena_allocator, &.{Parameter{ .Label = "Make var" }}, .{ .r = 0.15, .g = 0.15, .b = 0.15 }, &.{}, .instruction, CODERBLOCKEXECUTE_make_var),
        // CB(arena_allocator, &.{Parameter{ .Label = "Print var" }}, .{ .r = 0.15, .g = 0.2, .b = 0.2 }, &.{}, .instruction, CODERBLOCKEXECUTE_print_var),
    }, .start, CODEBLOCKEXECUTE_start, CODEBLOCKPRINT_event);
    // var dragging_code_block: ?*CodeBlock = null;
    var mouse_down_last_frame = false;
    // var selected_parameter: ?*Parameter = null;

    var top_level_code_blocks = std.ArrayList(*CodeBlock).initCapacity(allocator, 0) catch unreachable;
    defer top_level_code_blocks.deinit(allocator);
    top_level_code_blocks.append(allocator, top) catch unreachable;
    var script_context: ScriptContext = undefined;
    script_context.init(allocator);
    defer script_context.deinit();

    while (true) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&__resources);
        if (zeng.quit) break;
        commands.time += __resources.get(zeng.time_res).delta_time;
        zeng.net.recieve_net_messages(main_socket, &__resources, allocator, &tracker);

        if (zeng.get_mouse_button(.left) and zeng.get_key(.k)) {
            zeng.lock_cursor_to_window(__graphics.hwnd);
            zeng.hide_cursor();
        }
        if (zeng.get_key(.m)) {
            zeng.unlock_cursor();
            zeng.show_cursor();
        }

        const world_matrix_q = fet.fresh_query(.{zeng.world_matrix});
        const children_q = fet.fresh_query(.{zeng.children});
        const local_matrix_q = fet.fresh_query(.{zeng.local_matrix});

        const new_write_time = try get_last_write_time("zig-out/bin/hot_reload.dll");
        if (new_write_time != last_write_time) blk: {
            game_api = HOT_RELOAD_API() catch |err| {
                std.debug.print("error: {}\n", .{err});
                break :blk;
            };
            last_write_time = new_write_time;
        }

        // server communication
        for (server_tick_offset_events.array.items, server_tick_offset_events.addresses.?.items) |server_tick_offset_event, _| {
            const rtt = synced_time - server_tick_offset_event.client_time;
            draw_rtt = rtt;
            exponential_rtt = zeng.lerp(exponential_rtt, rtt, 0.08);

            const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
            draw_time_alignment = time_offset;

            if (@abs(time_offset) > 0.7) {
                // std.debug.print("time jump\n", .{});
                synced_time += time_offset;
            } else {
                timescale = 1.0 + (time_offset / 50.0);
            }
            if (!server_has_responded) {
                buffer_time = 0.4;
            }
            server_has_responded = true;
        }
        server_tick_offset_events.clear(allocator);
        for (input_events.array.items, input_events.addresses.?.items) |input_event, sockaddr| {
            const info = peer_map.getPtr(sockaddr) orelse break;
            for (input_event.arr) |_input_event| {
                if (_input_event.tick >= tick) {
                    info.input_buffer.set(_input_event.tick, _input_event);
                } // else discard late message
            }
        }
        input_events.clear(allocator);
        for (player_join_events.array.items, player_join_events.addresses.?.items) |_, sockaddr| {
            std.debug.print("player connected: \n", .{});

            const new_remote_player_entity = zeng.Player.create_player(&datablob, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator, .{ .net_id = zeng.get_new_netid(), .remote_peer = sockaddr });

            world.get(new_remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(new_remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 60.0 });
            world.get(new_remote_player_entity, zeng.Player.player).?.camera = world.spawn(.{zeng.mat_identity});

            peer_map.put(sockaddr, zeng.client_info{ .input_buffer = zeng.ring_buffer(rpc.input_message){ .arr = undefined }, .player = new_remote_player_entity }) catch unreachable;
            inverse_peer_map.put(new_remote_player_entity, sockaddr) catch unreachable;
        }
        player_join_events.clear(allocator);
        for (client_tick_events.array.items, client_tick_events.addresses.?.items) |client_tick_event, sockaddr| {
            // commands.remote_event(main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
            zeng.net.remote_event(&commands, &tracker, main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
        }
        client_tick_events.clear(allocator);

        // client communication
        for (hitmarker_events.array.items, hitmarker_events.addresses.?.items) |_, _| {
            aud.play_sound(bell, .one_shot);
        }
        hitmarker_events.clear(allocator);
        for (snap_events.array.items, snap_events.addresses.?.items) |snap_event, _| {
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
                zeng.Player.simulate_collision(world.get(player_entity, zeng.Player.player).?, world.get(player_entity, zeng.world_matrix).?, &spatial_hash_grid, &tri_events, __resources.get(zeng.debug_res));
                zeng.Player.simulate_player(world.get(player_entity, zeng.Player.player).?, &im, world.get(player_entity, zeng.world_matrix).?, __resources.get(zeng.time_res));
            }
            draw_resims = @intCast(@max(tick - snap_event.tick, 0));

            zeng.sync_transforms_children(player_entity, world_matrix_q, children_q, local_matrix_q);
        }
        snap_events.clear(allocator);
        for (missed_input_events.array.items, missed_input_events.addresses.?.items) |_, _| {
            if (server_has_responded and buffer_cooldown <= 0.0) {
                buffer_velocity = 0.04;
                buffer_cooldown = 0.3;
            }
        }
        missed_input_events.clear(allocator);
        for (world_update_events.array.items, world_update_events.addresses.?.items) |world_update_event, _| {
            const SI = world.get(remote_player_entity, zeng.snapshot_interpolator).?;
            SI.buffer.set(world_update_event.tick, .{ .position = zeng.mat_position(world_update_event.server_player_matrix), .tick = world_update_event.tick });

            const SI2 = world.get(cube_entity, zeng.snapshot_interpolator).?;
            SI2.buffer.set(world_update_event.tick, .{ .position = world_update_event.cube_pos, .tick = world_update_event.tick });

            const target = @as(f64, @floatFromInt(world_update_event.tick - tick));
            interpolated_tick_delta = target;
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
            buffer_cooldown -= __resources.get(zeng.time_res).delta_time;
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

        synced_time += __resources.get(zeng.time_res).delta_time * timescale;
        accumulator += __resources.get(zeng.time_res).delta_time * sim_timescale;
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
            if (zeng.get_mouse_button(.left) and !done) {
                local_player_shoot = true;
                done = true;
            }
            if (!zeng.get_mouse_button(.left)) done = false;
            if (local_player_shoot) aud.play_sound(datablob.get("sounds/gun_shot.wav", aud.audio_sample_info).*, .one_shot);

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

            const cam = world.get(main_camera, zeng.camera).?;
            if (zeng.get_mouse_button(.right)) {
                cam.fov = 1.2;
            } else {
                cam.fov = 1.5;
            }
            cam.projection_matrix = zeng.mat_perspective_projection(cam.fov, @as(f32, @floatFromInt(__graphics.width)) / @as(f32, @floatFromInt(__graphics.height)), 0.01, 1000.0);

            fet.run_system(camera_fly_system);
            fet.run_system(zeng.Player.player_collision_system);
            fet.run_system(zeng.Player.player_simulate_and_animate_system);
            fet.run_system(zeng.Player.shoot_system);

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

            fet.run_system(frame_interpolator_tick_store_system);
        }

        { // interpolate player and set camera to player; use players input component to apply rotation to the camera
            const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
            const player_interpolator = world.get(player_entity, zeng.frame_interpolator).?;
            const interpolated_matrix = player_interpolator.get_matrix(@as(f32, @floatCast(accumulator / fixed_delta)));
            const camera_target_position = zeng.mat_position(interpolated_matrix);
            zeng.mat_position_set(cam_matrix, camera_target_position.add(zeng.vec3{ .y = 0.75 }));
            const local_player_input = world.get(player_entity, rpc.input_message).?;
            zeng.matrix_use_rotations(world.get(main_camera, zeng.world_matrix).?, local_player_input.rot_x, local_player_input.rot_y);
        }

        const gun_position: zeng.vec4 = if (zeng.get_mouse_button(.right)) zeng.vec4{ .y = -0.107, .z = -0.15 } else zeng.vec4{ .x = 0.15, .y = -0.15, .z = -0.2 };
        world.get(pistol_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(main_camera, zeng.world_matrix).?.*, zeng.mat_mult_vec4(world.get(main_camera, zeng.world_matrix).?.*, gun_position).to_vec3());
        for (top_children.items) |ch| {
            zeng.sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
        }

        if (!is_server) { // animation host player on client
            const skel = world.get(remote_player_skeleton_entity, zeng.skeleton).?;
            const anim = world.get(remote_player_skeleton_entity, zeng.animation_component).?;
            const animation = datablob.get("assets/gltf/static_test.gltf/animations/idle", zeng.loader.animation);

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
        animate_skeleton(test_skeleton_entity, "assets/gltf/people/KingShiny.gltf/animations/Walk", @floatCast(__resources.get(zeng.time_res).delta_time), &datablob, &world);

        const camera_camera_ptr = world.get(main_camera, zeng.camera).?;
        const camera_matrix_ptr = world.get(main_camera, zeng.world_matrix).?;
        zeng.render.draw_sky(sky_shader, square_vao, square_indices_length, camera_matrix_ptr.*, camera_camera_ptr);
        fet.run_system(render_system);
        // zeng.render.draw_mesh(cube_mesh, zeng.mat_tran(zeng.mat_scal(zeng.mat_identity, zeng.vec3.ONE.mult(0.1)), zeng.vec3.ZERO), world.get(__resources.get(zeng.main_camera_res).id, zeng.camera).?.projection_matrix, zeng.mat_invert(world.get(__resources.get(zeng.main_camera_res).id, zeng.world_matrix).?.*));

        // const mouse_state = mouse_state_t{ .pos = .{ .x = @floatFromInt(zeng.global_mouse_pos[0]), .y = @floatFromInt(zeng.global_mouse_pos[1]) } };
        // const mouse_pressed = zeng.get_mouse_button(.left) and !mouse_down_last_frame;
        // const mouse_released = !zeng.get_mouse_button(.left) and mouse_down_last_frame;
        defer mouse_down_last_frame = zeng.get_mouse_button(.left);

        // { // block code
        //     var D1 = BlockNode{ .block = .{ .Add = .{ .a = 0, .b = 0 } }, .children = null };
        //     var C1 = BlockNode{ .block = .{ .String = "return;" }, .children = null };
        //     var B1 = BlockNode{ .block = .{ .String = "player.health = 100;" }, .children = &.{&D1} };
        //     var A1 = BlockNode{ .block = .{ .String = "game_start()" }, .children = &.{ &B1, &C1 } };

        //     const thickness: f32 = 18;
        //     const pos: zeng.vec2 = .{ .x = 100, .y = 100 };
        //     _ = get_height(&A1, thickness);
        //     draw(&A1, thickness, &__ui_box, &__graphics, &__resources, pos);
        // }
        { // makeshift UI debug HUD
            var buffer: [64]u8 = undefined;
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / __resources.get(zeng.time_res).delta_time}) catch unreachable, __resources.get(zeng.text_render_res), 0, 0, __graphics);
            if (!is_server) {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "synctime: {d:.6}", .{synced_time}) catch unreachable, __resources.get(zeng.text_render_res), 0, 80, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffer: {d:.6}", .{buffer_time}) catch unreachable, __resources.get(zeng.text_render_res), 0, 120, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "timescale: {d:.6}", .{sim_timescale}) catch unreachable, __resources.get(zeng.text_render_res), 0, 160, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "buffvel: {d:.6}", .{buffer_velocity}) catch unreachable, __resources.get(zeng.text_render_res), 0, 200, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "    rtt: {d:.6}", .{draw_rtt}) catch unreachable, __resources.get(zeng.text_render_res), 0, 240, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "exp rtt: {d:.6}", .{exponential_rtt}) catch unreachable, __resources.get(zeng.text_render_res), 0, 280, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "resims: {}", .{draw_resims}) catch unreachable, __resources.get(zeng.text_render_res), 0, 320, __graphics);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "interp: {d:.6}", .{interpolated_tick_delta}) catch unreachable, __resources.get(zeng.text_render_res), 0, 360, __graphics);

                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 0, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 0, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), @as(f32, @floatCast(draw_time_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);

                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 500, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 500, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);
            } else {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accumulator}) catch unreachable, __resources.get(zeng.text_render_res), 0, 40, __graphics);
            }
            for (str_events.array.items) |str| {
                zeng.render.draw_text(str, __resources.get(zeng.text_render_res), 0, 0, __graphics);
            }
            str_events.clear(allocator);
            for (tri_events.array.items) |tri| {
                zeng.render.debug_draw_triangle(tri, __resources.get(zeng.debug_res).*);
            }
            tri_events.clear(allocator);

            var ticker_display_time: f64 = undefined;
            if (is_server) {
                ticker_display_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator;
            } else {
                ticker_display_time = synced_time;
            }
            // zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), @floatCast(@mod(ticker_display_time * 100.0, 100.0) - 400), 400, 10, 10, zeng.render.color.YELLOW);
            __ui_box.draw_img(__graphics, @floatCast(@mod(ticker_display_time * 150.0, 150.0)), 0, 15, 15, .YELLOW, 0, 0);
            zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 0, 0, 6, 6, zeng.render.color.BLACK);
            zeng.render.draw_rect(__graphics, __resources.get(zeng.rect_render_res), 0, 0, 4, 4, zeng.render.color.WHITE);
        }
        { // UI processing
            // zeng.set_cursor(.arrow);

            // const page = page_widget(uv_checker_tex, uv_checker_tex, @floatFromInt(__graphics.width), @floatFromInt(__graphics.height), 0.3);
            // ui_layout(.{ .x = 0, .y = 0 }, page);
            // // const nptr = recursive_mouse_over(page, &mouse_state);
            // // zeng.set_cursor(.arrow);
            // // if (nptr) |node_ptr| {
            // //     if (node_ptr.color.?.a > 0.01) zeng.set_cursor(.pointer);
            // // }
            // const button = global_id_map.get("play_button");
            // if (mouse_over(button, &mouse_state)) {
            //     zeng.set_cursor(.pointer);
            //     button.color = .WHITE;
            //     if (mouse_pressed) {
            //         game_api.on_button_pressed(&__resources);
            //     }
            // }
            // ui_draw(&__ui_box, __graphics, page, __resources.get(zeng.text_render_res));

            // if (render_info == null) {
            //     render_info = visualize_graph(arena_allocator, page);
            // }

            // for (render_info.?.nodes) |*node| {
            //     var sum = vec2.ZERO;
            //     for (render_info.?.nodes) |*_node| {
            //         if (node == _node) continue;
            //         sum = sum.add(_node.sub(node.*).normalized());
            //     }
            //     sum = sum.normalized().mult(-0.5);
            //     node.* = node.add(sum);
            // }
            // for (render_info.?.edges) |*edge| {
            //     const pos_a = &render_info.?.nodes[edge.from];
            //     const pos_b = &render_info.?.nodes[edge.to];

            //     const delta = pos_a.sub(pos_b.*);
            //     const over_amt = delta.length() - 50;
            //     if (over_amt > 0) {
            //         pos_a.* = pos_a.add(delta.normalized().mult(-over_amt * 0.5));
            //         pos_b.* = pos_b.add(delta.normalized().mult(over_amt * 0.5));
            //     }
            // }
            // for (render_info.?.edges) |edge| {
            //     const pos_a = render_info.?.nodes[edge.from];
            //     const pos_b = render_info.?.nodes[edge.to];

            //     for (0..10) |i| {
            //         const p = pos_a.lerp(pos_b, @as(f32, @floatFromInt(i)) / @as(f32, @floatFromInt(10)));
            //         __ui_box.draw_img(__graphics, p.x, p.y, 6, 6, .WHITE, 0, 0);
            //     }
            //     __ui_box.draw_img(__graphics, pos_a.x, pos_a.y, 15, 15, .BLACK, 0, 4);
            //     __ui_box.draw_img(__graphics, pos_b.x, pos_b.y, 15, 15, .BLACK, 0, 4);
            // }

            // global_num = 0;
            // const a = block_widget(top, .{});
            // ui_layout(.{ .x = 0, .y = 0 }, a);
            // ui_draw(&__ui_box, __graphics, a, __resources.get(zeng.text_render_res));

            // if (mouse_pressed) {
            //     selected_parameter = null;
            // }
            // const nptr = recursive_mouse_over(a, &mouse_state);
            // if (nptr) |node_ptr| {
            //     if (node_ptr.data_ptr != null) {
            //         zeng.set_cursor(.pointer);
            //         if (mouse_pressed) {
            //             selected_parameter = @ptrCast(@alignCast(node_ptr.data_ptr.?));
            //         }
            //     }
            // }
        }
        { // block code 2
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
            // zeng.key_press_messages.clearAndFree(allocator);

            // var hovered_p: ?*CodeBlock = null;
            // var hovered_c: ?usize = null;
            // var hovered_block: ?*CodeBlock = null;
            // var hovered_param: ?*Parameter = null;

            // // for (top_level_code_blocks.items) |code_block| {
            // //     _ = get_height(code_block, 18);
            // //     draw(code_block, 18, &__ui_box, &__graphics, &__resources, code_block.__rect.pos);
            // // }
            // for (top_level_code_blocks.items) |code_block| {
            //     hovered_p, hovered_c, hovered_block, hovered_param = get_mouseover(code_block, null, null, mouse_state.pos);
            //     if (hovered_block != null) break;
            // }
            // if (mouse_pressed) {
            //     if (hovered_block != null) {
            //         if (hovered_p != null and hovered_c != null) {
            //             dragging_code_block = hovered_p.?.children.orderedRemove(hovered_c.?);
            //             top_level_code_blocks.append(allocator, dragging_code_block.?) catch unreachable;
            //         } else {
            //             dragging_code_block = hovered_block;
            //         }
            //     }
            // }
            // if (mouse_released) {
            //     if (hovered_block != null and dragging_code_block != null and hovered_block.? != dragging_code_block.?) {
            //         if (hovered_p != null and hovered_c != null) {
            //             hovered_p.?.children.insert(allocator, hovered_c.?, dragging_code_block.?) catch unreachable;
            //             remove_code_block_ptr(&top_level_code_blocks, dragging_code_block.?);
            //         } else {
            //             hovered_block.?.children.insert(allocator, 0, dragging_code_block.?) catch unreachable;
            //             remove_code_block_ptr(&top_level_code_blocks, dragging_code_block.?);
            //         }
            //     }
            // }
            // if (!zeng.get_mouse_button(.left)) dragging_code_block = null;

            // if (dragging_code_block) |cb| {
            //     cb.__rect.pos = mouse_state.pos;
            // }

            // if (zeng.get_key(.r) and !r_pressed_last_frame) {
            //     // top.execute.?(top, &__resources, &script_context);
            //     create_script_zig_file(top, allocator);
            //     script_context.reset();
            // }
            // r_pressed_last_frame = zeng.get_key(.r);

            // // var buffer: [255]u8 = undefined;
            // // zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], ":{} :{}", .{ top_level_code_blocks.items.len, top.children.items.len }) catch unreachable, __resources.get(zeng.text_render_res), 200, 40, __graphics);
        }

        commands.process_commands(&world);
        zeng.net.send_net_messages(&commands, __resources.get(zeng.time_res).delta_time, &tracker);

        zeng.swap_buffers(__graphics);
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
pub fn render_system(world: *ecs.world, cam: *zeng.main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const cam_matrix = world.get(cam.id, zeng.world_matrix).?;
    const cam_cam = world.get(cam.id, zeng.camera).?;

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam_matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.render.draw_mesh(mesh.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix, zeng.mat_position(cam_matrix.*));
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.render.draw_animated_skinned_mesh(world, skin.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix, zeng.mat_position(cam_matrix.*));
    }
}
/// Run collisions for all players
/// Frame buffers spatial data to be interpolated at a high framerate
pub fn frame_interpolator_tick_store_system(interp_q: *ecs.query(.{ zeng.frame_interpolator, zeng.world_matrix })) !void {
    var interp_it = interp_q.iterator();
    while (interp_it.next()) |curr| {
        const fi: *zeng.frame_interpolator, const wm: *zeng.world_matrix = curr;
        fi.store(wm.*);
    }
}

pub fn animate_skeleton(entity: ecs.entity_id, animation_name: []const u8, delta_time: f32, datablob: *zeng.Datablob, world: *ecs.world) void {
    const animation: *zeng.loader.animation = datablob.get(animation_name, zeng.loader.animation);
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

// start adding game elements to test the architecture flexibility
// finish block code + UI

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
// add proper materials
// test and overhaul the reliable message system
