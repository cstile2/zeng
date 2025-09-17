const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const phy = @import("physics.zig");
const aud = @import("audio.zig");
const util = @import("utils.zig");
const net = @import("networking.zig");
const gl = zeng.gl;
const c = zeng.c;

pub const COMPONENT_TYPES = [_]type{
    zeng.mesh,
    zeng.camera,
    zeng.skinned_mesh,
    zeng.world_matrix,
    sphere_collider,
    fly_component,
    follow_component,
    zeng.children,
    zeng.local_matrix,
    player,
    animation_component,
    zeng.skeleton,
    input_implement,
    rpc.input_message,
    floater_component,
    snapshot_interpolator,
    frame_interpolator,
};
pub const sphere_collider = struct {
    radius: f32 = 1.0,
};
pub const fly_component = struct {
    // SOMETHING_TO_MAKE_THIS_NOT_AN_EMPTY_STRUCT: u8 = 0,
};
pub const follow_component = struct {
    anchor_point: zeng.vec3,
    target: ecs.entity_id,
};
pub const animation_player = struct {
    time: f32,
    current_animation: usize,
    animations: []zeng.animation,
    skeleton_ptr: *zeng.skeleton,
};
pub const player = struct {
    velocity: zeng.vec3,
    old_velocity: zeng.vec3 = zeng.vec3.ZERO,
    ground_normal: zeng.vec3,
    grounded: bool,
    animation_controller: ecs.entity_id,
    tilt: zeng.vec3 = zeng.vec3.ZERO,
    camera: ecs.entity_id,
};
pub const animation_component = struct {
    time: f32,
    current_animation: usize,
};
pub const input_implement = struct {
    move_fn: *const fn () zeng.vec2,
    jump_fn: *const fn () bool,
    pub fn default_move_fn() zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (zeng.get_key(.a)) {
            input_vect.x += -1;
        }
        if (zeng.get_key(.d)) {
            input_vect.x += 1;
        }
        if (zeng.get_key(.w)) {
            input_vect.y += -1;
        }
        if (zeng.get_key(.s)) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_move_fn2() zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (zeng.get_key(.left)) {
            input_vect.x += -1;
        }
        if (zeng.get_key(.right)) {
            input_vect.x += 1;
        }
        if (zeng.get_key(.up)) {
            input_vect.y += -1;
        }
        if (zeng.get_key(.down)) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_jump() bool {
        return zeng.get_key(.space);
    }
    pub fn default_jump2() bool {
        return zeng.get_key(.y);
    }
};
pub const floater_component = struct {
    // SOMETHING_TO_MAKE_THIS_NOT_AN_EMPTY_STRUCT: u8 = 0,
};
pub const snapshot_interpolator = struct { // interpolates visual elements between server updates, smoothing movement of remote players
    buffer: zeng.ring_buffer(struct { position: zeng.vec3, tick: isize }),
};
pub const frame_interpolator = struct { // interpolates visual elements between simulation ticks, smoothing movement for high FPS
    pos_a: zeng.vec3,
    pos_b: zeng.vec3,

    rot_a: zeng.quat,
    rot_b: zeng.quat,

    pub fn store(self: *@This(), mat: [16]f32) void {
        self.pos_a = self.pos_b;
        self.pos_b = zeng.mat_position(mat);

        self.rot_a = self.rot_b;
        self.rot_b = zeng.mat_to_quat(mat);
    }
    pub fn get_matrix(self: @This(), t_value: f32) [16]f32 {
        const pos = self.pos_a.lerp(self.pos_b, t_value);
        const rot = self.rot_a.nlerp(self.rot_b, t_value);

        return zeng.mat_tran(zeng.quat_to_mat(rot), pos);
    }
};

pub const time_res = struct {
    delta_time: f64,
    dt: f32,
    fixed_delta_time: f64,
    fixed_dt: f32,
};
pub const input_res = struct {
    t_down_last_frame: bool,
};
pub const main_camera_res = struct {
    id: ecs.entity_id,
};
pub const text_render_res = struct {
    shader_program: u32,
    texture: u32,
    vao: u32,
    indices_len: c_int,
};
pub const rect_render_res = struct {
    shader_program: u32,
    vao: u32,
    indices_len: c_int,
};
pub const networking_res = struct {
    main_socket: zeng.net.socket_t,
    server_address: zeng.net.Address,
    is_server: bool = undefined,
};
pub const debug_res = @import("render.zig").triangle_debug_info;
// pub const animation_res = std.ArrayList(zeng.loader.animation);
pub const cube_tracker_res = struct {
    map: std.AutoHashMap(phy.ivec3, void),
    cube_mesh: zeng.mesh,
    cube_mesh_data: *const anyopaque,
};

const client_info = struct {
    input_buffer: zeng.ring_buffer(rpc.input_message),
    player: ecs.entity_id,
};

pub var global_world_ptr: *ecs.world = undefined;
pub var global_player_entity: ecs.entity_id = undefined;
pub var global_camera_entity: ecs.entity_id = undefined;
pub var global_mouse_pressed = false;

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
    direction: Direction = .left_to_right,
    id: ?[]const u8 = null,
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
const vec2 = zeng.vec2;

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
    node.pos = pos;
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
pub fn ui_draw(drawer: *const __ui_box_module, ctx: zeng.__graphics_module, node: *Node) void {
    const color = node.color orelse zeng.render.color.WHITE;

    drawer.draw_img(ctx, node.pos.x, node.pos.y, node.width, node.height, color, node.img orelse 0, node.radius);

    if (node.children != null) {
        for (node.children.?.items) |child| {
            ui_draw(drawer, ctx, child);
        }
    }
}
pub fn ui_layout(pos: vec2, node: *Node) void {
    ui_hug_children(node);
    ui_grow(node);
    ui_pos(pos, node);
}
/// TODO: finish this
pub fn deep_copy(allocator: std.mem.Allocator, node: *Node) *Node {
    const new = allocator.create(node) catch unreachable;
    new.* = node.*;

    // need to deep copy all children, and growables making sure that if &old.growable[a] == &old.children[b] then &new.growable[a] == &new.children[b]
    // for (node.children.?.items) |child| {}

    return new;
}

pub fn mouse_over(node: *const Node, s: *const mouse_state_t) bool {
    return (s.pos.x > node.pos.x and s.pos.x < node.pos.x + node.width and s.pos.y > node.pos.y and s.pos.y < node.pos.y + node.height);
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
        n(.{ .color = .CLEAR, .width_size_mode = .grow, .height_size_mode = .grow }, &.{}),
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

pub fn create_player(datablob: *Datablob, world: *ecs.world, skin_shader: u32, static_shader: u32, uv_checker_tex: u32, fet: *zeng.resource_fetcher, top_children: *std.ArrayList(ecs.entity_id), allocator: std.mem.Allocator) ecs.entity_id {
    const player_entity = zeng.loader.auto_import(datablob, world, "assets/gltf", "static_test", skin_shader, static_shader, uv_checker_tex, allocator);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, player_entity);
    world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2.ZERO, .rot_x = 0.0, .rot_y = 0.0 }, player_entity);
    const player_random_skinned_mesh = find_component_of_type(world, player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
    const player_skeleton_entity = world.get(player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, player_skeleton_entity);
    world.get(player_entity, player).?.animation_controller = player_skeleton_entity;
    top_children.append(allocator, player_entity) catch unreachable;

    return player_entity;
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
    const time = datablob.get("time", time_res);

    const run_animation = datablob.get("assets/gltfguy.gltf/run", zeng.loader.animation);
    const walk_animation = datablob.get("assets/gltfguy.gltf/walk", zeng.loader.animation);

    const anim = world.get(world.animation_controller, animation_component).?;
    const skel = world.get(_player.animation_controller, zeng.skeleton).?;
    const blend = _player.velocity.div(3.0).clamp(1.0).length();

    anim.time += time.fixed_dt / zeng.lerp(run_animation.duration, walk_animation.duration, blend);
    while (anim.time > 1.0) {
        anim.time -= 1.0;
    }

    const pose: zeng.skeleton_pose = undefined;
    get_animation_pose_with_weight(run_animation, anim.time, pose, blend);
    add_animation_pose_with_weight(walk_animation, anim.time, pose, 1.0 - blend);
    normalize_pose_quaternions(pose);
    apply_pose_to_skeleton(skel, pose);
}

pub const Datablob = struct {
    map: std.StringHashMap(*anyopaque),

    pub fn get(this: *@This(), str: []const u8, T: type) *T {
        return @ptrCast(@alignCast(this.map.get(str).?));
    }
    pub fn get_maybe(this: *@This(), str: []const u8, T: type) ?*T {
        return @ptrCast(@alignCast(this.map.get(str) orelse return null));
    }
    pub fn put(this: *@This(), str: []const u8, ptr: *anyopaque) void {
        this.map.put(str, ptr) catch unreachable;
    }
};

pub const packet_ack_tracker_t = struct {
    pub const packet_data_t = struct {
        acked: bool,
    };
    pub const header_t = struct {
        sequence_number: usize,
        most_recent_sequence_number_recieved: usize,
        ack_bits: u32,
    };
    pub const table_t = struct {
        packet_data: [1024]packet_data_t,
        sequence_buffer: [1024]usize,
        pub fn get_packet_data(this: *@This(), sequence_number: usize) ?*packet_data_t {
            const index: usize = sequence_number % this.packet_data.len;
            if (this.sequence_buffer[index] == sequence_number) {
                return &packet_data_t[index];
            } else return null;
        }

        pub fn insert(this: *@This(), sequence_number: usize) *packet_data_t {
            const index: usize = sequence_number % this.packet_data.len;
            this.sequence_buffer[index] = sequence_number;
            return &this.packet_data[index];
        }
    };

    mine: table_t,
    theirs: table_t,
    mine_sequence: usize,
    their_sequence: usize,

    pub fn generate_header(this: *@This()) header_t {
        var bits: u32 = 0;

        // Loop over the previous 32 sequence numbers
        for (0..32) |i| {
            const seq = this.their_sequence - i - 1;
            if (this.theirs.get_packet_data(seq).?.acked) {
                bits |= 1 << i; // set bit i if that packet was received
            }
        }

        return header_t{
            .ack_bits = bits,
            .sequence_number = this.mine_sequence,
            .most_recent_sequence_number_recieved = this.their_sequence,
        };
    }
};

pub fn packet_send(packet: anytype, tracker: *packet_ack_tracker_t) void {
    tracker.insert(tracker.mine_sequence).* = .{ .acked = false };

    const header = tracker.generate_header();
    packet.put_in_header(header);
    packet.send();

    tracker.mine_sequence += 1;
}

pub fn packet_recieve(packet: anytype, tracker: *packet_ack_tracker_t) void {
    var bits: u32 = undefined;
    if (packet.sequence_number > tracker.their_sequence) {
        bits = bits << packet.sequence_number - tracker.their_sequence;
        bits = bits & 1;
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

    var __graphics: zeng.__graphics_module = undefined;
    __graphics.init();
    var __resources: zeng.resources_t = undefined;
    __resources.init(arena_allocator, &__graphics);
    var __ui_box: __ui_box_module = undefined;
    __ui_box.init(arena_allocator);
    global_id_map.init(arena_allocator);

    var datablob = Datablob{ .map = std.StringHashMap(*anyopaque).init(allocator) };
    defer datablob.map.deinit();

    datablob.map.put("graphics", &__graphics) catch unreachable;
    datablob.map.put("ui", &__ui_box) catch unreachable;
    __resources.insert_ptr(&datablob);

    var world: ecs.world = ecs.world.init(allocator);
    defer world.deinit() catch unreachable;
    var fet: zeng.resource_fetcher = .{ .world = &world, .res = &__resources, .allocator = arena_allocator };

    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});
    global_world_ptr = &world;

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
    const main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, is_server) catch unreachable;
    defer zeng.net.undo_setup(main_socket);
    const server_address = net.sockaddr_socklen_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };

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
    const black_tex = zeng.loader.load_texture("assets/images/black.png", true, false);
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .texture = uv_checker_tex }, .vao_gpu = cube_vao };
    const gun_shot = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/gun_shot.wav", arena_allocator)) catch unreachable;
    const bell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/bell.wav", arena_allocator)) catch unreachable;

    var top_children = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    defer top_children.deinit(arena_allocator);

    const pistol_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "pistol", skin_shader, static_shader, black_tex, arena_allocator);
    top_children.append(arena_allocator, pistol_entity) catch unreachable;
    const map_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "outdoor_map_6_8_25", skin_shader, static_shader, uv_checker_tex, arena_allocator);
    top_children.append(arena_allocator, map_entity) catch unreachable;
    const cube_entity = world.spawn(.{ cube_mesh, zeng.mat_tran(zeng.mat_identity, .{ .x = -7.0, .y = 2.0 }), floater_component{} });
    if (!is_server) world.add(snapshot_interpolator{ .buffer = undefined }, cube_entity);

    const player_entity = create_player(&datablob, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);
    world.add(input_implement{ .move_fn = input_implement.default_move_fn, .jump_fn = input_implement.default_jump }, player_entity);
    world.get(player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 10.0 });
    world.add(@as(frame_interpolator, undefined), player_entity);
    var found_entity = find_component_of_type(&world, player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children}));
    while (found_entity) |_| {
        world.remove(zeng.skinned_mesh, found_entity.?);
        found_entity = find_component_of_type(&world, player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children}));
    }
    global_player_entity = player_entity;

    var remote_player_entity: ecs.entity_id = undefined;
    var remote_player_skeleton_entity: ecs.entity_id = undefined;
    if (!is_server) {
        remote_player_entity = zeng.loader.auto_import(&datablob, &world, "assets/gltf", "static_test", skin_shader, static_shader, uv_checker_tex, arena_allocator);
        const remote_player_random_skinned_mesh = find_component_of_type(&world, remote_player_entity, zeng.skinned_mesh, fet.fresh_query(.{zeng.children})).?;
        remote_player_skeleton_entity = world.get(remote_player_random_skinned_mesh, zeng.skinned_mesh).?.skeleton;
        world.add(animation_component{ .time = 0.0, .current_animation = 0 }, remote_player_skeleton_entity);
        world.get(remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 10.0 });
        world.add(snapshot_interpolator{ .buffer = undefined }, remote_player_entity);
        top_children.append(arena_allocator, remote_player_entity) catch unreachable;
    }

    const square_vao, const square_indices_length = zeng.loader.create_square_mesh();
    __resources.insert(text_render_res{ .shader_program = zeng.loader.load_shader(allocator, "assets/shaders/text_vertex.shader", "assets/shaders/text_fragment.shader"), .texture = zeng.loader.load_texture("assets/images/sdf_font.png", false, true), .vao = square_vao, .indices_len = square_indices_length });
    __resources.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands{ .reliable_message_seqs = std.AutoHashMap(usize, net.remote_message).init(allocator), .random = __resources.get(std.Random.Xoshiro256).random(), .remote_messages_send_queue = undefined, .remote_messages_send_queue_len = 0, .allocator = allocator };
    defer commands.destroy();
    __resources.insert_ptr(&commands);
    __resources.insert(input_res{ .t_down_last_frame = false });
    __resources.insert_ptr(&__graphics);
    __resources.insert_ptr(&world);
    __resources.insert(networking_res{ .main_socket = main_socket, .server_address = _server_address, .is_server = is_server });
    __resources.insert(rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });
    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, follow_component{ .target = player_entity, .anchor_point = zeng.mat_position(world.get(player_entity, zeng.world_matrix).?.*) } });
    global_camera_entity = main_camera;
    __resources.insert(debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = world.get(main_camera, zeng.camera).?.projection_matrix, .inv_camera_matrix = zeng.mat_invert(world.get(main_camera, zeng.world_matrix).?.*) });
    __resources.insert(main_camera_res{ .id = main_camera });
    world.get(player_entity, player).?.camera = main_camera;
    zeng.window_resize_handler(__graphics.width, __graphics.height);
    var client_map = std.AutoHashMap(net.sockaddr_socklen_t, client_info).init(allocator);
    defer client_map.deinit();

    // load in map colliders and construct spatial hash grid
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

    if (!is_server) {
        // commands.remote_event(main_socket, server_address, rpc.player_spawn_message{}, .reliable);
        zeng.net.remote_event(&commands, main_socket, server_address, rpc.player_spawn_message{}, .reliable);
    }

    const fixed_rate: f64 = 60.0;
    const fixed_delta: f64 = 1.0 / fixed_rate;
    __resources.insert(time_res{ .delta_time = 0.006944, .dt = 0.006944, .fixed_delta_time = fixed_delta, .fixed_dt = @floatCast(fixed_delta) });
    __resources.get(time_res).fixed_dt = @floatCast(fixed_delta);

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

    var render_info: ?graph_render.render_info = null;

    // main loop - rendering happens every frame at monitor framerate, game simulation is decoupled and runs at about 60hz
    while (true) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&__resources);
        if (zeng.quit) break;
        commands.time += __resources.get(time_res).delta_time;
        zeng.net.recieve_net_messages(main_socket, &__resources, &commands, allocator);

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

        // server communication
        for (server_tick_offset_events.array.items, server_tick_offset_events.addresses.?.items) |server_tick_offset_event, _| {
            const rtt = synced_time - server_tick_offset_event.client_time;
            draw_rtt = rtt;

            const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
            draw_time_alignment = time_offset;

            if (@abs(time_offset) > 0.7) {
                std.debug.print("time jump\n", .{});
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
            const info = client_map.getPtr(sockaddr) orelse break;

            for (input_event.arr) |_input_event| {
                if (_input_event.tick >= tick) {
                    info.input_buffer.set(_input_event.tick, _input_event);
                } // else discard late message
            }
        }
        input_events.clear(allocator);
        for (player_join_events.array.items, player_join_events.addresses.?.items) |_, sockaddr| {
            std.debug.print("player connected: \n", .{});

            const new_remote_player_entity = create_player(&datablob, &world, skin_shader, static_shader, uv_checker_tex, &fet, &top_children, arena_allocator);

            world.get(new_remote_player_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(new_remote_player_entity, zeng.world_matrix).?.*, zeng.vec3{ .y = 60.0 });
            world.get(new_remote_player_entity, player).?.camera = world.spawn(.{zeng.mat_identity});

            client_map.put(sockaddr, client_info{ .input_buffer = zeng.ring_buffer(rpc.input_message){ .arr = undefined }, .player = new_remote_player_entity }) catch unreachable;
        }
        player_join_events.clear(allocator);
        for (client_tick_events.array.items, client_tick_events.addresses.?.items) |client_tick_event, sockaddr| {
            // commands.remote_event(main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
            zeng.net.remote_event(&commands, main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator, .client_time = client_tick_event.time }, .unreliable);
        }
        client_tick_events.clear(allocator);

        // client communication
        for (snap_events.array.items, snap_events.addresses.?.items) |snap_event, _| {
            const P = world.get(player_entity, player).?;
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
                    std.debug.print("missing buffered input for tick: {} {} {}\n", .{ im.tick, _tick, tick });
                    break;
                }
                simulate_collision(world.get(player_entity, player).?, world.get(player_entity, zeng.world_matrix).?, &spatial_hash_grid, &tri_events, __resources.get(debug_res));
                simulate_player(world.get(player_entity, player).?, &im, world.get(player_entity, zeng.world_matrix).?, __resources.get(time_res));
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
            const SI = world.get(remote_player_entity, snapshot_interpolator).?;
            SI.buffer.set(world_update_event.tick, .{ .position = zeng.mat_position(world_update_event.server_player_matrix), .tick = world_update_event.tick });

            const SI2 = world.get(cube_entity, snapshot_interpolator).?;
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

            const interp_q = fet.fresh_query(.{ snapshot_interpolator, zeng.world_matrix });
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
            buffer_cooldown -= __resources.get(time_res).delta_time;
            if (buffer_cooldown < -10.0) {
                buffer_velocity = -0.002;
            } else if (buffer_cooldown < 0.0) {
                buffer_velocity = 0.0;
            }
            buffer_time += __resources.get(time_res).delta_time * buffer_velocity;

            const desired_time = synced_time + buffer_time;
            const my_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accumulator;
            const offset = desired_time - my_time;
            draw_local_alignment = offset;
            if (@abs(offset) > 0.5) {
                tick += @intFromFloat(offset * fixed_rate);
                std.debug.print("skipping time\n", .{});
            } else {
                sim_timescale = zeng.lerp(sim_timescale, 1.0 + offset / 5.0, 0.2);
            }
        }

        synced_time += __resources.get(time_res).delta_time * timescale;
        accumulator += __resources.get(time_res).delta_time * sim_timescale;
        while (accumulator >= fixed_delta) {
            defer tick += 1;
            accumulator -= fixed_delta;

            if (is_server) { // input prep
                var a = client_map.iterator();
                while (a.next()) |_c| {
                    const cool = _c.value_ptr.input_buffer.get(tick);
                    if (cool.tick == tick) {
                        const ent = _c.value_ptr.player;
                        const remote_player_input = world.get(ent, rpc.input_message).?;
                        remote_player_input.* = cool;
                    } else {
                        std.debug.print("missed input {}\n", .{tick});
                    }
                    if (_c.value_ptr.input_buffer.get(tick + 10).tick != tick + 10) {
                        zeng.net.remote_event(&commands, main_socket, _c.key_ptr.*, rpc.missed_input{}, .unreliable);
                    }
                }
            }
            const local_player_input = world.get(player_entity, rpc.input_message).?;
            local_player_input.* = rpc.input_message{ .tick = tick, .jump = input_implement.default_jump(), .move_vect = input_implement.default_move_fn(), .rot_x = local_player_input.rot_x, .rot_y = local_player_input.rot_y };
            if (!is_server) {
                client_input_buffer.set(tick, world.get(player_entity, rpc.input_message).?.*);
                var snd: rpc.input_chunck = undefined;
                var curr: usize = 0;
                while (curr < snd.arr.len) {
                    defer curr += 1;
                    snd.arr[curr] = client_input_buffer.get(tick - @as(isize, @intCast(curr)));
                }
                zeng.net.remote_event(&commands, main_socket, server_address, snd, .unreliable);
            }
            if (is_server) {
                const floater_q = fet.fresh_query(.{ floater_component, zeng.world_matrix });
                var floater_it = floater_q.iterator();
                while (floater_it.next()) |curr| {
                    _, const M = curr;
                    zeng.mat_position_set(M, .{ .x = -7.0, .y = 2.0, .z = @mod(@as(f32, @floatFromInt(tick)) * 0.01, 1.0) * 20.0 - 5.0 });
                }
            }

            fet.run_system(camera_fly_system);
            fet.run_system(player_collision_system);
            fet.run_system(player_simulate_and_animate_system);

            if (is_server) { // periodic client/server communication
                var client_it = client_map.iterator();
                while (client_it.next()) |thing| {
                    const P = world.get(thing.value_ptr.player, player).?.*;
                    const M = world.get(thing.value_ptr.player, zeng.world_matrix).?.*;
                    if (@as(usize, @intCast(tick)) % 4 == 0) zeng.net.remote_event(&commands, main_socket, thing.key_ptr.*, rpc.state_correction{ .tick = tick, .state = P, .world_matrix = M }, .unreliable);
                }

                client_it = client_map.iterator();
                while (client_it.next()) |thing| {
                    if (@as(usize, @intCast(tick)) % 1 == 0) {
                        zeng.net.remote_event(&commands, main_socket, thing.key_ptr.*, rpc.world_update{ .cube_pos = zeng.mat_position(world.get(cube_entity, zeng.world_matrix).?.*), .server_player_matrix = world.get(player_entity, zeng.world_matrix).?.*, .tick = tick }, .unreliable);
                    }
                }
            } else {
                if (@as(usize, @intCast(tick)) % 60 == 0) {
                    zeng.net.remote_event(&commands, main_socket, server_address, rpc.client_tick{ .time = synced_time }, .unreliable);
                }
            }

            if (global_mouse_pressed) { // player shoots gun
                global_mouse_pressed = false;
                aud.play_sound(gun_shot, .one_shot);

                if (is_server) {
                    var enter_t: f32 = undefined;
                    var exit_t: f32 = undefined;
                    var _error: bool = undefined;

                    var it = client_map.iterator();
                    while (it.next()) |thing| {
                        const a_coll = phy.collider_info{ .data = undefined, .matrix = world.get(thing.value_ptr.player, zeng.world_matrix).?.*, .support = &phy.player_capsule, .tag = .support_based };
                        const b_coll = phy.collider_info{ .data = undefined, .matrix = world.get(main_camera, zeng.world_matrix).?.*, .support = &phy.point, .tag = .support_based };

                        const result = phy.shape_cast(a_coll, b_coll, zeng.mat_forward(world.get(main_camera, zeng.world_matrix).?.*).neg(), &enter_t, &exit_t, &_error);
                        if (result) aud.play_sound(bell, .one_shot);
                    }
                }
            }

            fet.run_system(frame_interpolator_tick_system);
        }

        const cam_matrix = world.get(main_camera, zeng.world_matrix).?;
        const interp_model_root = world.get(player_entity, frame_interpolator).?;
        const cam_target_mat = interp_model_root.get_matrix(@as(f32, @floatCast(accumulator / fixed_delta)));
        const target_position = zeng.mat_position(cam_target_mat);
        zeng.mat_position_set(cam_matrix, target_position.add(zeng.vec3{ .y = 0.75 }));

        const local_player_input = world.get(player_entity, rpc.input_message).?;
        matrix_use_rotations(world.get(main_camera, zeng.world_matrix).?, local_player_input.rot_x, local_player_input.rot_y);

        world.get(pistol_entity, zeng.world_matrix).?.* = zeng.mat_tran(world.get(main_camera, zeng.world_matrix).?.*, zeng.mat_mult_vec4(world.get(main_camera, zeng.world_matrix).?.*, zeng.vec4{ .x = 0.15, .y = -0.15, .z = -0.2 }).to_vec3());
        for (top_children.items) |ch| {
            zeng.sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
        }

        if (!is_server) {
            const skel = world.get(remote_player_skeleton_entity, zeng.skeleton).?;
            const anim = world.get(remote_player_skeleton_entity, animation_component).?;
            const time = __resources.get(time_res);
            const animation = datablob.get("assets/gltf/static_test.gltf/animations/idle", zeng.loader.animation);

            anim.time += time.fixed_dt / animation.duration;
            while (anim.time > 1.0) {
                anim.time -= 1.0;
            }

            const rotations = allocator.alloc(zeng.quat, skel.bone_parent_indices.len) catch unreachable;
            const translations = allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
            const scales = allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
            get_animation_pose_with_weight(animation, anim.time, .{ rotations, translations, scales }, 1.0);
            apply_pose_to_skeleton(skel, .{ rotations, translations, scales });
            allocator.free(rotations);
            allocator.free(translations);
            allocator.free(scales);
        }

        const camera_camera_ptr = world.get(main_camera, zeng.camera).?;
        const camera_matrix_ptr = world.get(main_camera, zeng.world_matrix).?;
        zeng.render.draw_sky(sky_shader, square_vao, square_indices_length, camera_matrix_ptr.*, camera_camera_ptr);
        zeng.render.draw_mesh(cube_mesh, zeng.mat_identity, camera_camera_ptr.projection_matrix, zeng.mat_invert(camera_matrix_ptr.*));
        fet.run_system(render_system);

        { // makeshift UI debug HUD
            var buffer: [64]u8 = undefined;
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / __resources.get(time_res).delta_time}) catch unreachable, __resources.get(text_render_res), -0.9, 0.8);
            if (!is_server) {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{synced_time}) catch unreachable, __resources.get(text_render_res), -0.9, 0.7);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_time}) catch unreachable, __resources.get(text_render_res), -0.9, 0.6);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{sim_timescale}) catch unreachable, __resources.get(text_render_res), -0.9, 0.5);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_velocity}) catch unreachable, __resources.get(text_render_res), -0.9, 0.4);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{draw_rtt}) catch unreachable, __resources.get(text_render_res), -0.9, 0.2);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "resims: {}", .{draw_resims}) catch unreachable, __resources.get(text_render_res), -0.9, 0.1);
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "interp: {d:.6}", .{interpolated_tick_delta}) catch unreachable, __resources.get(text_render_res), -0.9, 0.0);

                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 0, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 0, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), @as(f32, @floatCast(draw_time_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);

                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 500, 400, 100, 2, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 500, 400, 2, 60, zeng.render.color.WHITE);
                zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);
            } else {
                zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accumulator}) catch unreachable, __resources.get(text_render_res), -0.9, 0.7);
            }
            for (str_events.array.items) |str| {
                zeng.render.draw_text(str, __resources.get(text_render_res), 0, 0);
            }
            str_events.clear(allocator);
            for (tri_events.array.items) |tri| {
                zeng.render.debug_draw_triangle(tri, __resources.get(debug_res).*);
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
            zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 0, 0, 6, 6, zeng.render.color.BLACK);
            zeng.render.draw_rect(__graphics, __resources.get(rect_render_res), 0, 0, 4, 4, zeng.render.color.WHITE);
        }
        { // UI processing
            const mouse_state = mouse_state_t{ .pos = .{ .x = @floatFromInt(zeng.global_mouse_pos[0]), .y = @floatFromInt(zeng.global_mouse_pos[1]) } };

            const page = page_widget(uv_checker_tex, uv_checker_tex, @floatFromInt(__graphics.width), @floatFromInt(__graphics.height), 0.3);
            ui_layout(.{ .x = 0, .y = 0 }, page);
            const button = global_id_map.get("play_button");
            if (mouse_over(button, &mouse_state)) {
                zeng.set_cursor(.pointer);
                button.color = .WHITE;
            } else {
                zeng.set_cursor(.arrow);
            }
            ui_draw(&__ui_box, __graphics, page);

            if (render_info == null) {
                render_info = visualize_graph(arena_allocator, page);
            }

            for (render_info.?.nodes) |*node| {
                var sum = vec2.ZERO;
                for (render_info.?.nodes) |*_node| {
                    if (node == _node) continue;
                    sum = sum.add(_node.sub(node.*).normalized());
                }
                sum = sum.normalized().mult(-0.5);
                node.* = node.add(sum);
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
                    __ui_box.draw_img(__graphics, p.x, p.y, 6, 6, .WHITE, 0, 0);
                }
                __ui_box.draw_img(__graphics, pos_a.x, pos_a.y, 15, 15, .BLACK, 0, 4);
                __ui_box.draw_img(__graphics, pos_b.x, pos_b.y, 15, 15, .BLACK, 0, 4);
            }
        }

        commands.process_commands(&world);
        zeng.net.send_net_messages(&commands, __resources.get(time_res).delta_time);

        _ = zeng.c.SwapBuffers(__graphics.hdc);
    }
}

/// Make all entities with a camera and a transform component fly around like a ghost, useful when pausing the simulation
pub fn camera_fly_system(cam: *main_camera_res, world: *ecs.world, q: *ecs.query(.{ zeng.world_matrix, fly_component })) !void {
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
pub fn render_system(world: *ecs.world, cam: *main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const cam_matrix = world.get(cam.id, zeng.world_matrix).?;
    const cam_cam = world.get(cam.id, zeng.camera).?;

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam_matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.render.draw_mesh(mesh.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.render.draw_animated_skinned_mesh(world, skin.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix);
    }
}
/// Run collisions for all players
pub fn player_collision_system(player_q: *ecs.query(.{ player, zeng.world_matrix }), debug: *debug_res, tri_ev: *zeng.events([3]zeng.vec3), spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info))) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        simulate_collision(plyr, world_matrix, spatial_hash_grid, tri_ev, debug);
    }
}
/// Runs player movement simulation and visual animations once per tick
pub fn player_simulate_and_animate_system(datablob: *Datablob, time: *time_res, player_q: *ecs.query(.{ player, rpc.input_message, zeng.world_matrix }), animator_q: *ecs.query(.{ zeng.skeleton, animation_component })) !void {
    const animation_A = datablob.get("assets/gltf/static_test.gltf/animations/idle", zeng.loader.animation);
    const animation_B = datablob.get("assets/gltf/static_test.gltf/animations/run_in_place2", zeng.loader.animation);

    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const _player, const input: *rpc.input_message, const matrix = player_curr;

        simulate_player(_player, input, matrix, time);

        const anim = animator_q.get(_player.animation_controller, animation_component).?;
        const skel = animator_q.get(_player.animation_controller, zeng.skeleton).?;
        const blend = _player.velocity.div(3.0).clamp(1.0).length();

        anim.time += time.fixed_dt / zeng.lerp(animation_A.duration, animation_B.duration, blend);
        while (anim.time > 1.0) {
            anim.time -= 1.0;
        }
        const pose = create_pose(std.heap.c_allocator, skel.bone_parent_indices.len);
        get_animation_pose_with_weight(animation_B, anim.time, pose, blend);
        add_animation_pose_with_weight(animation_A, anim.time, pose, 1.0 - blend);
        normalize_pose_quaternions(pose);
        apply_pose_to_skeleton(skel, pose);
        free_pose(std.heap.c_allocator, pose);
    }
}
/// Collision detection for players - designed to be run multiple times per frame for latency compensation
pub fn simulate_collision(plyr: *player, world_matrix: *zeng.world_matrix, spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)), tri_ev: *zeng.events([3]zeng.vec3), debug: *debug_res) void {
    _ = tri_ev; // autofix
    const b_coll = phy.collider_info{ .data = undefined, .matrix = world_matrix.*, .support = phy.dual_point };
    const old_grounded = plyr.grounded;
    var closest_dist = std.math.floatMax(f32);
    var cloest_point: zeng.vec3 = undefined;
    var combined_normal = zeng.vec3.ZERO;
    var combined_normal_count: usize = 0;
    plyr.grounded = false;

    const right, const left, const up, const down, const forward, const backward = phy.collider_bounds(b_coll);

    var collection = std.ArrayList(std.ArrayList(*phy.collider_info)).initCapacity(std.heap.c_allocator, 0) catch unreachable;
    defer collection.deinit(std.heap.c_allocator);

    var already_checked = std.AutoHashMap(*phy.collider_info, void).init(std.heap.c_allocator);
    defer already_checked.deinit();

    var i: isize = left;
    while (i <= right) {
        defer i += 1;

        var j: isize = down;
        while (j <= up) {
            defer j += 1;

            var k: isize = backward;
            while (k <= forward) {
                defer k += 1;

                // const vec = zeng.vec3{
                //     .x = @as(f32, @floatFromInt(i)) * phy.GRID_SIZE,
                //     .y = @as(f32, @floatFromInt(j)) * phy.GRID_SIZE,
                //     .z = @as(f32, @floatFromInt(k)) * phy.GRID_SIZE,
                // };
                // tri_ev.send(.{ vec, vec.add(zeng.vec3.UP.mult(0.1)), vec.add(zeng.vec3.RIGHT.mult(0.1)) });

                const guy = spatial_hash_grid.get(.{ i, j, k });
                if (guy != null) collection.append(std.heap.c_allocator, guy.?) catch unreachable;
            }
        }
    }

    for (collection.items) |Q| {
        for (Q.items) |coll| {
            if (already_checked.contains(coll)) continue;
            already_checked.put(coll, void{}) catch unreachable;

            if (coll.tag != .support_based) unreachable; // just for now

            // const coll_data = @as(*const phy.mesh_triangle_data, @alignCast(@ptrCast(coll.data)));

            // tri_ev.send(.{
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[0]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[1]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[2]].to_vec4(1.0)).to_vec3(),
            // });

            const p = phy.shape_separation(coll.*, b_coll, debug.*, 10);
            if (p.length() < 0.35) {
                if (p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                    plyr.grounded = true;
                    plyr.ground_normal = p.neg().normalized();
                }
                world_matrix.* = zeng.mat_tran(world_matrix.*, p.add(p.neg().normalized().mult(0.35)));
                combined_normal = combined_normal.add(p.neg().normalized());
                combined_normal_count += 1;
            }
            if (p.length() < closest_dist) {
                cloest_point = p;
                closest_dist = p.length();
            }
        }
    }

    if (old_grounded and !plyr.grounded and closest_dist < 0.5) {
        if (cloest_point.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
            plyr.grounded = true;
            plyr.ground_normal = cloest_point.neg().normalized();
            world_matrix.* = zeng.mat_tran(world_matrix.*, cloest_point.add(cloest_point.neg().normalized().mult(0.35)));
        }
    }
    if (combined_normal_count > 0) {
        plyr.velocity = plyr.velocity.slide(combined_normal);
    }
}
/// Player movement and logic - designed to be multiple times per frame for latency compensation
pub fn simulate_player(_player: *player, input: *const rpc.input_message, matrix: *zeng.world_matrix, time: *time_res) void {
    const rotated_matrix = _player_matrix_from_rotations(input.rot_x, input.rot_y);

    if (input.jump and _player.grounded) {
        _player.velocity = _player.velocity.add(zeng.vec3{ .y = 6 });
        _player.grounded = false;
        _player.ground_normal = zeng.vec3.UP;
    }

    const acc: f32 = 60.0;
    const basis_right = zeng.mat_right(rotated_matrix).slide(_player.ground_normal).normalized();
    const basis_forward = basis_right.cross(_player.ground_normal);
    var move_vect = basis_right.mult(input.move_vect.x).add(basis_forward.mult(input.move_vect.y));

    var tilt = zeng.vec3.ZERO;
    if (_player.grounded) {
        if (input.move_vect.length() > 0.1) {
            if (_player.velocity.length_sq() > 0.01) {
                const g = move_vect.sub(_player.velocity.normalized()).clamp(1.0);
                const h = g.mult(2.0).add(move_vect).normalized();
                var h_v = h.project(_player.velocity);
                const h_h = h.sub(h_v);
                if (_player.velocity.length() > 3.8 and h_v.dot(_player.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                tilt = h_v.add(h_h);
                _player.velocity = _player.velocity.add(h_v.add(h_h).mult(acc * time.fixed_dt));
            } else {
                _player.velocity = _player.velocity.add(move_vect.mult(acc * time.fixed_dt));
            }
        } else {
            tilt = _player.velocity.neg().clamp(1.0);
            _player.velocity = _player.velocity.add(_player.velocity.neg().clamp(acc * time.fixed_dt));
        }
    } else {
        _player.velocity = _player.velocity.add(zeng.vec3.UP.mult(-9.8 * time.fixed_dt));
        _player.ground_normal = zeng.vec3.UP;
        _player.velocity = _player.velocity.add(move_vect.mult(acc * 0.1 * time.fixed_dt));
        _player.velocity = _player.velocity.slide(zeng.vec3.UP).add(_player.velocity.project(zeng.vec3.UP));
    }
    _player.tilt = _player.tilt.lerp(tilt, 8.0 * time.fixed_dt);
    matrix.* = zeng.mat_tran(matrix.*, _player.velocity.mult(time.fixed_dt));

    if (_player.velocity.slide(zeng.vec3.UP).length() > 0.05) {
        _player.old_velocity = _player.old_velocity.slerp(_player.velocity.slide(zeng.vec3.UP).normalized(), 8 * time.fixed_dt);
    }
    if (_player.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
        const _up = (zeng.vec3.UP.add(_player.tilt.mult(0.3))).normalized();
        matrix.* = zeng.mat_rebasis(matrix.*, _up.cross(_player.old_velocity.slide(_up)).normalized(), _up, _player.old_velocity.slide(_up).normalized());
    }

    if (zeng.mat_position(matrix.*).y < -30.0) {
        zeng.mat_position_set(matrix, .{ .y = 20.0, .x = -5.0 });
        _player.velocity = zeng.vec3.ZERO;
    }
}
/// Frame buffers spatial data to be interpolated at a high framerate
pub fn frame_interpolator_tick_system(interp_q: *ecs.query(.{ frame_interpolator, zeng.world_matrix })) !void {
    var interp_it = interp_q.iterator();
    while (interp_it.next()) |curr| {
        const fi: *frame_interpolator, const wm: *zeng.world_matrix = curr;
        fi.store(wm.*);
    }
}

// helper functions
pub fn find_component_of_type(world: *ecs.world, parent: ecs.entity_id, component_type: type, q_children: *ecs.query(.{zeng.children})) ?ecs.entity_id {
    if (world.get(parent, component_type) != null) return parent;

    const childrens = world.get(parent, zeng.children) orelse return null;
    for (childrens.items) |child| {
        const res = find_component_of_type(world, child, component_type, q_children);
        if (res != null) return res;
    }

    return null;
}
pub fn find_component_of_type_actual(world: *ecs.world, parent: ecs.entity_id, component_type: type, q_children: *ecs.query(.{zeng.children})) ?*component_type {
    const guy = world.get(parent, component_type);
    if (guy != null) return guy.?;

    const childrens = world.get(parent, zeng.children) orelse return null;
    for (childrens.items) |child| {
        const res = find_component_of_type_actual(world, child, component_type, q_children);
        if (res != null) return res;
    }

    return null;
}
pub fn binary_search(inputs: []const f32, time: f32) usize {
    if (time <= inputs[0]) return 0;
    if (time >= inputs[inputs.len - 1]) return inputs.len - 2;

    var left: usize = 0;
    var right: usize = inputs.len - 1;

    while (left < right - 1) {
        const mid = left + (right - left) / 2;
        if (inputs[mid] <= time) {
            left = mid;
        } else {
            right = mid;
        }
    }
    return left;
}
pub fn get_animation_pose_with_weight(animation: *zeng.loader.animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight);
        }
    }
}
pub fn add_animation_pose_with_weight(animation: *zeng.loader.animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = rotations[channel.target].add2(channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .translation) {
            translations[channel.target] = translations[channel.target].add(channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .scale) {
            scales[channel.target] = scales[channel.target].add(channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight));
        }
    }
}
pub fn normalize_pose_quaternions(pose: zeng.skeleton_pose) void {
    const rotations = pose[0];
    for (rotations) |*r| {
        r.* = r.normalize();
    }
}
pub fn apply_pose_to_skeleton(skeleton: *zeng.skeleton, pose: zeng.skeleton_pose) void {
    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(pose[0][curr]), zeng.mat_scal(zeng.mat_identity, pose[2][curr])), pose[1][curr]);
        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }
        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}
pub fn matrix_use_rotations(matrix: *zeng.world_matrix, x: f64, y: f64) void {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    matrix.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(matrix.*));
}
pub fn _player_matrix_from_rotations(x: f64, y: f64) zeng.world_matrix {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    return zeng.mat_mult(rot_mat_hor, rot_mat_vert);
}
pub fn create_pose(allocator: std.mem.Allocator, num: usize) zeng.skeleton_pose {
    const rotations = allocator.alloc(zeng.quat, num) catch unreachable;
    const translations = allocator.alloc(zeng.vec3, num) catch unreachable;
    const scales = allocator.alloc(zeng.vec3, num) catch unreachable;

    return .{ rotations, translations, scales };
}
pub fn free_pose(allocator: std.mem.Allocator, pose: zeng.skeleton_pose) void {
    allocator.free(pose[0]);
    allocator.free(pose[1]);
    allocator.free(pose[2]);
}

// MISSING FEATURES:
// implement reliable messages
// add more flexible custom serialization functions to allow for dynamic data
// use dynamic data to send variable input messages
// use dynamic data to send snapshots for every replicated entity

// OPTIONAL REFACTORING:
// clean up events API

// NECESSARY IMPROVEMENTS
// make sure audio is thread-safe, deal with multiple samplerates
// robust text rendering
// better rendering - lights
// better material system
