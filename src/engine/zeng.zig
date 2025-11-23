const zeng = @This();
const std = @import("std");
pub const utils = @import("utils.zig");
pub const ecs = @import("ecs.zig");
pub const rpc = @import("rpc.zig");
pub const net = @import("networking.zig");
pub const gl = @import("gl");
pub const c = @cImport({
    @cInclude("initguid.h");
    @cInclude("windows.h");
    @cInclude("audioclient.h");
    @cInclude("audiopolicy.h");
    @cInclude("mmdeviceapi.h");
    @cInclude("stb_image.h");
    @cInclude("clay.h");
    @cInclude("winsock2.h");
});
pub const loader = @import("loader.zig");
pub const render = @import("render.zig");
pub const aud = @import("audio.zig");
pub const phy = @import("physics.zig");
pub const Player = @import("user/player.zig");

// Engine structs
pub const vec2 = struct {
    x: f32 = 0,
    y: f32 = 0,

    pub fn add(self: vec2, v: vec2) vec2 {
        return .{ .x = self.x + v.x, .y = self.y + v.y };
    }
    pub fn sub(self: vec2, v: vec2) vec2 {
        return .{ .x = self.x - v.x, .y = self.y - v.y };
    }
    pub fn mult(self: vec2, f: f32) vec2 {
        return .{ .x = self.x * f, .y = self.y * f };
    }
    pub fn div(self: vec2, f: f32) vec2 {
        return .{ .x = self.x / f, .y = self.y / f };
    }
    pub fn lerp(a: vec2, b: vec2, t: f32) vec2 {
        return a.mult(1.0 - t).add(b.mult(t));
    }

    pub fn dot(a: vec2, b: vec2) f32 {
        return a.x * b.x + a.y * b.y;
    }
    pub fn neg(v: vec2) vec2 {
        return .{ .x = -v.x, .y = -v.y };
    }

    pub fn length(self: vec2) f32 {
        return @sqrt(self.x * self.x + self.y * self.y);
    }
    pub fn length_sq(self: vec2) f32 {
        return self.x * self.x + self.y * self.y;
    }
    pub fn normalized(self: vec2) vec2 {
        return self.div(self.length());
    }

    pub fn perp(this: vec2) vec2 {
        return vec2{ .x = -this.y, .y = this.x };
    }
    pub fn perp_toward(this: vec2, v: vec2) vec2 {
        if (v.dot(this.perp()) < 0) return this.perp().neg();
        return this.perp();
    }
    pub fn clamp(this: vec2, mag: f32) vec2 {
        if (this.length() > mag) return this.normalized().mult(mag);
        return this;
    }

    pub const ZERO = vec2{ .x = 0, .y = 0 };
};
pub const vec3 = extern struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,

    pub fn mult(self: vec3, f: f32) vec3 {
        return .{ .x = self.x * f, .y = self.y * f, .z = self.z * f };
    }
    pub fn div(self: vec3, f: f32) vec3 {
        return .{ .x = self.x / f, .y = self.y / f, .z = self.z / f };
    }
    pub fn add(self: vec3, v: vec3) vec3 {
        return .{ .x = self.x + v.x, .y = self.y + v.y, .z = self.z + v.z };
    }
    pub fn sub(self: vec3, v: vec3) vec3 {
        return .{ .x = self.x - v.x, .y = self.y - v.y, .z = self.z - v.z };
    }
    pub fn length(self: vec3) f32 {
        return @sqrt(self.x * self.x + self.y * self.y + self.z * self.z);
    }
    pub fn length_sq(self: vec3) f32 {
        return self.x * self.x + self.y * self.y + self.z * self.z;
    }
    pub fn normalized(self: vec3) vec3 {
        return self.div(self.length());
    }
    pub fn lerp(a: vec3, b: vec3, t: f32) vec3 {
        return a.mult(1.0 - t).add(b.mult(t));
    }
    pub fn slerp(a: vec3, b: vec3, t: f32) vec3 {
        var _dot = a.dot(b);
        _dot = std.math.clamp(_dot, -1.0, 1.0);

        const theta = std.math.acos(_dot) * t;
        const relative = b.sub(a.mult(_dot)).normalized();
        return a.mult(@cos(theta)).add(relative.mult(@sin(theta)));
    }
    pub fn neg(v: vec3) vec3 {
        return .{ .x = -v.x, .y = -v.y, .z = -v.z };
    }
    pub fn dot(a: vec3, b: vec3) f32 {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }
    pub fn cross(a: vec3, b: vec3) vec3 {
        return vec3{
            .x = a.y * b.z - a.z * b.y,
            .y = a.z * b.x - a.x * b.z,
            .z = a.x * b.y - a.y * b.x,
        };
    }
    pub fn project(lhs: vec3, rhs: vec3) vec3 {
        return rhs.mult(lhs.dot(rhs) / rhs.dot(rhs));
    }
    pub fn slide(lhs: vec3, rhs: vec3) vec3 {
        return lhs.sub(lhs.project(rhs));
    }
    pub fn project_s(lhs: vec3, rhs: vec3) vec3 {
        return lhs.dot(rhs) / rhs.dot(rhs);
    }
    pub fn reject_out(lhs: vec3, rhs: vec3) vec3 {
        return lhs.sub(lhs.project(rhs));
    }
    pub fn to_vec4(this: vec3, extra: f32) vec4 {
        return vec4{ .x = this.x, .y = this.y, .z = this.z, .w = extra };
    }
    pub fn clamp(this: vec3, mag: f32) vec3 {
        if (this.length() > mag) return this.normalized().mult(mag);
        return this;
    }

    pub const ZERO = vec3{};
    pub const ONE = vec3{ .x = 1.0, .y = 1.0, .z = 1.0 };
    pub const RIGHT = vec3{ .x = 1.0 };
    pub const UP = vec3{ .y = 1.0 };
    pub const FORWARD = vec3{ .z = 1.0 };
};
pub const quat = packed struct {
    x: f32 = 0.0,
    y: f32 = 0.0,
    z: f32 = 0.0,
    w: f32 = 0.0,
    pub fn add(self: quat, other: quat) quat {
        return quat{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
            .w = self.w + other.w,
        };
    }
    pub fn add2(self: quat, other: quat) quat {
        const _dot = quat.dot(self, other);
        var b = other;
        if (_dot < 0.0) {
            b = quat{
                .x = -other.x,
                .y = -other.y,
                .z = -other.z,
                .w = -other.w,
            };
        }
        return quat{
            .x = self.x + b.x,
            .y = self.y + b.y,
            .z = self.z + b.z,
            .w = self.w + b.w,
        };
    }
    pub fn mult(self: quat, scalar: f32) quat {
        return quat{
            .x = self.x * scalar,
            .y = self.y * scalar,
            .z = self.z * scalar,
            .w = self.w * scalar,
        };
    }
    pub fn lenSq(self: quat) f32 {
        return self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w;
    }
    pub fn normalize(self: quat) quat {
        var len = self.lenSq();
        if (len == 0.0) return self; // Avoid division by zero
        len = @sqrt(len);
        return quat{
            .x = self.x / len,
            .y = self.y / len,
            .z = self.z / len,
            .w = self.w / len,
        };
    }
    pub fn dot(a: quat, b: quat) f32 {
        return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w;
    }
    pub fn nlerp(a: quat, b: quat, t: f32) quat {
        const q1 = a;
        var q2 = b;

        const _dot = quat.dot(q1, q2);

        if (_dot < 0.0) {
            q2 = quat{
                .x = -q2.x,
                .y = -q2.y,
                .z = -q2.z,
                .w = -q2.w,
            };
        }
        return normalize(quat{
            .x = q1.x + t * (q2.x - q1.x),
            .y = q1.y + t * (q2.y - q1.y),
            .z = q1.z + t * (q2.z - q1.z),
            .w = q1.w + t * (q2.w - q1.w),
        });
    }

    pub const IDENTITY = quat{ .x = 0, .y = 0, .z = 0, .w = 1.0 };
};
pub const material = struct {
    shader_program: u32,
    texture: u32,
    // parameter_map: hashmap(string, *anyopaque)
};
pub const vec4 = packed struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,
    w: f32 = 0,

    pub fn to_vec3(this: vec4) vec3 {
        return vec3{ .x = this.x, .y = this.y, .z = this.z };
    }
};

// Engine components
pub const mesh = struct {
    vao_gpu: u32,
    indices_length: i32,
    indices_type: gl.GLenum,
    material: material,
};
pub const camera = struct {
    projection_matrix: [16]f32,
};
pub const world_matrix = [16]f32;
pub const skeleton = struct {
    bone_parent_indices: []isize,
    inverse_bind_matrices: []zeng.world_matrix,
    local_bone_matrices: []zeng.world_matrix,
    model_bone_matrices: []zeng.world_matrix,
    // animations: std.ArrayList(usize),
    default_bone_translations: []zeng.vec3,
    default_bone_rotations: []zeng.quat,
    default_bone_scales: []zeng.vec3,
};
pub const skinned_mesh = struct {
    vao_gpu: u32,
    indices_length: i32,
    indices_type: gl.GLenum,
    material: material,
    skeleton: ecs.entity_id,
};
pub const cpu_mesh = struct {
    indices: []u32,
    positions: []vec3,
};
pub const skeleton_pose = struct { []zeng.quat, []zeng.vec3, []zeng.vec3 };

pub var global_world_ptr: *ecs.world = undefined;
pub var global_player_entity: ecs.entity_id = undefined;
pub var global_camera_entity: ecs.entity_id = undefined;
pub var global_mouse_pressed = false;
pub var global_allocator: std.mem.Allocator = undefined;

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
pub const cube_tracker_res = struct {
    map: std.AutoHashMap(phy.ivec3, void),
    cube_mesh: zeng.mesh,
    cube_mesh_data: *const anyopaque,
};
pub const main_player_res = struct {
    id: ecs.entity_id,
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
pub const net_id_component = struct {
    net_id: u32,
    remote_peer: ?net.peer_info_t,
};

pub const client_info = struct {
    input_buffer: zeng.ring_buffer(rpc.input_message),
    player: ecs.entity_id,
};

pub const name_component = []const u8;

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
    zeng.Player.player,
    animation_component,
    zeng.skeleton,
    input_implement,
    rpc.input_message,
    floater_component,
    snapshot_interpolator,
    frame_interpolator,
    net_id_component,
    name_component,
};

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
pub fn apply_pose_to_skeleton(_skeleton: *zeng.skeleton, pose: zeng.skeleton_pose) void {
    var curr: usize = 0;
    while (curr < _skeleton.bone_parent_indices.len) {
        _skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(pose[0][curr]), zeng.mat_scal(zeng.mat_identity, pose[2][curr])), pose[1][curr]);
        const parent_index = _skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            _skeleton.local_bone_matrices[curr] = zeng.mat_mult(_skeleton.local_bone_matrices[@intCast(parent_index)], _skeleton.local_bone_matrices[curr]);
        }
        _skeleton.model_bone_matrices[curr] = zeng.mat_mult(_skeleton.local_bone_matrices[curr], _skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}
pub fn matrix_use_rotations(matrix: *zeng.world_matrix, x: f64, y: f64) void {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    matrix.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(matrix.*));
}
pub fn create_pose(allocator: std.mem.Allocator, num: usize, _skeleton: zeng.skeleton) zeng.skeleton_pose {
    const rotations = allocator.alloc(zeng.quat, num) catch unreachable;
    const translations = allocator.alloc(zeng.vec3, num) catch unreachable;
    const scales = allocator.alloc(zeng.vec3, num) catch unreachable;

    @memcpy(translations, _skeleton.default_bone_translations);
    @memcpy(rotations, _skeleton.default_bone_rotations);
    @memcpy(scales, _skeleton.default_bone_scales);

    return .{ rotations, translations, scales };
}
pub fn free_pose(allocator: std.mem.Allocator, pose: zeng.skeleton_pose) void {
    allocator.free(pose[0]);
    allocator.free(pose[1]);
    allocator.free(pose[2]);
}

// Math
pub fn quat_to_mat(q: quat) [16]f32 {
    var matrix: [16]f32 = undefined;
    // column 0
    matrix[0] = 1.0 - 2.0 * (q.y * q.y) - 2.0 * (q.z * q.z);
    matrix[1] = 2.0 * q.x * q.y + 2.0 * q.z * q.w;
    matrix[2] = 2.0 * q.x * q.z - 2.0 * q.y * q.w;
    matrix[3] = 0.0;
    // column 1
    matrix[4] = 2.0 * q.x * q.y - 2.0 * q.z * q.w;
    matrix[5] = 1.0 - 2.0 * (q.x * q.x) - 2.0 * (q.z * q.z);
    matrix[6] = 2.0 * q.y * q.z + 2.0 * q.x * q.w;
    matrix[7] = 0.0;
    // column 2
    matrix[8] = 2.0 * q.x * q.z + 2.0 * q.y * q.w;
    matrix[9] = 2.0 * q.y * q.z - 2.0 * q.x * q.w;
    matrix[10] = 1.0 - 2.0 * (q.x * q.x) - 2.0 * (q.y * q.y);
    matrix[11] = 0.0;
    // column 3
    matrix[12] = 0.0;
    matrix[13] = 0.0;
    matrix[14] = 0.0;
    matrix[15] = 1.0;
    return matrix;
}
pub fn quat_axis_angle(axis: vec3, angle: f32) quat {
    return .{
        .x = axis.x * @sin(angle / 2.0), //
        .y = axis.y * @sin(angle / 2.0), //
        .z = axis.z * @sin(angle / 2.0), //
        .w = @cos(angle / 2.0),
    };
}
pub fn inv_lerp(a: f32, b: f32, v: f32) f32 {
    if (a == b) return 0.0; // Avoid division by zero; undefined behavior for constant ranges.
    return (v - a) / (b - a);
}
pub fn lerp(a: anytype, b: anytype, v: anytype) @TypeOf(a) {
    return a + (b - a) * v;
}

// Matrices
pub const mat_identity = [16]f32{
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
};

pub fn mat_right(t: [16]f32) vec3 {
    return vec3{ .x = t[0], .y = t[1], .z = t[2] };
}
pub fn mat_up(t: [16]f32) vec3 {
    return vec3{ .x = t[4], .y = t[5], .z = t[6] };
}
pub fn mat_forward(t: [16]f32) vec3 {
    return vec3{ .x = t[8], .y = t[9], .z = t[10] };
}
pub fn mat_tran(a: [16]f32, v: vec3) [16]f32 {
    var b = a;
    b[12] += v.x;
    b[13] += v.y;
    b[14] += v.z;
    return b;
}
pub fn mat_scal(a: [16]f32, v: vec3) [16]f32 {
    var b = a;
    b[0] *= v.x; // Scale the x-axis
    b[5] *= v.y; // Scale the y-axis
    b[10] *= v.z; // Scale the z-axis
    return b;
}
pub fn mat_mult(b: [16]f32, a: [16]f32) [16]f32 {
    var result: [16]f32 = undefined;

    for (0..4) |row| {
        for (0..4) |col| {
            var sum: f32 = 0.0;
            for (0..4) |idx| {
                sum += a[row * 4 + idx] * b[idx * 4 + col];
            }
            result[row * 4 + col] = sum;
        }
    }

    return result;
}
pub fn mat_mult_vec3(m: [16]f32, v: vec3) vec3 {
    return vec3{
        .x = m[0] * v.x + m[4] * v.y + m[8] * v.z + m[12],
        .y = m[1] * v.x + m[5] * v.y + m[9] * v.z + m[13],
        .z = m[2] * v.x + m[6] * v.y + m[10] * v.z + m[14],
    };
}
pub fn mat_mult_vec4(m: [16]f32, v: vec4) vec4 {
    return vec4{
        .x = m[0] * v.x + m[4] * v.y + m[8] * v.z + m[12] * v.w,
        .y = m[1] * v.x + m[5] * v.y + m[9] * v.z + m[13] * v.w,
        .z = m[2] * v.x + m[6] * v.y + m[10] * v.z + m[14] * v.w,
        .w = m[3] * v.x + m[7] * v.y + m[11] * v.z + m[15] * v.w,
    };
}
pub fn mat_invert(m: [16]f32) [16]f32 {
    var inv: [16]f32 = undefined;

    inv[0] =
        m[5] * m[10] * m[15] -
        m[5] * m[11] * m[14] -
        m[9] * m[6] * m[15] +
        m[9] * m[7] * m[14] +
        m[13] * m[6] * m[11] -
        m[13] * m[7] * m[10];

    inv[4] =
        -m[4] * m[10] * m[15] +
        m[4] * m[11] * m[14] +
        m[8] * m[6] * m[15] -
        m[8] * m[7] * m[14] -
        m[12] * m[6] * m[11] +
        m[12] * m[7] * m[10];

    inv[8] =
        m[4] * m[9] * m[15] -
        m[4] * m[11] * m[13] -
        m[8] * m[5] * m[15] +
        m[8] * m[7] * m[13] +
        m[12] * m[5] * m[11] -
        m[12] * m[7] * m[9];

    inv[12] =
        -m[4] * m[9] * m[14] +
        m[4] * m[10] * m[13] +
        m[8] * m[5] * m[14] -
        m[8] * m[6] * m[13] -
        m[12] * m[5] * m[10] +
        m[12] * m[6] * m[9];

    inv[1] =
        -m[1] * m[10] * m[15] +
        m[1] * m[11] * m[14] +
        m[9] * m[2] * m[15] -
        m[9] * m[3] * m[14] -
        m[13] * m[2] * m[11] +
        m[13] * m[3] * m[10];

    inv[5] =
        m[0] * m[10] * m[15] -
        m[0] * m[11] * m[14] -
        m[8] * m[2] * m[15] +
        m[8] * m[3] * m[14] +
        m[12] * m[2] * m[11] -
        m[12] * m[3] * m[10];

    inv[9] =
        -m[0] * m[9] * m[15] +
        m[0] * m[11] * m[13] +
        m[8] * m[1] * m[15] -
        m[8] * m[3] * m[13] -
        m[12] * m[1] * m[11] +
        m[12] * m[3] * m[9];

    inv[13] =
        m[0] * m[9] * m[14] -
        m[0] * m[10] * m[13] -
        m[8] * m[1] * m[14] +
        m[8] * m[2] * m[13] +
        m[12] * m[1] * m[10] -
        m[12] * m[2] * m[9];

    inv[2] =
        m[1] * m[6] * m[15] -
        m[1] * m[7] * m[14] -
        m[5] * m[2] * m[15] +
        m[5] * m[3] * m[14] +
        m[13] * m[2] * m[7] -
        m[13] * m[3] * m[6];

    inv[6] =
        -m[0] * m[6] * m[15] +
        m[0] * m[7] * m[14] +
        m[4] * m[2] * m[15] -
        m[4] * m[3] * m[14] -
        m[12] * m[2] * m[7] +
        m[12] * m[3] * m[6];

    inv[10] =
        m[0] * m[5] * m[15] -
        m[0] * m[7] * m[13] -
        m[4] * m[1] * m[15] +
        m[4] * m[3] * m[13] +
        m[12] * m[1] * m[7] -
        m[12] * m[3] * m[5];

    inv[14] =
        -m[0] * m[5] * m[14] +
        m[0] * m[6] * m[13] +
        m[4] * m[1] * m[14] -
        m[4] * m[2] * m[13] -
        m[12] * m[1] * m[6] +
        m[12] * m[2] * m[5];

    inv[3] =
        -m[1] * m[6] * m[11] +
        m[1] * m[7] * m[10] +
        m[5] * m[2] * m[11] -
        m[5] * m[3] * m[10] -
        m[9] * m[2] * m[7] +
        m[9] * m[3] * m[6];

    inv[7] =
        m[0] * m[6] * m[11] -
        m[0] * m[7] * m[10] -
        m[4] * m[2] * m[11] +
        m[4] * m[3] * m[10] +
        m[8] * m[2] * m[7] -
        m[8] * m[3] * m[6];

    inv[11] =
        -m[0] * m[5] * m[11] +
        m[0] * m[7] * m[9] +
        m[4] * m[1] * m[11] -
        m[4] * m[3] * m[9] -
        m[8] * m[1] * m[7] +
        m[8] * m[3] * m[5];

    inv[15] =
        m[0] * m[5] * m[10] -
        m[0] * m[6] * m[9] -
        m[4] * m[1] * m[10] +
        m[4] * m[2] * m[9] +
        m[8] * m[1] * m[6] -
        m[8] * m[2] * m[5];

    var det: f32 = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

    if (det == 0.0)
        return inv;

    det = 1.0 / det;

    var invOut: [16]f32 = undefined;

    for (0..16) |i| {
        invOut[i] = inv[i] * det;
    }

    return invOut;
}
pub fn mat_position(t: [16]f32) vec3 {
    return vec3{ .x = t[12], .y = t[13], .z = t[14] };
}
pub fn mat_position_set(t: *[16]f32, v: vec3) void {
    t[12] = v.x;
    t[13] = v.y;
    t[14] = v.z;
}
pub fn mat_perspective_projection(fov: f32, aspect_ratio: f32, near: f32, far: f32) [16]f32 {
    const f = 1.0 / @tan(fov / 2.0);
    const range_inv = 1.0 / (near - far);

    var result: [16]f32 = undefined;

    result[0] = f / aspect_ratio;
    result[1] = 0.0;
    result[2] = 0.0;
    result[3] = 0.0;

    result[4] = 0.0;
    result[5] = f;
    result[6] = 0.0;
    result[7] = 0.0;

    result[8] = 0.0;
    result[9] = 0.0;
    result[10] = (near + far) * range_inv;
    result[11] = -1.0;

    result[12] = 0.0;
    result[13] = 0.0;
    result[14] = 2.0 * near * far * range_inv;
    result[15] = 0.0;

    return result;
}
pub fn mat_axis_angle(axis: vec3, angle: f32) [16]f32 {
    const cosine = @cos(angle);
    const s = @sin(angle);
    const t = 1.0 - cosine;

    const x = axis.x;
    const y = axis.y;
    const z = axis.z;

    var result: [16]f32 = undefined;

    result[0] = t * x * x + cosine;
    result[1] = t * x * y + s * z;
    result[2] = t * x * z - s * y;
    result[3] = 0.0;

    result[4] = t * x * y - s * z;
    result[5] = t * y * y + cosine;
    result[6] = t * y * z + s * x;
    result[7] = 0.0;

    result[8] = t * x * z + s * y;
    result[9] = t * y * z - s * x;
    result[10] = t * z * z + cosine;
    result[11] = 0.0;

    result[12] = 0.0;
    result[13] = 0.0;
    result[14] = 0.0;
    result[15] = 1.0;

    return result;
}
pub fn mat3_determinant_col_major(m: [9]f32) f32 {
    return m[0] * (m[4] * m[8] - m[5] * m[7]) - m[3] * (m[1] * m[8] - m[2] * m[7]) + m[6] * (m[1] * m[5] - m[2] * m[4]);
}
pub fn mat4_minor3x3(m: [16]f32, i: usize, j: usize) [9]f32 {
    var result: [9]f32 = undefined;
    var dst_index: usize = 0;

    for (0..4) |col| {
        if (col == j) continue; // skip the j-th column

        for (0..4) |row| {
            if (row == i) continue; // skip the i-th row

            result[dst_index] = m[col * 4 + row]; // column-major access
            dst_index += 1;
        }
    }

    return result;
}
pub fn mat4_from_vectors(right: vec3, up: vec3, forward: vec3, position: vec3) [16]f32 {
    return [16]f32{
        right.x,    right.y,    right.z,    0.0,
        up.x,       up.y,       up.z,       0.0,
        forward.x,  forward.y,  forward.z,  0.0,
        position.x, position.y, position.z, 1.0,
    };
}
pub fn mat_rebasis(mat: [16]f32, right: vec3, up: vec3, forward: vec3) [16]f32 {
    return [16]f32{
        right.x,   right.y,   right.z,   0.0,
        up.x,      up.y,      up.z,      0.0,
        forward.x, forward.y, forward.z, 0.0,
        mat[12],   mat[13],   mat[14],   1.0,
    };
}
pub fn mat_to_quat(mat: [16]f32) quat {
    const m00 = mat[0];
    const m01 = mat[4];
    const m02 = mat[8];
    const m10 = mat[1];
    const m11 = mat[5];
    const m12 = mat[9];
    const m20 = mat[2];
    const m21 = mat[6];
    const m22 = mat[10];

    const trace = m00 + m11 + m22;
    var q: quat = undefined;

    if (trace > 0.0) {
        const s = @sqrt(trace + 1.0) * 2.0; // S = 4 * qw
        q.w = 0.25 * s;
        q.x = (m21 - m12) / s;
        q.y = (m02 - m20) / s;
        q.z = (m10 - m01) / s;
    } else if (m00 > m11 and m00 > m22) {
        const s = @sqrt(1.0 + m00 - m11 - m22) * 2.0; // S = 4 * qx
        q.w = (m21 - m12) / s;
        q.x = 0.25 * s;
        q.y = (m01 + m10) / s;
        q.z = (m02 + m20) / s;
    } else if (m11 > m22) {
        const s = @sqrt(1.0 + m11 - m00 - m22) * 2.0; // S = 4 * qy
        q.w = (m02 - m20) / s;
        q.x = (m01 + m10) / s;
        q.y = 0.25 * s;
        q.z = (m12 + m21) / s;
    } else {
        const s = @sqrt(1.0 + m22 - m00 - m11) * 2.0; // S = 4 * qz
        q.w = (m10 - m01) / s;
        q.x = (m02 + m20) / s;
        q.y = (m12 + m21) / s;
        q.z = 0.25 * s;
    }

    return q;
}

// OpenGl
pub fn gl_log_errors() void {
    var err: zeng.gl.GLenum = zeng.gl.getError();
    while (err != zeng.gl.NO_ERROR) {
        const errorString = switch (err) {
            zeng.gl.INVALID_ENUM => "INVALID_ENUM",
            zeng.gl.INVALID_VALUE => "INVALID_VALUE",
            zeng.gl.INVALID_OPERATION => "INVALID_OPERATION",
            zeng.gl.OUT_OF_MEMORY => "OUT_OF_MEMORY",
            zeng.gl.INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
            else => "unknown error",
        };

        std.log.err("gl_log_errors() found: {s}", .{errorString});

        err = zeng.gl.getError();
    }
}

// Application
pub const __graphics_module = struct {
    width: u16 = 0,
    height: u16 = 0,

    hwnd: [*c]c.struct_HWND__,
    hdc: [*c]c.struct_HDC__,

    pub fn init(this: *@This()) void {
        const hInstance = zeng.c.GetModuleHandleW(null);

        var wc: zeng.c.WNDCLASSW = std.mem.zeroes(zeng.c.WNDCLASSW);
        wc.lpfnWndProc = zeng.windows_message_handler;
        wc.hInstance = hInstance;
        wc.lpszClassName = L("MyZigWindowClass");

        if (zeng.c.RegisterClassW(&wc) == 0) unreachable; // failed to register window class

        const hwnd = zeng.c.CreateWindowExW(0, wc.lpszClassName, L("window title"), zeng.c.WS_OVERLAPPEDWINDOW | zeng.c.WS_VISIBLE, zeng.c.CW_USEDEFAULT, zeng.c.CW_USEDEFAULT, 800, 450, null, null, hInstance, null);
        if (hwnd == null) unreachable; // failed to create window
        this.hwnd = hwnd;
        var client_rect: c.RECT = undefined;
        _ = c.GetClientRect(this.hwnd, &client_rect);
        this.width = @intCast(client_rect.right - client_rect.left);
        this.height = @intCast(client_rect.bottom - client_rect.top);

        // raw input setup
        var rid: zeng.c.RAWINPUTDEVICE = .{
            .usUsagePage = 0x01,
            .usUsage = 0x02,
            .dwFlags = 0,
            .hwndTarget = hwnd,
        };
        if (zeng.c.RegisterRawInputDevices(&rid, 1, @sizeOf(zeng.c.RAWINPUTDEVICE)) == 0) unreachable; // failed to register for raw input
        // end raw input setup

        // opengl setup
        const hdc = zeng.c.GetDC(hwnd);
        this.hdc = hdc;
        var pfd: zeng.c.PIXELFORMATDESCRIPTOR = std.mem.zeroes(zeng.c.PIXELFORMATDESCRIPTOR);
        pfd.nSize = @sizeOf(zeng.c.PIXELFORMATDESCRIPTOR);
        pfd.nVersion = 1;
        pfd.dwFlags = zeng.c.PFD_DRAW_TO_WINDOW | zeng.c.PFD_SUPPORT_OPENGL | zeng.c.PFD_DOUBLEBUFFER;
        pfd.iPixelType = zeng.c.PFD_TYPE_RGBA;
        pfd.cColorBits = 32;
        pfd.cDepthBits = 24;
        pfd.cStencilBits = 8;
        pfd.iLayerType = zeng.c.PFD_MAIN_PLANE;

        const pf = zeng.c.ChoosePixelFormat(hdc, &pfd);
        if (pf == 0 or zeng.c.SetPixelFormat(hdc, pf, &pfd) == 0) unreachable; // failed to set pixel format

        const hglrc = zeng.c.wglCreateContext(hdc);
        if (hglrc == null or zeng.c.wglMakeCurrent(hdc, hglrc) == 0) unreachable; // failed to create opengl context
        // end opengl setup

        const user32 = zeng.c.LoadLibraryA("opengl32.dll");
        if (user32 == null) unreachable; // failed to load opengl32.dll

        gl.load(user32, zeng.get_proc_address) catch unreachable;

        gl.enable(gl.DEPTH_TEST);
        gl.enable(gl.CULL_FACE);
        gl.enable(gl.BLEND);
        gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
        gl.enable(gl.FRAMEBUFFER_SRGB);

        zeng.timer_warmup();
        zeng.old_time = zeng.timer_get();
    }
    pub fn deinit(this: *@This()) void {
        _ = this;
    }
};
pub fn window_resize_handler(width: u32, height: u32) void {
    zeng.gl.viewport(0, 0, @bitCast(width), @bitCast(height));

    const cam = global_world_ptr.get(global_camera_entity, zeng.camera).?;
    cam.projection_matrix = zeng.mat_perspective_projection(1.5, @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height)), 0.01, 1000.0);
}
fn key_callback(window: zeng.glfw.Window, key: zeng.glfw.Key, scancode: i32, action: zeng.glfw.Action, mods: zeng.glfw.Mods) void {
    _ = key; // autofix
    _ = window; // autofix
    _ = scancode; // autofix
    _ = action; // autofix
    _ = mods; // autofix
}

// Iterators + Resources
pub const resource_fetcher = struct {
    res: *resources_t,
    world: *ecs.world,
    allocator: std.mem.Allocator,

    pub fn run_system(self: *resource_fetcher, comptime func: anytype) void {
        const t = @typeInfo(@TypeOf(func));

        const typ = comptime utils.type_array_to_tuple_type(utils.fn_parameter_type_array(t));
        var params: typ = undefined;

        inline for (&params) |*param| {
            if (@hasDecl(@TypeOf(param.*.*), "TYPES")) {
                const component_list = comptime @TypeOf(param.*.*).TYPES;
                param.* = self.fresh_query(component_list);
            } else {
                param.* = self.res.get(@TypeOf(param.*.*));
            }
        }
        @call(.auto, func, params) catch unreachable;
    }
    pub fn fresh_query(self: *resource_fetcher, component_list: anytype) *ecs.query(component_list) {
        const q_ptr, const undef = self.res.get_create(ecs.query(component_list));
        if (undef) {
            // an undefined query was allocated
            q_ptr.* = try ecs.query(component_list).create(self.world, self.allocator);
        } else {
            // query was found, but we want to refresh it
            try q_ptr.destroy();
            q_ptr.* = try ecs.query(component_list).create(self.world, self.allocator);
        }
        return q_ptr;
    }
};

pub const resources_t = struct {
    map: std.AutoArrayHashMap(usize, *anyopaque),
    allocator: std.mem.Allocator,

    pub fn init(this: *@This(), allocator: std.mem.Allocator, __graphics: *__graphics_module) void {
        this.map = std.AutoArrayHashMap(usize, *anyopaque).init(allocator);
        this.allocator = allocator;
        _ = c.SetWindowLongPtrW(__graphics.hwnd, c.GWLP_USERDATA, @intCast(@intFromPtr(this)));
        this.insert_ptr(__graphics);
    }
    pub fn deinit(self: *resources_t) void {
        self.map.deinit();
    }

    pub fn insert(self: *resources_t, p: anytype) void {
        const new_guy = self.allocator.create(@TypeOf(p)) catch unreachable;
        new_guy.* = p;

        const a = self.map.getOrPut(utils.type_id(@TypeOf(p))) catch unreachable;
        if (a.found_existing) {
            const ref = @as(*@TypeOf(p), @ptrCast(@alignCast(a.value_ptr.*)));
            self.allocator.destroy(ref);
        }
        a.value_ptr.* = @ptrCast(new_guy);
    }
    pub fn insert_ptr(self: *resources_t, p: anytype) void {
        const erased = @as(*anyopaque, @ptrCast(p));
        const type_id = utils.type_id(@typeInfo(@TypeOf(p)).pointer.child);
        self.map.put(type_id, erased) catch unreachable;
    }
    pub fn get(resources: *resources_t, p: type) *p {
        if (!resources.map.contains(utils.type_id(p))) {
            std.debug.print("TRIED TO FETCH A RESOURCE THAT DOESNT EXIST IN REGISTRY: {}\n", .{p});
            unreachable;
        }
        return @ptrCast(@alignCast(resources.map.getPtr(utils.type_id(p)).?.*));
    }
    pub fn get_create(self: *resources_t, p: type) struct { *p, bool } {
        var gotten: *p = undefined;
        var undef = false;
        if (self.map.contains(utils.type_id(p))) {
            gotten = @ptrCast(@alignCast(self.map.get(utils.type_id(p)).?));
        } else {
            undef = true;
            const new_guy = self.allocator.create(p) catch unreachable;
            self.map.put(utils.type_id(p), @ptrCast(new_guy)) catch unreachable;
            gotten = new_guy;
        }
        return .{ gotten, undef };
    }
};

// Commands
pub fn GET_PROC_CODE(comptime func: anytype) u32 {
    var count: u32 = 0;
    for (rpc.REMOTE_PROCEDURES) |proc| {
        if (@as(*const anyopaque, @ptrCast(&proc)) == @as(*const anyopaque, @ptrCast(&func))) {
            return count;
        }
        count += 1;
    }
    @compileError("invalid procedure");
}
pub fn GET_MSG_CODE(T: type) u32 {
    var count: u32 = 0;
    for (rpc.REMOTE_MESSAGE_TYPES) |msg_type| {
        if (msg_type == T) {
            return count;
        }
        count += 1;
    }
    @compileError("invalid remote message type!");
}
pub const commands = struct {
    pub const Command = struct {
        stuff: [256]u8,
        size: u32,
        id: u64,
        kind: command_type,
    };
    const command_type = enum(u8) {
        spawn,
        insert,
        empty,
    };
    allocator: std.mem.Allocator,
    queued_commands: [1024]Command = undefined,
    queued_commands_curr: u32 = 0,

    remote_messages_send_queue: [4000]remote_message,
    remote_messages_send_queue_len: u16,

    // reliable_resend_queue: std.ArrayList(remote_message),
    // reliable_message_seqs: std.AutoHashMap(usize, remote_message),
    // reliable_resend_queue: [100]remote_message,

    // curr_seq: usize = 1,

    time: f64 = 0.0,

    // last_recieved_seq: usize = 0,
    // ack_bits: u32 = 0,

    random: std.Random,

    /// queues the spawning of an entity until sometime later in this frame
    pub fn spawn(self: *commands, payload: anytype) void {
        self.add_command_type(.spawn);
        inline for (payload) |elem| {
            self.add_insertion_command(elem);
        }
        self.add_command_type(.empty);
    }
    pub fn insert() void {}
    pub fn remove() void {}

    // command implementation
    fn add_insertion_command(self: *commands, payload: anytype) void {
        self.queued_commands[self.queued_commands_curr] = Command{ .size = @sizeOf(@TypeOf(payload)), .id = comptime ecs.COMP_TYPE_TO_ID(@TypeOf(payload)), .kind = .insert, .stuff = undefined };
        @memcpy(@as([*]u8, @ptrCast(&self.queued_commands[self.queued_commands_curr].stuff)), @as([*]const u8, @ptrCast(&payload))[0..@sizeOf(@TypeOf(payload))]);
        self.queued_commands_curr += 1;
    }
    fn add_command_type(self: *commands, t: command_type) void {
        self.queued_commands[self.queued_commands_curr].kind = t;
        self.queued_commands_curr += 1;
    }
    pub fn process_commands(self: *commands, world: *ecs.world) void {
        var curr: u32 = 0;
        var current_ent: ecs.entity_id = undefined;

        while (curr < self.queued_commands_curr) {
            defer curr += 1;

            if (self.queued_commands[curr].kind == .spawn) {
                current_ent = world.spawn(.{});
                continue;
            }
            if (self.queued_commands[curr].kind == .insert) {
                world.add_runtime(ecs.__runtime_type_information[self.queued_commands[curr].id], &self.queued_commands[curr].stuff, current_ent) catch unreachable;
                continue;
            }
            if (self.queued_commands[curr].kind == .empty) {
                current_ent = undefined;
            }
        }

        self.queued_commands_curr = 0;
    }

    // networking
    /// queues a remote procedure call to be sent to destination at the end of the current frame.
    pub fn remote_call(self: *commands, socket: net.socket_t, address: net.Address, comptime procedure: anytype, _args: anytype) void {
        const procedure_code: u32 = comptime GET_PROC_CODE(procedure);

        const args: blk: {
            if (@typeInfo(std.meta.ArgsTuple(@TypeOf(procedure))).Struct.fields.len > 0) {
                break :blk std.meta.ArgsTuple(@TypeOf(procedure));
            } else {
                break :blk @TypeOf(_args);
            }
        } = _args;

        var payload_array = self.allocator.alloc(u8, 4 + @sizeOf(@TypeOf(args))) catch unreachable;
        var payload_curr: u32 = 0;
        zeng.loader.serialize_to_bytes(procedure_code, payload_array, &payload_curr);
        zeng.loader.serialize_to_bytes(args, payload_array, &payload_curr);

        self.remote_messages_send_queue[self.remote_messages_send_queue_len] = remote_message{ .payload = payload_array[0..payload_curr], .sender_socket = socket, .target_address = address };
        self.remote_messages_send_queue_len += 1;
    }

    pub const reliability_channel = enum {
        unreliable,
        reliable,
    };

    pub fn get_sim_send_time(self: *commands) f64 {
        // const jittered_delay = self.random.float(f32) * 0.06 + 0.15; // 60ms + 150ms
        const jittered_delay = self.random.float(f32) * 0.05 + 0.1; // 60ms + 150ms
        return self.time + jittered_delay;
    }

    pub fn destroy(self: *commands) void {
        for (self.remote_messages_send_queue[0..self.remote_messages_send_queue_len]) |mes| {
            self.allocator.free(mes.payload);
        }
    }
};

// Timing + Clock
pub var clock_hz: f64 = 0.0;
pub fn timer_warmup() void {
    var li: c.LARGE_INTEGER = undefined;
    _ = c.QueryPerformanceFrequency(&li);
    clock_hz = @floatFromInt(li.QuadPart);
}
pub fn timer_get() i64 {
    var li: c.LARGE_INTEGER = undefined;
    _ = c.QueryPerformanceCounter(&li);
    return li.QuadPart;
}
pub inline fn timer_calc_delta(a: i64, b: i64) f64 {
    return @as(f64, @floatFromInt(b - a)) / clock_hz;
}

// Engine Frame Housekeeping
pub var quit = false;
var old_time: i64 = 0;
pub fn start_of_frame() void {
    var msg: c.MSG = undefined;
    while (c.PeekMessageW(&msg, null, 0, 0, c.PM_REMOVE) != 0) {
        if (msg.message == c.WM_QUIT) {
            quit = true;
            break;
        }
        _ = c.TranslateMessage(&msg);
        _ = c.DispatchMessageW(&msg);
    }
}
pub fn end_of_frame(res: *resources_t) void {
    const new_time = zeng.timer_get();
    res.get(time_res).delta_time = zeng.timer_calc_delta(old_time, new_time);
    old_time = new_time;
    // std.time.sleep(std.time.ns_per_s / 60);
}

const remote_message = net.remote_message;

// User Input
var key_down = [_]bool{false} ** 256;
pub const key_code = enum(u8) {
    a = 0x41,
    b,
    c,
    d,
    e,
    f,
    g,
    h,
    i,
    j,
    k,
    l,
    m,
    n,
    o,
    p,
    q,
    r,
    s,
    t,
    u,
    v,
    w,
    x,
    y,
    z,

    // Numbers (Top row)
    num_0 = 0x30,
    num_1,
    num_2,
    num_3,
    num_4,
    num_5,
    num_6,
    num_7,
    num_8,
    num_9,

    // Function keys
    F1 = 0x70,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,

    // Control keys
    escape = 0x1B,
    tab = 0x09,
    caps_lock = 0x14,
    shift = 0x10,
    control = 0x11,
    alt = 0x12,
    space = 0x20,
    enter = 0x0D,
    backspace = 0x08,

    // Arrow keys
    left = 0x25,
    up = 0x26,
    right = 0x27,
    down = 0x28,

    // Special keys
    insert = 0x2D,
    delete = 0x2E,
    home = 0x24,
    end = 0x23,
    page_up = 0x21,
    page_down = 0x22,

    // Numpad
    numpad_0 = 0x60,
    numpad_1,
    numpad_2,
    numpad_3,
    numpad_4,
    numpad_5,
    numpad_6,
    numpad_7,
    numpad_8,
    numpad_9,
    multiply = 0x6A,
    add = 0x6B,
    subtract = 0x6D,
    decimal = 0x6E,
    divide = 0x6F,

    // Symbols and misc
    print_screen = 0x2C,
    scroll_lock = 0x91,
    pause = 0x13,
};
pub fn get_key(kc: key_code) bool {
    return key_down[@intFromEnum(kc)];
}
pub var mouse_button_down = [_]bool{false} ** 10;
pub const mouse_button = enum {
    left,
    right,
    middle,
};
pub fn get_mouse_button(b: mouse_button) bool {
    return mouse_button_down[@intFromEnum(b)];
}

pub const cursor_type_enum = enum(usize) {
    arrow = 32512,
    pointer = 32649,
    other = 32644,
};
pub fn set_cursor(cursor_type: cursor_type_enum) void {
    const p: usize = @intFromEnum(cursor_type);
    const op: *const anyopaque = @ptrCast(&p);
    const cop: *const [*c]const c_ushort = @ptrCast(@alignCast(op));
    _ = c.SetCursor(c.LoadCursorW(null, cop.*));
}

pub var global_mouse_pos: [2]i16 = .{ 0, 0 };
pub var key_press_messages: std.ArrayList(u8) = undefined;
pub fn windows_message_handler(hwnd: c.HWND, msg: c.UINT, wParam: c.WPARAM, lParam: c.LPARAM) callconv(.c) c.LRESULT {
    switch (msg) {
        c.WM_DESTROY => {
            c.PostQuitMessage(0);
            return 0;
        },
        c.WM_MOUSEMOVE => {
            const x: i16 = @intCast(lParam & 0xFFFF);
            const y: i16 = @intCast((lParam >> 16) & 0xFFFF);
            global_mouse_pos = .{ x, y };
            return 0;
        },
        c.WM_INPUT => {
            var raw_input: [256]u8 = undefined;
            var size: c.UINT = 256;
            const handle: c.HRAWINPUT = @as(*const c.HRAWINPUT, @ptrCast(&lParam)).*;

            _ = c.GetRawInputData(handle, c.RID_INPUT, &raw_input, &size, @sizeOf(c.RAWINPUTHEADER));
            const raw: *const c.RAWINPUT = @ptrCast(@alignCast(&raw_input));
            if (raw.data.mouse.usFlags == 0) { // Relative mouse movement
                const _dx = raw.data.mouse.lLastX;
                const _dy = raw.data.mouse.lLastY;

                const _input = global_world_ptr.get(global_player_entity, rpc.input_message).?;
                _input.rot_x += @as(f64, @floatFromInt(_dx)) * 0.7;
                _input.rot_y += @as(f64, @floatFromInt(_dy)) * 0.7;
            }

            const flags = raw.data.mouse.unnamed_0.unnamed_0.usButtonFlags;
            if ((flags & c.RI_MOUSE_LEFT_BUTTON_DOWN) != 0) {
                mouse_button_down[@intFromEnum(mouse_button.left)] = true;
                global_mouse_pressed = true;
            }
            if ((flags & c.RI_MOUSE_LEFT_BUTTON_UP) != 0) {
                mouse_button_down[@intFromEnum(mouse_button.left)] = false;
            }
            if ((flags & c.RI_MOUSE_RIGHT_BUTTON_DOWN) != 0) {
                mouse_button_down[@intFromEnum(mouse_button.right)] = true;
            }
            if ((flags & c.RI_MOUSE_RIGHT_BUTTON_UP) != 0) {
                mouse_button_down[@intFromEnum(mouse_button.right)] = false;
            }
            return 0;
        },
        c.WM_KEYDOWN => {
            const vk: c.UINT = @intCast(wParam);
            key_down[vk] = true;
            return 0;
        },
        c.WM_CHAR => {
            const ch: u16 = @intCast(wParam);
            // Convert UTF-16 to UTF-8
            var buffer: [4]u8 = undefined;
            _ = std.unicode.utf16LeToUtf8(&buffer, &[_]u16{ch}) catch return 0;
            // std.debug.print("Character pressed: {c}\n", .{buffer[0]});
            key_press_messages.append(global_allocator, buffer[0]) catch unreachable;

            return 0;
        },
        c.WM_KEYUP => {
            const vk: c.UINT = @intCast(wParam);
            key_down[vk] = false;
            return 0;
        },
        c.WM_SIZE => {
            const width: u64 = @bitCast(lParam & 0xFFFF);
            const height: u64 = @bitCast((lParam >> 16) & 0xFFFF);
            const long_ptr = c.GetWindowLongPtrW(hwnd, c.GWLP_USERDATA);
            if (long_ptr > 0) {
                var res = @as(*zeng.resources_t, @ptrFromInt(@as(usize, @bitCast(long_ptr))));
                res.get(zeng.__graphics_module).width = @intCast(width);
                res.get(zeng.__graphics_module).height = @intCast(height);
                zeng.window_resize_handler(@intCast(width), @intCast(height));
            }
            return 0;
        },
        // c.WM_SETCURSOR => {
        //     // LOWORD(lParam) = hit-test result
        //     // HIWORD(lParam) = mouse-message identifier
        //     if ((lParam & 0xFFFF) == c.HTCLIENT) {
        //         _ = c.SetCursor(c.LoadCursorW(null, @ptrFromInt(32512))); // Set default arrow
        //         return 1; // We handled it
        //     }
        //     return c.DefWindowProcW(hwnd, msg, wParam, lParam);
        // },
        else => return c.DefWindowProcW(hwnd, msg, wParam, lParam),
    }
}
pub const L = std.unicode.utf8ToUtf16LeStringLiteral;
pub fn get_proc_address(user32: c.HMODULE, name: [:0]const u8) ?gl.FunctionPointer {
    // Try wglGetProcAddress first
    const addr = c.wglGetProcAddress(name.ptr);
    if (addr != null) return @ptrCast(addr);

    // Fallback to opengl32.dll exports
    return @ptrCast(c.GetProcAddress(user32, name.ptr));
}
pub fn lock_cursor_to_window(hwnd: c.HWND) void {
    var rect: c.RECT = undefined;
    _ = c.GetClientRect(hwnd, &rect);
    _ = c.ClientToScreen(hwnd, @ptrCast(&rect.left));
    _ = c.ClientToScreen(hwnd, @ptrCast(&rect.right));
    _ = c.ClipCursor(&rect);
}
pub fn hide_cursor() void {
    while (c.ShowCursor(0) >= 0) {} // Keep hiding until display count < 0
}
pub fn show_cursor() void {
    while (c.ShowCursor(1) < 0) {} // Keep showing until display count >= 0
}
pub fn unlock_cursor() void {
    _ = c.ClipCursor(null);
}

// Communication Data Structures
pub fn events(T: type) type {
    return struct {
        const is_events = void{};

        array: std.ArrayList(T),
        addresses: ?std.ArrayList(net.peer_info_t) = null,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, networked: bool) @This() {
            var ret = @This(){ .array = std.ArrayList(T).initCapacity(allocator, 0) catch unreachable, .allocator = allocator };
            if (networked) ret.addresses = std.ArrayList(net.peer_info_t).initCapacity(allocator, 0) catch unreachable;
            return ret;
        }
        pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
            self.array.deinit(allocator);
        }
        pub fn send(this: *@This(), event: T) void {
            this.array.append(this.allocator, event) catch unreachable;
            (this.addresses orelse return).append(this.allocator, std.mem.zeroes(net.peer_info_t)) catch unreachable;
        }
        pub fn send_with_address(this: *@This(), allocator: std.mem.Allocator, event: T, address: net.peer_info_t) void {
            this.array.append(allocator, event) catch unreachable;
            this.addresses.?.append(allocator, address) catch unreachable;
        }
        pub fn items(this: *@This()) []T {
            return this.array.items;
        }
        pub fn clear(this: *@This(), allocator: std.mem.Allocator) void {
            this.array.clearAndFree(allocator);
            if (this.addresses != null) this.addresses.?.clearAndFree(allocator);
        }
    };
}
pub fn ring_buffer(T: type) type {
    return struct {
        arr: [800]T,

        pub fn set(self: *@This(), i: isize, x: T) void {
            const _i = if (i >= 0) i else 0;
            self.arr[@as(usize, @intCast(_i)) % self.arr.len] = x;
        }
        pub fn get(self: *@This(), i: isize) T {
            const _i = if (i >= 0) i else 0;
            return self.arr[@as(usize, @intCast(_i)) % self.arr.len];
        }
    };
}

// Parent-Child Hierarchy
pub const children = struct {
    items: []ecs.entity_id,
};
pub const local_matrix = struct {
    transform: zeng.world_matrix = zeng.mat_identity,
};
pub fn sync_transforms_children(id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn sync_transforms_recursive(parent_global: zeng.world_matrix, id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const local = q_local_transform.get(id, local_matrix) orelse return;
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    global.* = zeng.mat_mult(parent_global, local.transform);

    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
