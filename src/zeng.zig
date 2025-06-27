const zeng = @This();
const std = @import("std");
const utils = @import("utils.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const main = @import("main.zig");
pub const net = @import("networking.zig");
pub const glfw = @import("mach-glfw");
pub const gl = @import("gl");
pub const c = @cImport({
    @cInclude("initguid.h");
    @cInclude("windows.h");
    @cInclude("audioclient.h");
    @cInclude("audiopolicy.h");
    @cInclude("mmdeviceapi.h");
    @cInclude("stb_image.h");
});
pub usingnamespace @import("loader.zig");
pub usingnamespace @import("render.zig");

pub const Input = struct {
    pub const key = glfw.Key;
};

// Engine structs
pub const vec2 = struct {
    x: f32 = 0,
    y: f32 = 0,

    pub fn sub(self: vec2, v: vec2) vec2 {
        return .{ .x = self.x - v.x, .y = self.y - v.y };
    }
    pub fn mult(self: vec2, f: f32) vec2 {
        return .{ .x = self.x * f, .y = self.y * f };
    }
    pub fn div(self: vec2, f: f32) vec2 {
        return .{ .x = self.x / f, .y = self.y / f };
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
    animations: std.ArrayList(usize),
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

// GLFW + GL
pub fn glfw_get_proc_address(p: zeng.glfw.GLProc, proc: [:0]const u8) ?zeng.gl.FunctionPointer {
    _ = p;
    return zeng.glfw.getProcAddress(proc);
}
pub fn error_callback(error_code: zeng.glfw.ErrorCode, description: [:0]const u8) void {
    std.log.err("glfw error - code: {}\n{s}\n", .{ error_code, description });
}
pub fn opengl_log_error() !void {
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

        std.log.err("Found OpenGL error: {s}", .{errorString});

        err = zeng.gl.getError();
    }
}

// Application
pub const engine_context = struct {
    active_window: zeng.glfw.Window,

    gpa: std.heap.GeneralPurposeAllocator(.{}),
    allocator: std.mem.Allocator,

    arena: std.heap.ArenaAllocator,
    arena_allocator: std.mem.Allocator,
};
pub fn window_resize_handler(window: zeng.glfw.Window, width: u32, height: u32) void {
    zeng.gl.viewport(0, 0, @intCast(width), @intCast(height));
    if (window.getUserPointer(zeng.resources_t)) |res| {
        res.get(main.main_camera_res).camera.projection_matrix = zeng.mat_perspective_projection(1.2, @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height)), 0.01, 1000.0);
    }
}
pub fn engine_start(gd: *zeng.engine_context, res: *resources_t, world: *ecs.world, dep: *resource_fetcher) !void {
    // set glfw error callback
    zeng.glfw.setErrorCallback(error_callback);
    if (!zeng.glfw.init(.{})) {
        std.log.err("failed to initialize GLFW: {?s}", .{zeng.glfw.getErrorString()});
        std.process.exit(1);
    }

    // create our window
    gd.active_window = zeng.glfw.Window.create(800, 450, "the amazing game!", null, null, .{
        .opengl_profile = .opengl_core_profile,
        .context_version_major = 4,
        .context_version_minor = 0,
    }) orelse {
        std.log.err("failed to create GLFW window: {?s}", .{zeng.glfw.getErrorString()});
        std.process.exit(1);
    };
    // necessary for window resizing
    gd.active_window.setFramebufferSizeCallback(window_resize_handler);
    gd.active_window.setUserPointer(res);
    gd.active_window.setKeyCallback(key_callback);
    gd.active_window.setInputModeRawMouseMotion(true);

    zeng.glfw.makeContextCurrent(gd.active_window);
    const proc: zeng.glfw.GLProc = undefined;
    try zeng.gl.load(proc, glfw_get_proc_address);

    // zeng.glfw.swapInterval(0); // turn vsync off

    zeng.gl.enable(zeng.gl.DEPTH_TEST);
    zeng.gl.enable(zeng.gl.CULL_FACE);
    zeng.gl.enable(zeng.gl.BLEND);
    zeng.gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    zeng.gl.enable(zeng.gl.FRAMEBUFFER_SRGB);

    timer_warmup();
    old_time = timer_get();

    gd.gpa = std.heap.GeneralPurposeAllocator(.{}){};
    gd.allocator = gd.gpa.allocator();

    gd.arena = std.heap.ArenaAllocator.init(gd.allocator);
    gd.arena_allocator = gd.arena.allocator();

    res.* = zeng.resources_t.init(gd.arena_allocator);

    world.* = ecs.world.init(gd.allocator);
    dep.* = .{ .world = world, .res = res, .allocator = gd.arena_allocator };
}
pub fn engine_end(gd: *zeng.engine_context, res: *resources_t, world: *ecs.world) void {
    gd.active_window.destroy();
    zeng.glfw.terminate();
    world.deinit() catch void;
    res.deinit();
    gd.arena.deinit();
    _ = gd.gpa.deinit();
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
            if (comptime blk: {
                for (main.RESOURCE_TYPES) |type_| {
                    if (@TypeOf(param.*) == *type_) {
                        break :blk true;
                    }
                }
                break :blk false;
            }) { // valid registered resource type
                param.* = self.res.get(@TypeOf(param.*.*));
            } else { // assume query by default
                const component_list = comptime @TypeOf(param.*.*).TYPES;
                param.* = self.fresh_query(component_list);
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

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .map = std.AutoArrayHashMap(usize, *anyopaque).init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *resources_t) void {
        self.map.deinit();
    }

    pub fn insert(self: *resources_t, p: anytype) void {
        const new_guy = self.allocator.create(@TypeOf(p)) catch unreachable;
        new_guy.* = p;

        const a = self.map.getOrPut(utils.type_id(@TypeOf(p))) catch unreachable;
        if (a.found_existing) {
            const ref = @as(*@TypeOf(p), @alignCast(@ptrCast(a.value_ptr.*)));
            self.allocator.destroy(ref);
        }
        a.value_ptr.* = @ptrCast(new_guy);
    }
    pub fn insert_ptr(self: *resources_t, p: anytype) void {
        const erased = @as(*anyopaque, @ptrCast(p));
        const type_id = utils.type_id(@typeInfo(@TypeOf(p)).Pointer.child);
        self.map.put(type_id, erased) catch unreachable;
    }
    pub fn get(resources: *resources_t, p: type) *p {
        return @alignCast(@ptrCast(resources.map.getPtr(utils.type_id(p)).?.*));
    }
    pub fn get_create(self: *resources_t, p: type) struct { *p, bool } {
        var gotten: *p = undefined;
        var undef = false;
        if (self.map.contains(utils.type_id(p))) {
            gotten = @alignCast(@ptrCast(self.map.get(utils.type_id(p)).?));
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

    remote_messages: [4000]remote_message,
    remote_messages_len: u16,

    time: f64 = 0.0,

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
                // break;
            }
        }

        self.queued_commands_curr = 0;
    }

    // networking
    /// queues a remote procedure call to be sent to destination at the end of the current frame.
    pub fn remote_call(self: *commands, socket: net.socket_t, address: net.address_t, comptime procedure: anytype, _args: anytype) void {
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
        zeng.serialize_to_bytes(procedure_code, payload_array, &payload_curr);
        zeng.serialize_to_bytes(args, payload_array, &payload_curr);

        self.remote_messages[self.remote_messages_len] = remote_message{ .payload = payload_array[0..payload_curr], .sender_socket = socket, .target_address = address };
        self.remote_messages_len += 1;
    }

    pub fn remote_event(self: *commands, socket: net.socket_t, address: net.Address_t, event: anytype) void {
        if (self.random.float(f32) < 0.7) return;
        const payload_array = self.allocator.alloc(u8, 4 + @sizeOf(@TypeOf(event))) catch unreachable;
        var curr_byte: u32 = 0;
        zeng.serialize_to_bytes(comptime GET_MSG_CODE(@TypeOf(event)), payload_array, &curr_byte);
        zeng.serialize_to_bytes(event, payload_array, &curr_byte);

        const jittered_delay = self.random.float(f32) * 0.2 + 1.0; // 60ms + 150ms

        self.remote_messages[self.remote_messages_len] = remote_message{ .payload = self.allocator.realloc(payload_array, curr_byte) catch unreachable, .sender_socket = socket, .target_address = address, .time_to_send = self.time + jittered_delay };
        self.remote_messages_len += 1;
    }

    pub fn remote_event_(self: *commands, socket: net.socket_t, address: net.Address_t, event: anytype) void {
        const payload_array = self.allocator.alloc(u8, 4 + @sizeOf(@TypeOf(event))) catch unreachable;
        var curr_byte: u32 = 0;
        zeng.serialize_to_bytes(comptime GET_MSG_CODE(@TypeOf(event)), payload_array, &curr_byte);
        zeng.serialize_to_bytes(event, payload_array, &curr_byte);

        self.remote_messages[self.remote_messages_len] = remote_message{ .payload = self.allocator.realloc(payload_array, curr_byte) catch unreachable, .sender_socket = socket, .target_address = address, .time_to_send = self.time };
        self.remote_messages_len += 1;
    }

    pub fn destroy(self: *commands) void {
        for (self.remote_messages[0..self.remote_messages_len]) |mes| {
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
var old_time: i64 = 0;
pub fn start_of_frame() void {
    glfw.pollEvents();
}
pub fn end_of_frame(res: *resources_t) void {
    const new_time = zeng.timer_get();
    res.get(main.time_res).delta_time = zeng.timer_calc_delta(old_time, new_time);
    old_time = new_time;
    // std.time.sleep(std.time.ns_per_s / 60);
}

const remote_message = net.remote_message;

// Misc + Unused Stuff
pub fn custom_struct(comptime in: anytype) type {
    var fields: [in.len]std.builtin.Type.StructField = undefined;
    for (in, 0..) |t, i| {
        const fieldType: type = t;
        const fieldName = @typeName(fieldType); //[:0]const u8 = t[0][0..];
        fields[i] = .{
            .name = fieldName,
            .type = fieldType,
            .default_value = null,
            .is_comptime = false,
            .alignment = 0,
        };
    }
    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = fields[0..],
            .decls = &[_]std.builtin.Type.Declaration{},
            .is_tuple = false,
        },
    });
}
pub fn execute_console_command(gd: *zeng.engine_context, input_read: []const u8) void {
    const separated: [][]u8 = zeng.separate_text(input_read, ';');
    defer {
        for (separated) |string| {
            gd.allocator.free(string);
        }
        gd.allocator.free(separated);
    }
    for (separated) |sub_command| {
        const parsed: [][]u8 = zeng.separate_text(sub_command, ' ');
        defer {
            for (parsed) |string| {
                gd.allocator.free(string);
            }
            gd.allocator.free(parsed);
        }
        if (std.mem.eql(u8, parsed[0], "import")) {
            if (parsed.len >= 2) {
                const ents = zeng.ImportModelAsset(parsed[1], gd.allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);
                defer gd.allocator.free(ents);
            } else {
                std.debug.print("No path specified", .{});
            }
        } else if (std.mem.eql(u8, parsed[0], "freeze") and parsed.len == 1) { // pauses everything in the game except a spectator camera
            std.debug.print("FREEZE!\n", .{});
            gd.frozen = !gd.frozen;
        } else if (std.mem.eql(u8, parsed[0], "add")) { // adds a component to an entity
            if (parsed.len >= 3) {
                if (std.fmt.parseInt(u32, parsed[1], 10)) |parsed_int| {
                    const index: u32 = parsed_int;
                    std.debug.print("modifying index: {any}\n", .{index});
                    zeng.AddComponent(&gd.entity_slice[index], parsed[2]) catch {
                        std.debug.print("Could not add component: '{s}'", .{parsed[2]});
                    };
                } else |_| {
                    std.debug.print("Invalid numerical field in command\n", .{});
                }
            } else {
                std.debug.print("Too few arguments for 'add' command\n", .{});
            }
        } else if (std.mem.eql(u8, parsed[0], "remove")) { // removes a component from an entity
            if (parsed.len >= 3) {
                if (std.fmt.parseInt(u32, parsed[1], 10)) |parsed_int| {
                    const index: u32 = parsed_int;
                    std.debug.print("modifying index: {any}\n", .{index});
                    zeng.remove_component(&gd.entity_slice[index], parsed[2]) catch {
                        std.debug.print("Could not remove component: '{s}'", .{parsed[2]});
                    };
                } else |_| {
                    std.debug.print("Invalid numerical field in command\n", .{});
                }
            } else {
                std.debug.print("Too few arguments for 'remove' command\n", .{});
            }
        } else { // unrecognized command
            std.debug.print("Command not recognized: '{s}'\n", .{sub_command});
        }
    }
}

const box_vertices = [_]f32{
    // positions
    -1.0, 1.0,  -1.0,
    -1.0, -1.0, -1.0,
    1.0,  -1.0, -1.0,
    1.0,  -1.0, -1.0,
    1.0,  1.0,  -1.0,
    -1.0, 1.0,  -1.0,

    -1.0, -1.0, 1.0,
    -1.0, -1.0, -1.0,
    -1.0, 1.0,  -1.0,
    -1.0, 1.0,  -1.0,
    -1.0, 1.0,  1.0,
    -1.0, -1.0, 1.0,

    1.0,  -1.0, -1.0,
    1.0,  -1.0, 1.0,
    1.0,  1.0,  1.0,
    1.0,  1.0,  1.0,
    1.0,  1.0,  -1.0,
    1.0,  -1.0, -1.0,

    -1.0, -1.0, 1.0,
    -1.0, 1.0,  1.0,
    1.0,  1.0,  1.0,
    1.0,  1.0,  1.0,
    1.0,  -1.0, 1.0,
    -1.0, -1.0, 1.0,

    -1.0, 1.0,  -1.0,
    1.0,  1.0,  -1.0,
    1.0,  1.0,  1.0,
    1.0,  1.0,  1.0,
    -1.0, 1.0,  1.0,
    -1.0, 1.0,  -1.0,

    -1.0, -1.0, -1.0,
    -1.0, -1.0, 1.0,
    1.0,  -1.0, -1.0,
    1.0,  -1.0, -1.0,
    -1.0, -1.0, 1.0,
    1.0,  -1.0, 1.0,
};

// Maybe Useful Someday
fn SubTuple(comptime T: type, comptime low: usize, comptime high: usize) type {
    const info = @typeInfo(T);
    const old_fields = std.meta.fields(T)[low..high];
    var new_fields: [old_fields.len]std.builtin.Type.StructField = undefined;
    for (old_fields, 0..) |old, i| {
        new_fields[i] = .{
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .type = old.type,
            .default_value = old.default_value,
            .alignment = old.alignment,
            .is_comptime = old.is_comptime,
        };
    }
    return @Type(.{
        .Struct = .{
            .layout = info.Struct.layout,
            .fields = &new_fields,
            .decls = &.{},
            .is_tuple = true,
        },
    });
}

fn Split(comptime T: type, comptime pivot: usize) type {
    const fields = std.meta.fields(T);
    return std.meta.Tuple(&[_]type{
        SubTuple(T, 0, pivot),
        SubTuple(T, pivot, fields.len),
    });
}

fn split(tuple: anytype, comptime pivot: usize) Split(@TypeOf(tuple), pivot) {
    const fields = std.meta.fields(@TypeOf(tuple));
    return .{
        extract(tuple, 0, pivot),
        extract(tuple, pivot, fields.len),
    };
}

pub fn extract(tuple: anytype, comptime low: usize, comptime high: usize) SubTuple(@TypeOf(tuple), low, high) {
    var out: SubTuple(@TypeOf(tuple), low, high) = undefined;
    inline for (low..high, 0..) |i, o| {
        out[o] = tuple[i];
    }
    return out;
}
