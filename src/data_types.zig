const glfw = @import("mach-glfw");
const std = @import("std");

pub const Camera = struct {
    projection_matrix: [16]f32,
};

pub const Vec3 = packed struct {
    x: f32,
    y: f32,
    z: f32,

    fn div(self: Vec3, f: f32) Vec3 {
        return .{ .x = self.x / f, .y = self.y / f, .z = self.z / f };
    }
    fn length(self: Vec3) f32 {
        return @sqrt(self.x * self.x + self.y * self.y + self.z * self.z);
    }
    fn normalized(self: Vec3) Vec3 {
        return self.div(self.length());
    }
};

pub const Quat = packed struct {
    x: f32 = 0.0,
    y: f32 = 0.0,
    z: f32 = 0.0,
    w: f32 = 0.0,
};

pub const Material = struct {
    shader_program_GPU: u32,
    texture_GPU: u32,
};

pub const ComponentFlags = packed struct {
    sine_mover: bool = false,
    ghost: bool = false,
    mesh: bool = false,
    camera: bool = false,

    _padding: u28 = 0,
};

pub const Entity = struct {
    vao_gpu: u32,
    indices_length: i32,
    world_matrix: [16]f32,
    material: Material,
    camera: Camera,

    component_flags: ComponentFlags,
};

pub fn identity() [16]f32 {
    return [16]f32{
        1, 0, 0, 0, //
        0, 1, 0, 0, //
        0, 0, 1, 0, //
        0, 0, 0, 1, //
    };
}

// TODO: make better, add tests
pub fn multiply_matrices(b: [16]f32, a: [16]f32) [16]f32 {
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

pub fn QuatToMatrix(q: Quat) [16]f32 {
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

pub fn CreateEntity(entities: *[]Entity, e: Entity) void {
    entities.len += 1;
    entities.*[entities.len - 1] = e;
}

// TODO: add tests, research better options
pub fn InvertMatrix(m: [16]f32) [16]f32 {
    var inv: [16]f32 = undefined;

    inv[0] = m[5] * m[10] * m[15] -
        m[5] * m[11] * m[14] -
        m[9] * m[6] * m[15] +
        m[9] * m[7] * m[14] +
        m[13] * m[6] * m[11] -
        m[13] * m[7] * m[10];

    inv[4] = -m[4] * m[10] * m[15] +
        m[4] * m[11] * m[14] +
        m[8] * m[6] * m[15] -
        m[8] * m[7] * m[14] -
        m[12] * m[6] * m[11] +
        m[12] * m[7] * m[10];

    inv[8] = m[4] * m[9] * m[15] -
        m[4] * m[11] * m[13] -
        m[8] * m[5] * m[15] +
        m[8] * m[7] * m[13] +
        m[12] * m[5] * m[11] -
        m[12] * m[7] * m[9];

    inv[12] = -m[4] * m[9] * m[14] +
        m[4] * m[10] * m[13] +
        m[8] * m[5] * m[14] -
        m[8] * m[6] * m[13] -
        m[12] * m[5] * m[10] +
        m[12] * m[6] * m[9];

    inv[1] = -m[1] * m[10] * m[15] +
        m[1] * m[11] * m[14] +
        m[9] * m[2] * m[15] -
        m[9] * m[3] * m[14] -
        m[13] * m[2] * m[11] +
        m[13] * m[3] * m[10];

    inv[5] = m[0] * m[10] * m[15] -
        m[0] * m[11] * m[14] -
        m[8] * m[2] * m[15] +
        m[8] * m[3] * m[14] +
        m[12] * m[2] * m[11] -
        m[12] * m[3] * m[10];

    inv[9] = -m[0] * m[9] * m[15] +
        m[0] * m[11] * m[13] +
        m[8] * m[1] * m[15] -
        m[8] * m[3] * m[13] -
        m[12] * m[1] * m[11] +
        m[12] * m[3] * m[9];

    inv[13] = m[0] * m[9] * m[14] -
        m[0] * m[10] * m[13] -
        m[8] * m[1] * m[14] +
        m[8] * m[2] * m[13] +
        m[12] * m[1] * m[10] -
        m[12] * m[2] * m[9];

    inv[2] = m[1] * m[6] * m[15] -
        m[1] * m[7] * m[14] -
        m[5] * m[2] * m[15] +
        m[5] * m[3] * m[14] +
        m[13] * m[2] * m[7] -
        m[13] * m[3] * m[6];

    inv[6] = -m[0] * m[6] * m[15] +
        m[0] * m[7] * m[14] +
        m[4] * m[2] * m[15] -
        m[4] * m[3] * m[14] -
        m[12] * m[2] * m[7] +
        m[12] * m[3] * m[6];

    inv[10] = m[0] * m[5] * m[15] -
        m[0] * m[7] * m[13] -
        m[4] * m[1] * m[15] +
        m[4] * m[3] * m[13] +
        m[12] * m[1] * m[7] -
        m[12] * m[3] * m[5];

    inv[14] = -m[0] * m[5] * m[14] +
        m[0] * m[6] * m[13] +
        m[4] * m[1] * m[14] -
        m[4] * m[2] * m[13] -
        m[12] * m[1] * m[6] +
        m[12] * m[2] * m[5];

    inv[3] = -m[1] * m[6] * m[11] +
        m[1] * m[7] * m[10] +
        m[5] * m[2] * m[11] -
        m[5] * m[3] * m[10] -
        m[9] * m[2] * m[7] +
        m[9] * m[3] * m[6];

    inv[7] = m[0] * m[6] * m[11] -
        m[0] * m[7] * m[10] -
        m[4] * m[2] * m[11] +
        m[4] * m[3] * m[10] +
        m[8] * m[2] * m[7] -
        m[8] * m[3] * m[6];

    inv[11] = -m[0] * m[5] * m[11] +
        m[0] * m[7] * m[9] +
        m[4] * m[1] * m[11] -
        m[4] * m[3] * m[9] -
        m[8] * m[1] * m[7] +
        m[8] * m[3] * m[5];

    inv[15] = m[0] * m[5] * m[10] -
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

// TODO: add tests
pub fn axis_angle_to_matrix(axis: Vec3, angle: f32) [16]f32 {
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

pub fn AxisAngle(axis: Vec3, angle: f32) Quat {
    return .{
        .x = axis.x * @sin(angle / 2.0), //
        .y = axis.y * @sin(angle / 2.0), //
        .z = axis.z * @sin(angle / 2.0), //
        .w = @cos(angle / 2.0),
    };
}

// TODO: add tests
pub fn perspective_projection_matrix(fov: f32, aspect_ratio: f32, near: f32, far: f32) [16]f32 {
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

pub fn Deserialize(payload: anytype) void {
    switch (@typeInfo(@TypeOf(payload))) {
        .Int => {
            std.debug.print("{any} = {any}\n", .{ @TypeOf(payload), payload });
        },
        .Float => {
            std.debug.print("{any} = {any}\n", .{ @TypeOf(payload), payload });
        },
        .ComptimeInt => {
            std.debug.print("{any} = {any}\n", .{ @TypeOf(payload), payload });
        },
        .ComptimeFloat => {
            std.debug.print("{any} = {any}\n", .{ @TypeOf(payload), payload });
        },
        .Struct => {
            inline for (std.meta.fields(@TypeOf(payload))) |f| {
                switch (@typeInfo(f.type)) {
                    .Int => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .Float => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .ComptimeInt => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .ComptimeFloat => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .Bool => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .Array => {
                        std.debug.print("{s}: {} = {any}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                    },
                    .Pointer => |info| {
                        if (info.size == .Slice) {
                            std.debug.print("{s}: {} = {*}, length: {}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)), @as(f.type, @field(payload, f.name)).len });
                        } else {
                            std.debug.print("{s}: {} = {*}\n", .{ f.name, f.type, @as(f.type, @field(payload, f.name)) });
                        }
                    },
                    else => {
                        Deserialize(@field(payload, f.name));
                    },
                }
            }
        },
        else => {},
    }
}
