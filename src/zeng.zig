const std = @import("std");
const zeng = @This();
const utils = @import("utils.zig");

// Crufty Backends
pub const glfw = @import("mach-glfw");
pub const gl = @import("gl");
pub const c = @cImport({
    @cInclude("windows.h");
    @cInclude("stb_image.h");
});

// Engine Namespacing
pub usingnamespace @import("loader.zig");
const loader = @import("loader.zig");
pub usingnamespace @import("render.zig");
pub const networking = @import("networking.zig");

const ECS = @import("main.zig").ECS;
const ResourceTypes = @import("main.zig").ResourceTypes;

// Engine Data Structures
pub const Vec3 = packed struct {
    x: f32 = 0,
    y: f32 = 0,
    z: f32 = 0,

    pub fn mult(self: Vec3, f: f32) Vec3 {
        return .{ .x = self.x * f, .y = self.y * f, .z = self.z * f };
    }
    pub fn div(self: Vec3, f: f32) Vec3 {
        return .{ .x = self.x / f, .y = self.y / f, .z = self.z / f };
    }
    pub fn add(self: Vec3, v: Vec3) Vec3 {
        return .{ .x = self.x + v.x, .y = self.y + v.y, .z = self.z + v.z };
    }
    pub fn sub(self: Vec3, v: Vec3) Vec3 {
        return .{ .x = self.x - v.x, .y = self.y - v.y, .z = self.z - v.z };
    }
    pub fn length(self: Vec3) f32 {
        return @sqrt(self.x * self.x + self.y * self.y + self.z * self.z);
    }
    pub fn length_sq(self: Vec3) f32 {
        return self.x * self.x + self.y * self.y + self.z * self.z;
    }
    pub fn normalized(self: Vec3) Vec3 {
        return self.div(self.length());
    }
    pub fn lerp(a: Vec3, b: Vec3, t: f32) Vec3 {
        return a.mult(1.0 - t).add(b.mult(t));
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

// Precoded Components
pub const Mesh = struct {
    vao_gpu: u32,
    indices_length: i32,
    material: Material,
};
pub const Camera = struct {
    projection_matrix: [16]f32,
};
pub const Transform = [16]f32;

pub const SkinnedMesh = struct {
    vao_gpu: u32,
    indices_length: i32,
    material: Material,
    num_bones: usize,
};

// Linear Algebra
pub fn identity_matrix() [16]f32 {
    return [16]f32{
        1, 0, 0, 0, //
        0, 1, 0, 0, //
        0, 0, 1, 0, //
        0, 0, 0, 1, //
    };
}
pub fn translated(a: [16]f32, v: Vec3) [16]f32 {
    var b = a;
    b[12] += v.x;
    b[13] += v.y;
    b[14] += v.z;
    return b;
}
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
pub fn quaternion_to_matrix(q: Quat) [16]f32 {
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
pub fn invert_matrix(m: [16]f32) [16]f32 {
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
pub fn axis_angle(axis: Vec3, angle: f32) Quat {
    return .{
        .x = axis.x * @sin(angle / 2.0), //
        .y = axis.y * @sin(angle / 2.0), //
        .z = axis.z * @sin(angle / 2.0), //
        .w = @cos(angle / 2.0),
    };
}
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
pub fn get_matrix_position(t: [16]f32) Vec3 {
    return Vec3{ .x = t[12], .y = t[13], .z = t[14] };
}
pub fn set_matrix_position(t: *[16]f32, v: Vec3) void {
    t[12] = v.x;
    t[13] = v.y;
    t[14] = v.z;
}
pub fn get_matrix_z_axis(t: [16]f32) Vec3 {
    return Vec3{ .x = t[8], .y = t[9], .z = t[10] };
}
pub fn get_column(t: [16]f32, col: anytype) Vec3 {
    return Vec3{ .x = t[col * 4], .y = t[col * 4 + 1], .z = t[col * 4 + 2] };
}

// GLFW + GL
fn glfw_get_proc_address(p: zeng.glfw.GLProc, proc: [:0]const u8) ?zeng.gl.FunctionPointer {
    _ = p;
    return zeng.glfw.getProcAddress(proc);
}
fn error_callback(error_code: zeng.glfw.ErrorCode, description: [:0]const u8) void {
    std.log.err("glfw error: {}: {s}\n", .{ error_code, description });
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
pub fn window_resize(window: zeng.glfw.Window, width: u32, height: u32) void {
    zeng.gl.viewport(0, 0, @intCast(width), @intCast(height));
    if (window.getUserPointer(zeng.EngineState)) |gd| {
        gd.window_width = width;
        gd.window_height = height;
        gd.res.get(@import("main.zig").MainCamera).camera.projection_matrix = zeng.perspective_projection_matrix(1.3, @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height)), 0.01, 100.0);
    }
}
pub const EngineState = struct {
    active_window: zeng.glfw.Window,
    window_width: u32,
    window_height: u32,

    gpa: std.heap.GeneralPurposeAllocator(.{}),
    allocator: std.mem.Allocator,

    arena: std.heap.ArenaAllocator,
    arena_allocator: std.mem.Allocator,

    res: *Resources,
};
pub fn engine_start(gd: *zeng.EngineState, res: *Resources, world: *ECS.World) !void {
    {
        // set glfw error callback
        zeng.glfw.setErrorCallback(error_callback);
        if (!zeng.glfw.init(.{})) {
            std.log.err("failed to initialize GLFW: {?s}", .{zeng.glfw.getErrorString()});
            std.process.exit(1);
        }

        // window dimensions
        gd.window_width = 800;
        gd.window_height = 450;

        // create our window
        gd.active_window = zeng.glfw.Window.create(gd.window_width, gd.window_height, "colsens game window!", null, null, .{
            .opengl_profile = .opengl_core_profile,
            .context_version_major = 4,
            .context_version_minor = 0,
        }) orelse {
            std.log.err("failed to create GLFW window: {?s}", .{zeng.glfw.getErrorString()});
            std.process.exit(1);
        };
        // necessary for window resizing
        gd.active_window.setFramebufferSizeCallback(window_resize);
        gd.active_window.setUserPointer(gd);
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

        zeng.c.stbi_set_flip_vertically_on_load(1);

        warmup_timer_counter();
        old_time = get_high_resolution_time();

        gd.gpa = std.heap.GeneralPurposeAllocator(.{}){};
        gd.allocator = gd.gpa.allocator();

        gd.arena = std.heap.ArenaAllocator.init(gd.allocator);
        gd.arena_allocator = gd.arena.allocator();

        res.* = zeng.Resources.init(gd.allocator);
        gd.res = res;

        world.* = ECS.World.init(gd.allocator);
    }
}
pub fn engine_end(gd: *zeng.EngineState, res: *Resources, world: *ECS.World) void {
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

pub fn Iterator(comptime types: anytype) type {
    comptime var tuple_fields: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        tuple_fields[i] = .{
            .type = *_type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    };
    const payload_type = @Type(.{ .Struct = .{
        .layout = .Auto,
        .fields = &tuple_fields,
        .decls = &.{},
        .is_tuple = true,
    } });

    return struct {
        q: *const ECS.Query(types),
        q_table_values: []*const ECS.ArchetypeTable,
        index: usize,
        current_table: usize,
        pub const TYPES: @TypeOf(types) = types;
        pub fn create(_q: *ECS.Query(types)) !@This() {
            // TODO: gather all relevant component columns to iterate faster
            return .{ .q = _q, .index = 0, .current_table = 0, .q_table_values = _q._relevant_tables.values() };
        }
        pub fn next(self: *@This()) ?payload_type {
            if (self.q_table_values.len == 0) return null;
            if (self.index >= self.q_table_values[self.current_table].count) {
                if (self.current_table + 1 < self.q_table_values.len) {
                    self.current_table += 1;
                    self.index = 0;
                } else return null;
            }

            var current_columns = self.q.components.items[self.current_table];

            var thing: payload_type = undefined;
            inline for (&thing, comptime 0..) |*_thing, i| {
                _thing.* = current_columns[i].entry_ptr(self.index, @TypeOf(_thing.*.*)) catch unreachable;
            }

            self.index += 1;
            return thing;
        }
        pub fn reset(self: *@This()) void {
            self.index = 0;
            self.current_table = 0;
        }
    };
}
pub const EngineAPIHelper = struct {
    world: *ECS.World,
    gd: *zeng.EngineState,
    res: *Resources,
    allocator: std.mem.Allocator,

    pub fn run_system(self: *EngineAPIHelper, comptime func: anytype) void {
        const t = @typeInfo(@TypeOf(func));

        const typ = comptime utils.tuple_of_types(utils.fn_parameter_types(t));
        var params: typ = undefined;

        inline for (&params) |*param| {
            if (@TypeOf(param.*) == *zeng.EngineState) { // global struct
                param.* = self.gd;
            } else if (comptime blk: {
                for (ResourceTypes) |type_| {
                    if (@TypeOf(param.*) == *type_) {
                        break :blk true;
                    }
                }
                break :blk false;
            }) { // resource
                param.* = self.res.get(@TypeOf(param.*.*));
            } else { // query
                const these_types = comptime @TypeOf(param.*.*).TYPES;

                const q_ptr, const undef = self.res.get_create(self.allocator, ECS.Query(these_types));
                if (undef) {
                    q_ptr.* = try ECS.Query(these_types).create(self.world, self.allocator);
                } else {
                    try q_ptr.destroy();
                    q_ptr.* = try ECS.Query(these_types).create(self.world, self.allocator);
                }

                param.* = q_ptr;
            }
        }
        @call(.auto, func, params) catch unreachable;
    }
};

pub const Resources = struct {
    ptrs: std.AutoArrayHashMap(usize, *anyopaque),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{ .ptrs = std.AutoArrayHashMap(usize, *anyopaque).init(allocator) };
    }
    pub fn deinit(self: *Resources) void {
        self.ptrs.deinit();
    }
    pub fn insert(resources: *Resources, allocator: std.mem.Allocator, p: anytype) void {
        const new_guy = allocator.create(@TypeOf(p)) catch unreachable;
        new_guy.* = p;

        resources.ptrs.put(utils.type_id(@TypeOf(p)), @ptrCast(new_guy)) catch unreachable;
    }
    pub fn get(resources: *Resources, p: type) *p {
        return @alignCast(@ptrCast(resources.ptrs.getEntry(utils.type_id(p)).?.value_ptr.*));
    }
    pub fn get_create(resources: *Resources, allocator: std.mem.Allocator, p: type) struct { *p, bool } {
        var gotten: *p = undefined;
        var undef = false;
        if (resources.ptrs.contains(utils.type_id(p))) {
            gotten = @alignCast(@ptrCast(resources.ptrs.get(utils.type_id(p)).?));
        } else {
            undef = true;
            const new_guy = allocator.create(p) catch unreachable;
            resources.ptrs.put(utils.type_id(p), @ptrCast(new_guy)) catch unreachable;
            gotten = new_guy;
        }
        return .{ gotten, undef };
    }
};

// Commands
const PlayerEvent = @import("main.zig").PlayerEvent;

pub fn remote_event_implementation(event: PlayerEvent) void {
    std.debug.print("I recieved an event: {any}\n", .{event});
}
pub const procs = .{
    remote_event_implementation,
    @import("rpc.zig").TestNetMessage,
};
pub const args_to_serialize = [_]type{ // TODO: refactor
    struct { PlayerEvent },
    struct { f32 },
};
pub const args_to_retrieve = [_]type{ // TODO: refactor
    struct {},
    struct { *ECS.World },
};

pub fn GET_PROC_CODE(comptime func: anytype) u32 {
    var count: u32 = 0;
    for (procs) |proc| {
        if (@as(*const anyopaque, @ptrCast(&proc)) == @as(*const anyopaque, @ptrCast(&func))) {
            return count;
        }
        count += 1;
    }
    @compileError("invalid procedure");
}
pub const Commands = struct {
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
    storage_bytes: [1024]Command = undefined,
    storage_bytes_curr: u32 = 0,

    remote_messages: [128]RemoteMessage,
    remote_messages_len: u8,

    // entities
    pub fn spawn(self: *Commands, payload: anytype) void {
        self.add_command_type(.spawn);
        inline for (payload) |elem| {
            self.add_insertion_command(elem);
        }
        self.add_command_type(.empty);
    }
    fn add_insertion_command(self: *Commands, payload: anytype) void {
        self.storage_bytes[self.storage_bytes_curr] = Command{ .size = @sizeOf(@TypeOf(payload)), .id = comptime ECS.GET_COMPONENT_ID(@TypeOf(payload)), .kind = .insert, .stuff = undefined };
        @memcpy(@as([*]u8, @ptrCast(&self.storage_bytes[self.storage_bytes_curr].stuff)), @as([*]const u8, @ptrCast(&payload))[0..@sizeOf(@TypeOf(payload))]);
        self.storage_bytes_curr += 1;
    }
    fn add_command_type(self: *Commands, t: command_type) void {
        self.storage_bytes[self.storage_bytes_curr].kind = t;
        self.storage_bytes_curr += 1;
    }
    pub fn process_commands(self: *Commands, world: *ECS.World) void {
        var curr: u32 = 0;
        var current_ent: ECS.entity_id = undefined;

        while (curr < self.storage_bytes_curr) {
            defer curr += 1;

            if (self.storage_bytes[curr].kind == .spawn) {
                current_ent = world.spawn(.{}) catch unreachable;
                continue;
            }
            if (self.storage_bytes[curr].kind == .insert) {
                world.insert_runtime(ECS.__runtime_type_information[self.storage_bytes[curr].id], &self.storage_bytes[curr].stuff, world._public_ids[current_ent]) catch unreachable;
                continue;
            }
            if (self.storage_bytes[curr].kind == .empty) {
                current_ent = undefined;
                // break;
            }
        }

        self.storage_bytes_curr = 0;
    }

    pub fn insert() void {}
    pub fn remove() void {}

    // networking
    /// queues a remote procedure call to be sent to destination whenever this application sends next
    pub fn remote_call(self: *Commands, conn_id: SocketAndAddress, comptime procedure: anytype, args: anytype) void {
        const args2 = @as(args_to_serialize[GET_PROC_CODE(procedure)], args);
        // const arg_tup: std.meta.ArgsTuple(procedure) = undefined;
        // const caps = @import("main.zig").extract(arg_tup, args2.len, arg_tup.len);
        // _ = caps; // autofix
        // std.builtin.Type

        const procedure_code: u32 = comptime GET_PROC_CODE(procedure);

        var payload_array = self.allocator.alloc(u8, @sizeOf(u32) + @sizeOf(@TypeOf(args2))) catch unreachable;
        var payload_curr: u32 = 0;
        loader.serialize_to_bytes(procedure_code, payload_array, &payload_curr);
        loader.serialize_to_bytes(args2, payload_array, &payload_curr);
        payload_array = self.allocator.realloc(payload_array, payload_curr) catch unreachable;

        self.remote_messages[self.remote_messages_len] = RemoteMessage{ .payload = payload_array[0..payload_curr], .target = conn_id };
        self.remote_messages_len += 1;
    }
    /// queues a remote procedure call that will trigger an event on some other machine
    pub fn remote_event(self: *Commands, conn_id: SocketAndAddress, event: anytype) void {
        self.remote_call(conn_id, remote_event_implementation, .{event});
    }
};

// Timing + Clock
pub var clock_hz: f64 = 0.0;
pub fn warmup_timer_counter() void {
    var li: c.LARGE_INTEGER = undefined;
    _ = c.QueryPerformanceFrequency(&li);
    clock_hz = @floatFromInt(li.QuadPart);
}
pub fn get_high_resolution_time() i64 {
    var li: c.LARGE_INTEGER = undefined;
    _ = c.QueryPerformanceCounter(&li);
    return li.QuadPart;
}
pub inline fn calculate_time_delta(a: i64, b: i64) f64 {
    return @as(f64, @floatFromInt(b - a)) / clock_hz;
}

// Engine Frame Housekeeping
var old_time: i64 = 0;
pub fn start_of_frame() void {
    glfw.pollEvents();
}
pub fn end_of_frame(res: *Resources) void {
    const new_time = zeng.get_high_resolution_time();
    res.get(@import("main.zig").Time).delta_time = zeng.calculate_time_delta(old_time, new_time);
    old_time = new_time;
}

const ConnectionID = networking.ConnectionID;
const RemoteMessage = networking.RemoteMessage;
const SocketAndAddress = networking.SocketAndAddress;

// Events
pub fn Events(T: type) type {
    return struct {
        circular_buffer: [128]T = undefined,
        start: u32 = 0,
        end: u32 = 0,

        pub fn insert(self: *Events(T), e: T) void {
            self.circular_buffer[self.end] = e;
            self.end = (self.end + 1) % @as(u32, self.circular_buffer.len);
        }
        pub fn pop(self: *Events(T)) void {
            self.start = (self.start + 1) % @as(u32, self.circular_buffer.len);
        }

        pub fn get_pieces(self: *Events(T)) struct { []T, []T } {
            if (self.start < self.end) {
                return .{ self.circular_buffer[self.start..self.end], self.circular_buffer[0..0] };
            } else {
                std.debug.print("yeet\n", .{});
                return .{ self.circular_buffer[self.start..self.circular_buffer.len], self.circular_buffer[0..self.end] };
            }
        }

        pub fn make_writer(self: *Events(T)) EventWriter(T) {
            return EventWriter(T){ .events = self };
        }
        pub fn make_reader(self: *Events(T)) EventReader(T) {
            return EventReader(T){ .events = self };
        }
    };
}
pub fn EventWriter(T: type) type {
    return struct {
        events: *Events(T),

        pub fn send(self: *EventWriter(T), e: T) void {
            self.events.insert(e);
        }
    };
}
pub fn EventReader(T: type) type {
    return struct {
        events: *Events(T),

        pub fn read(self: *EventReader(T)) []T {
            self.events.GetSlice();
        }
    };
}

// test "Events" {
//     var C = zeng.Events(u16){};

//     C.insert(0);
//     var curr: u16 = 1;
//     while (curr < 10) {
//         defer curr += 1;
//         C.insert(curr);
//         const tup = C.get_pieces();
//         std.debug.print("=============\n", .{});
//         for (tup[0]) |int| {
//             std.debug.print("{}\n", .{int});
//         }
//         for (tup[1]) |int| {
//             std.debug.print("{}\n", .{int});
//         }
//         std.debug.print("{any} | {any}", .{ tup[0], tup[1] });
//         std.debug.print("=============\n", .{});
//         C.pop();
//     }
// }

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
pub fn run_command(gd: *zeng.EngineState, input_read: []const u8) void {
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
