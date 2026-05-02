const std = @import("std");
const zeng = @import("zeng.zig");

/// returns an array of types - each type corresponds to a function parameter
pub fn fn_parameter_type_array(comptime t: std.builtin.Type) [t.@"fn".params.len]type {
    var types: [t.@"fn".params.len]type = undefined;
    for (0..types.len) |i| {
        types[i] = t.@"fn".params[i].type.?;
    }
    return types;
}
pub fn type_array_to_tuple_type(comptime types: anytype) type {
    const payload_type = @Tuple(&types);
    return payload_type;
}
pub fn tuple_of_ptrs(comptime types: anytype) type {
    comptime var struct_fields: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        struct_fields[i] = .{
            .type = *_type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    };
    const payload_type = @Struct(.auto, null, &struct_fields, &.{}, true);
    return payload_type;
}
pub fn type_id(comptime T: type) usize {
    return @intFromPtr(@typeName(T));
}

var type_registry: std.StringArrayHashMap(u32) = undefined;
var type_registry_next_id: u32 = 0;
pub fn warmup_registry(allocator: std.mem.Allocator) void {
    type_registry = std.StringArrayHashMap(u32).init(allocator);
}
pub fn runtime_type_id(comptime T: type) !u32 {
    const result = try type_registry.getOrPut(@typeName(T));
    if (result.found_existing) {} else {
        result.value_ptr.* = type_registry_next_id;
        type_registry_next_id += 1;
    }
    return result.value_ptr.*;
}

pub fn convert_float_slice_to_vec_slice(s: []f32) []zeng.vec3 {
    var ret: []zeng.vec3 = undefined;
    ret.ptr = @ptrCast(s.ptr);
    ret.len = s.len / 3;
    return ret;
}

pub fn fract(v: zeng.vec2) zeng.vec2 {
    return .{ .x = @mod(v.x, 1.0), .y = @mod(v.y, 1.0) };
}
pub fn floor(v: zeng.vec2) zeng.vec2 {
    return .{ .x = @floor(v.x), .y = @floor(v.y) };
}

// perlin
pub fn hash2d(position: zeng.vec2) zeng.vec2 {
    const new_position = zeng.vec2{ .x = position.dot(zeng.vec2{ .x = 334.1, .y = 781.7 }), .y = position.dot(zeng.vec2{ .x = 652.5, .y = 153.3 }) };
    return fract(zeng.vec2.mult_scalar(zeng.vec2{ .x = @sin(new_position.x), .y = @sin(new_position.y) }, 241.5453123)).mult_scalar(3.0).add_scalar(-1.0);
}

pub fn perlin(position: zeng.vec2) f32 {
    var i = floor(position);
    var f = fract(position);

    const u = f.mult(f).mult(zeng.vec2.ONE.mult_scalar(3.0).sub(f.mult_scalar(2.0)));

    const n00 = zeng.vec2.dot(hash2d(i.add(.{ .x = 0.0, .y = 0.0 })), f.sub(.{ .x = 0.0, .y = 0.0 }));
    const n10 = zeng.vec2.dot(hash2d(i.add(.{ .x = 1.0, .y = 0.0 })), f.sub(.{ .x = 1.0, .y = 0.0 }));
    const n01 = zeng.vec2.dot(hash2d(i.add(.{ .x = 0.0, .y = 1.0 })), f.sub(.{ .x = 0.0, .y = 1.0 }));
    const n11 = zeng.vec2.dot(hash2d(i.add(.{ .x = 1.0, .y = 1.0 })), f.sub(.{ .x = 1.0, .y = 1.0 }));

    const mix_x0 = zeng.lerp(n00, n10, u.x);
    const mix_x1 = zeng.lerp(n01, n11, u.x);
    const mix_y = zeng.lerp(mix_x0, mix_x1, u.y);

    return mix_y;
}
