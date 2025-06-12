const std = @import("std");
const zeng = @import("zeng.zig");

/// returns an array of types - each type corresponds to a function parameter
pub fn fn_parameter_type_array(comptime t: std.builtin.Type) [t.Fn.params.len]type {
    var types: [t.Fn.params.len]type = undefined;
    for (0..types.len) |i| {
        types[i] = t.Fn.params[i].type.?;
    }
    return types;
}

pub fn type_array_to_tuple_type(comptime types: anytype) type {
    var struct_fields: [types.len]std.builtin.Type.StructField = undefined;
    for (types, 0..) |_type, i| {
        struct_fields[i] = .{
            .type = _type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    }
    const payload_type = @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &struct_fields,
            .decls = &.{},
            .is_tuple = true,
        },
    });
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
    const payload_type = @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &struct_fields,
            .decls = &.{},
            .is_tuple = true,
        },
    });
    return payload_type;
}

pub fn type_id(comptime T: type) usize {
    return @intFromPtr(&@typeName(T));
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
