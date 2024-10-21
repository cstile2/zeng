const std = @import("std");

pub fn fn_parameter_types(comptime t: std.builtin.Type) [t.Fn.params.len]type {
    var types: [t.Fn.params.len]type = undefined;
    for (0..types.len) |i| {
        types[i] = t.Fn.params[i].type.?;
    }
    return types;
}

pub fn tuple_of_types(comptime types: anytype) type {
    comptime var struct_fields: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        struct_fields[i] = .{
            .type = _type,
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
