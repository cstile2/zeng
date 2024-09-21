const std = @import("std");

pub fn TUP(comptime types: anytype) type {
    comptime var new_fields2: [types.len]std.builtin.Type.StructField = undefined;
    comptime for (types, 0..) |_type, i| {
        new_fields2[i] = .{
            .type = _type,
            .name = std.fmt.comptimePrint("{d}", .{i}),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(_type),
        };
    };
    const payload_type2 = @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &new_fields2,
            .decls = &.{},
            .is_tuple = true,
        },
    });
    return payload_type2;
}

pub fn TYPETUP(comptime t: std.builtin.Type) [t.Fn.params.len]type {
    var types: [t.Fn.params.len]type = undefined;
    for (0..types.len) |i| {
        types[i] = t.Fn.params[i].type.?;
    }
    return types;
}

pub fn type_id(comptime T: type) usize {
    var a: u8 = 0;
    comptime var c: u8 = 0;

    inline for (@typeName(T)) |char| {
        a = a ^ char ^ c;
        c += 1;
    }

    return @intCast(a);
}
