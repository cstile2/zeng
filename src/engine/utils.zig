const std = @import("std");
const zeng = @import("zeng.zig");

pub const utils_error = error{
    InvalidTriangleCount,
};

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

const edge_entry_t = struct {
    sorted_edge_key: u64,
    tri_index: usize,
};
fn encode_edge(a: usize, b: usize) u64 {
    if (a < b) return (@as(u64, a) << 32) | @as(u64, b);
    return (@as(u64, b) << 32) | @as(u64, a);
}
fn compare_edge(context: void, a: edge_entry_t, b: edge_entry_t) bool {
    _ = context;
    return a.sorted_edge_key < b.sorted_edge_key;
}
pub fn build_triangle_adjacency(comptime index_type_t: type, allocator: std.mem.Allocator, triangles: []const [3]index_type_t) !std.ArrayList(std.ArrayList(usize)) {
    var adjacency = std.ArrayList(std.ArrayList(usize)).initCapacity(allocator, triangles.len) catch unreachable;
    var cleanup = true;
    defer if (cleanup) {
        for (adjacency.items) |*list| list.deinit(allocator);
        adjacency.deinit(allocator);
    };

    var edge_entries = std.ArrayList(edge_entry_t).initCapacity(allocator, triangles.len * 3) catch unreachable;
    defer edge_entries.deinit(allocator);

    for (triangles, 0..) |triangle, tri_index| {
        try adjacency.append(allocator, std.ArrayList(usize).initCapacity(allocator, 0) catch unreachable);

        const a: usize = @intCast(triangle[0]);
        const b: usize = @intCast(triangle[1]);
        const c: usize = @intCast(triangle[2]);

        try edge_entries.append(allocator, edge_entry_t{ .sorted_edge_key = encode_edge(a, b), .tri_index = tri_index });
        try edge_entries.append(allocator, edge_entry_t{ .sorted_edge_key = encode_edge(b, c), .tri_index = tri_index });
        try edge_entries.append(allocator, edge_entry_t{ .sorted_edge_key = encode_edge(c, a), .tri_index = tri_index });
    }

    std.sort.heap(edge_entry_t, edge_entries.items, void{}, compare_edge);

    var i: usize = 0;
    while (i < edge_entries.items.len) {
        const sorted_edge_key = edge_entries.items[i].sorted_edge_key;
        const start = i;
        i += 1;
        while (i < edge_entries.items.len and edge_entries.items[i].sorted_edge_key == sorted_edge_key) : (i += 1) {}

        const end = i;
        if (end - start <= 1) continue;

        for (start..end - 1) |j| {
            const tri_j = edge_entries.items[j].tri_index;
            for (j + 1..end) |k| {
                const tri_k = edge_entries.items[k].tri_index;
                try adjacency.items[tri_j].append(allocator, tri_k);
                try adjacency.items[tri_k].append(allocator, tri_j);
            }
        }
    }

    cleanup = false;
    return adjacency;
}
pub fn compute_weights(allocator: std.mem.Allocator, adjacency: std.ArrayList(std.ArrayList(usize)), centroids: std.ArrayList(zeng.vec3)) std.AutoHashMap(u64, f32) {
    var weights = std.AutoHashMap(u64, f32).init(allocator);

    for (0.., adjacency.items) |a, neighbors| {
        for (neighbors.items) |b| {
            const weight = centroids.items[a].sub(centroids.items[b]).length();
            weights.put(encode_edge(a, b), weight) catch unreachable;
        }
    }

    return weights;
}
pub fn build_triangle_adjacency_from_indices(allocator: std.mem.Allocator, mesh: zeng.cpu_mesh) !std.ArrayList(std.ArrayList(usize)) {
    if (mesh.indices.len % 3 != 0) return utils_error.InvalidTriangleCount;
    const triangles = @as([*]const [3]u32, @ptrCast(mesh.indices.ptr))[0 .. mesh.indices.len / 3];
    return build_triangle_adjacency(u32, allocator, triangles);
}
pub fn build_triangle_centroids(allocator: std.mem.Allocator, mesh: zeng.cpu_mesh) std.ArrayList(zeng.vec3) {
    var centroids = std.ArrayList(zeng.vec3).initCapacity(allocator, 0) catch unreachable;
    var pos_index_index: usize = 0;
    while (pos_index_index < mesh.indices.len) : (pos_index_index += 3) {
        const a = mesh.positions[mesh.indices[pos_index_index + 0]];
        const b = mesh.positions[mesh.indices[pos_index_index + 1]];
        const c = mesh.positions[mesh.indices[pos_index_index + 2]];

        centroids.append(allocator, a.add(b).add(c).div_scalar(3.0)) catch unreachable;
    }

    return centroids;
}
