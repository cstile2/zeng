const std = @import("std");

pub fn KdTree(comptime Vec3: type) type {
    const Axis = enum(u2) { x, y, z };

    const Entry = struct {
        point: Vec3,
        original_index: usize,
    };

    const Node = struct {
        point: Vec3,
        original_index: usize,
        axis: Axis,
        left: ?usize,
        right: ?usize,
    };

    return struct {
        const Self = @This();

        pub const NearestPoint = struct {
            point: Vec3,
            original_index: usize,
            distance_squared: f32,
        };

        allocator: std.mem.Allocator,
        nodes: []Node,
        root: ?usize,

        pub fn init(allocator: std.mem.Allocator, points: []const Vec3) !Self {
            const nodes = try allocator.alloc(Node, points.len);
            errdefer allocator.free(nodes);

            const entries = try allocator.alloc(Entry, points.len);
            defer allocator.free(entries);

            for (points, 0..) |point, i| {
                entries[i] = .{ .point = point, .original_index = i };
            }

            var next_node: usize = 0;
            const root = buildRange(nodes, entries, 0, entries.len, 0, &next_node);
            return .{
                .allocator = allocator,
                .nodes = nodes,
                .root = root,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.nodes);
            self.nodes = &.{};
            self.root = null;
        }

        pub fn nearest(self: *const Self, query: Vec3) ?NearestPoint {
            const root = self.root orelse return null;
            var best: ?NearestPoint = null;
            self.search(root, query, &best);
            return best;
        }

        fn buildRange(
            nodes: []Node,
            entries: []Entry,
            start: usize,
            end: usize,
            depth: usize,
            next_node: *usize,
        ) ?usize {
            if (start == end) return null;

            const axis: Axis = @enumFromInt(depth % 3);
            std.sort.heap(Entry, entries[start..end], axis, lessThan);

            const median = start + (end - start) / 2;
            const node_index = next_node.*;
            next_node.* += 1;
            nodes[node_index] = .{
                .point = entries[median].point,
                .original_index = entries[median].original_index,
                .axis = axis,
                .left = buildRange(nodes, entries, start, median, depth + 1, next_node),
                .right = buildRange(nodes, entries, median + 1, end, depth + 1, next_node),
            };
            return node_index;
        }

        fn search(self: *const Self, node_index: usize, query: Vec3, best: *?NearestPoint) void {
            const node = self.nodes[node_index];
            const distance_squared = squaredDistance(query, node.point);

            if (best.* == null or distance_squared < best.*.?.distance_squared) {
                best.* = .{
                    .point = node.point,
                    .original_index = node.original_index,
                    .distance_squared = distance_squared,
                };
            }

            const query_axis = coordinate(query, node.axis);
            const split_axis = coordinate(node.point, node.axis);
            const near_child = if (query_axis < split_axis) node.left else node.right;
            const far_child = if (query_axis < split_axis) node.right else node.left;

            if (near_child) |child| {
                self.search(child, query, best);
            }

            const axis_distance = query_axis - split_axis;
            if (far_child) |child| {
                if (axis_distance * axis_distance <= best.*.?.distance_squared) {
                    self.search(child, query, best);
                }
            }
        }

        fn lessThan(axis: Axis, a: Entry, b: Entry) bool {
            const a_primary = coordinate(a.point, axis);
            const b_primary = coordinate(b.point, axis);
            if (a_primary != b_primary) return a_primary < b_primary;

            if (a.point.x != b.point.x) return a.point.x < b.point.x;
            if (a.point.y != b.point.y) return a.point.y < b.point.y;
            if (a.point.z != b.point.z) return a.point.z < b.point.z;
            return a.original_index < b.original_index;
        }

        fn coordinate(point: Vec3, axis: Axis) f32 {
            return switch (axis) {
                .x => point.x,
                .y => point.y,
                .z => point.z,
            };
        }

        fn squaredDistance(a: Vec3, b: Vec3) f32 {
            const dx = a.x - b.x;
            const dy = a.y - b.y;
            const dz = a.z - b.z;
            return dx * dx + dy * dy + dz * dz;
        }
    };
}

test "k-d tree returns the nearest input point" {
    const Vec3 = struct {
        x: f32 = 0,
        y: f32 = 0,
        z: f32 = 0,
    };
    const Tree = KdTree(Vec3);
    const points = [_]Vec3{
        .{ .x = -4, .y = 2, .z = 1 },
        .{ .x = 3, .y = 1, .z = 0 },
        .{ .x = 0, .y = -2, .z = 2 },
        .{ .x = 1, .y = 1, .z = 1 },
    };
    var tree = try Tree.init(std.testing.allocator, &points);
    defer tree.deinit();

    const result = tree.nearest(.{ .x = 1.2, .y = 0.9, .z = 1.1 }).?;
    try std.testing.expectEqual(@as(usize, 3), result.original_index);
    try std.testing.expectApproxEqAbs(@as(f32, 0.06), result.distance_squared, 0.000001);
}

test "empty k-d tree has no nearest point" {
    const Vec3 = struct {
        x: f32 = 0,
        y: f32 = 0,
        z: f32 = 0,
    };
    const Tree = KdTree(Vec3);
    var tree = try Tree.init(std.testing.allocator, &.{});
    defer tree.deinit();

    try std.testing.expect(tree.nearest(.{}) == null);
}
