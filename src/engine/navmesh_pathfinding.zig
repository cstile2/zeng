const std = @import("std");

const zeng = @import("zeng.zig");
const vec3 = zeng.vec3;

const NavTriangle = struct {
    vertices: [3]vec3,
    centroid: vec3,
};

const Link = struct {
    node: usize,
};

fn find_node_path(
    comptime Node: type,
    allocator: std.mem.Allocator,
    nodes: []const Node,
    comptime position_of: *const fn (Node) vec3,
    offsets: []const usize,
    links: []const Link,
    start_node: usize,
    goal_node: usize,
) !?[]usize {
    const count = nodes.len;
    const no_parent = std.math.maxInt(usize);

    const g_scores = try allocator.alloc(f32, count);
    defer allocator.free(g_scores);
    @memset(g_scores, std.math.inf(f32));

    const f_scores = try allocator.alloc(f32, count);
    defer allocator.free(f_scores);
    @memset(f_scores, std.math.inf(f32));

    const parents = try allocator.alloc(usize, count);
    defer allocator.free(parents);
    @memset(parents, no_parent);

    const closed = try allocator.alloc(bool, count);
    defer allocator.free(closed);
    @memset(closed, false);

    const heap = try allocator.alloc(usize, count);
    defer allocator.free(heap);
    const heap_positions = try allocator.alloc(usize, count);
    defer allocator.free(heap_positions);
    @memset(heap_positions, no_parent);
    var heap_len: usize = 0;

    g_scores[start_node] = 0;
    f_scores[start_node] = distance(position_of(nodes[start_node]), position_of(nodes[goal_node]));
    heap_insert_or_decrease(heap, heap_positions, &heap_len, f_scores, start_node);

    while (heap_len > 0) {
        const current = heap_pop(heap, heap_positions, &heap_len, f_scores);
        if (closed[current]) continue;
        if (current == goal_node) break;
        closed[current] = true;

        for (links[offsets[current]..offsets[current + 1]]) |link| {
            if (closed[link.node]) continue;
            const tentative_g = g_scores[current] + distance(position_of(nodes[current]), position_of(nodes[link.node]));
            if (tentative_g >= g_scores[link.node]) continue;

            parents[link.node] = current;
            g_scores[link.node] = tentative_g;
            f_scores[link.node] = tentative_g + distance(position_of(nodes[link.node]), position_of(nodes[goal_node]));
            heap_insert_or_decrease(heap, heap_positions, &heap_len, f_scores, link.node);
        }
    }

    if (goal_node != start_node and parents[goal_node] == no_parent) return null;

    const reversed_path = try allocator.alloc(usize, count);
    defer allocator.free(reversed_path);
    var path_len: usize = 0;
    var node = goal_node;
    while (true) {
        reversed_path[path_len] = node;
        path_len += 1;
        if (node == start_node) break;
        node = parents[node];
        if (node == no_parent) return null;
    }

    const path = try allocator.alloc(usize, path_len);
    errdefer allocator.free(path);
    for (0..path_len) |i| {
        path[i] = reversed_path[path_len - i - 1];
    }
    return path;
}

fn triangle_position(triangle: NavTriangle) vec3 {
    return triangle.centroid;
}

fn identity_position(position: vec3) vec3 {
    return position;
}

pub const Pathfinder = struct {
    const EdgeEntry = struct {
        key: u64,
        triangle: usize,
    };

    const VertexEntry = struct {
        position: vec3,
        original_index: usize,
    };

    allocator: std.mem.Allocator,
    triangles: []NavTriangle,
    offsets: []usize,
    links: []Link,

    fn less_than_vertex(_: void, a: VertexEntry, b: VertexEntry) bool {
        if (a.position.x != b.position.x) return a.position.x < b.position.x;
        if (a.position.y != b.position.y) return a.position.y < b.position.y;
        if (a.position.z != b.position.z) return a.position.z < b.position.z;
        return a.original_index < b.original_index;
    }

    fn less_than_edge(_: void, a: EdgeEntry, b: EdgeEntry) bool {
        if (a.key != b.key) return a.key < b.key;
        return a.triangle < b.triangle;
    }

    pub fn init(
        allocator: std.mem.Allocator,
        positions: []const vec3,
        indices: []const u32,
    ) !@This() {
        if (indices.len % 3 != 0) return error.InvalidNavMesh;

        const triangle_count = indices.len / 3;
        const triangles = try allocator.alloc(NavTriangle, triangle_count);
        errdefer allocator.free(triangles);

        const canonical_indices = try allocator.alloc(u32, positions.len);
        defer allocator.free(canonical_indices);
        const vertices = try allocator.alloc(VertexEntry, positions.len);
        defer allocator.free(vertices);
        for (positions, 0..) |position, i| {
            if (!std.math.isFinite(position.x) or !std.math.isFinite(position.y) or !std.math.isFinite(position.z)) {
                return error.InvalidNavMesh;
            }
            vertices[i] = .{ .position = position, .original_index = i };
        }
        std.sort.heap(VertexEntry, vertices, {}, less_than_vertex);
        var canonical_index: u32 = 0;
        for (vertices, 0..) |vertex, i| {
            if (i > 0 and !same_position(vertices[i - 1].position, vertex.position)) {
                canonical_index = std.math.add(u32, canonical_index, 1) catch return error.InvalidNavMesh;
            }
            canonical_indices[vertex.original_index] = canonical_index;
        }

        const edge_entries = try allocator.alloc(EdgeEntry, indices.len);
        defer allocator.free(edge_entries);

        for (0..triangle_count) |triangle_index| {
            const offset = triangle_index * 3;
            const tri_indices = [3]u32{
                indices[offset],
                indices[offset + 1],
                indices[offset + 2],
            };
            for (tri_indices) |index| {
                if (index >= positions.len) return error.InvalidNavMesh;
            }

            const a = positions[tri_indices[0]];
            const b = positions[tri_indices[1]];
            const c = positions[tri_indices[2]];
            if (b.sub(a).cross(c.sub(a)).length_sq() == 0) return error.InvalidNavMesh;

            triangles[triangle_index] = .{
                .vertices = .{ a, b, c },
                .centroid = a.add(b).add(c).mult_scalar(1.0 / 3.0),
            };

            edge_entries[offset] = .{
                .key = edge_key(canonical_indices[tri_indices[0]], canonical_indices[tri_indices[1]]),
                .triangle = triangle_index,
            };
            edge_entries[offset + 1] = .{
                .key = edge_key(canonical_indices[tri_indices[1]], canonical_indices[tri_indices[2]]),
                .triangle = triangle_index,
            };
            edge_entries[offset + 2] = .{
                .key = edge_key(canonical_indices[tri_indices[2]], canonical_indices[tri_indices[0]]),
                .triangle = triangle_index,
            };
        }

        std.sort.heap(EdgeEntry, edge_entries, {}, less_than_edge);

        const degrees = try allocator.alloc(usize, triangle_count);
        defer allocator.free(degrees);
        @memset(degrees, 0);

        var edge_start: usize = 0;
        while (edge_start < edge_entries.len) {
            var edge_end = edge_start + 1;
            while (edge_end < edge_entries.len and edge_entries[edge_end].key == edge_entries[edge_start].key) : (edge_end += 1) {}
            if (edge_end - edge_start > 2) return error.NonManifoldNavMesh;
            if (edge_end - edge_start == 2) {
                const first = edge_entries[edge_start];
                const second = edge_entries[edge_start + 1];
                if (first.triangle == second.triangle) return error.InvalidNavMesh;
                degrees[first.triangle] += 1;
                degrees[second.triangle] += 1;
            }
            edge_start = edge_end;
        }

        const offsets = try allocator.alloc(usize, triangle_count + 1);
        errdefer allocator.free(offsets);
        offsets[0] = 0;
        for (degrees, 0..) |degree, i| {
            offsets[i + 1] = offsets[i] + degree;
        }

        const links = try allocator.alloc(Link, offsets[triangle_count]);
        errdefer allocator.free(links);
        const cursors = try allocator.dupe(usize, offsets[0..triangle_count]);
        defer allocator.free(cursors);

        edge_start = 0;
        while (edge_start < edge_entries.len) {
            var edge_end = edge_start + 1;
            while (edge_end < edge_entries.len and edge_entries[edge_end].key == edge_entries[edge_start].key) : (edge_end += 1) {}
            if (edge_end - edge_start == 2) {
                const first = edge_entries[edge_start];
                const second = edge_entries[edge_start + 1];
                links[cursors[first.triangle]] = .{
                    .node = second.triangle,
                };
                cursors[first.triangle] += 1;
                links[cursors[second.triangle]] = .{
                    .node = first.triangle,
                };
                cursors[second.triangle] += 1;
            }
            edge_start = edge_end;
        }

        return .{
            .allocator = allocator,
            .triangles = triangles,
            .offsets = offsets,
            .links = links,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.triangles);
        self.allocator.free(self.offsets);
        self.allocator.free(self.links);
        self.triangles = &.{};
        self.offsets = &.{};
        self.links = &.{};
    }

    /// Returns projected endpoints with the centroids of the triangles along the route.
    /// The returned slice is owned by the caller and must be freed with `allocator`.
    pub fn find_path(
        self: *const @This(),
        allocator: std.mem.Allocator,
        start: vec3,
        goal: vec3,
    ) !?[]vec3 {
        if (self.triangles.len == 0) return null;

        const start_projection = self.closest_point(start);
        const goal_projection = self.closest_point(goal);
        const node_path = (try find_node_path(
            NavTriangle,
            allocator,
            self.triangles,
            triangle_position,
            self.offsets,
            self.links,
            start_projection.triangle,
            goal_projection.triangle,
        )) orelse return null;
        defer allocator.free(node_path);

        const path = try allocator.alloc(vec3, node_path.len + 2);
        errdefer allocator.free(path);
        path[0] = start_projection.point;
        for (node_path, 0..) |node, i| {
            path[i + 1] = self.triangles[node].centroid;
        }
        path[node_path.len + 1] = goal_projection.point;
        return path;
    }

    const Projection = struct {
        triangle: usize,
        point: vec3,
        distance_squared: f32,
    };

    fn closest_point(self: *const @This(), point: vec3) Projection {
        var best = Projection{
            .triangle = 0,
            .point = self.triangles[0].vertices[0],
            .distance_squared = std.math.inf(f32),
        };
        for (self.triangles, 0..) |triangle, i| {
            const candidate = closest_point_on_triangle(point, triangle.vertices);
            const candidate_distance = distance_squared(point, candidate);
            if (candidate_distance < best.distance_squared) {
                best = .{
                    .triangle = i,
                    .point = candidate,
                    .distance_squared = candidate_distance,
                };
            }
        }
        return best;
    }
};

pub const GraphPathfinder = struct {
    allocator: std.mem.Allocator,
    positions: []vec3,
    offsets: []usize,
    links: []Link,

    /// Builds a pathfinder from node positions and per-node neighbor indices.
    /// Neighbor lists are used as supplied, so add both directions for undirected edges.
    pub fn init(
        allocator: std.mem.Allocator,
        positions: []const vec3,
        adjacency: []const []const usize,
    ) !@This() {
        if (positions.len != adjacency.len) return error.InvalidGraph;

        const node_positions = try allocator.dupe(vec3, positions);
        errdefer allocator.free(node_positions);
        for (node_positions) |position| {
            if (!std.math.isFinite(position.x) or !std.math.isFinite(position.y) or !std.math.isFinite(position.z)) {
                return error.InvalidGraph;
            }
        }

        const offset_count = std.math.add(usize, positions.len, 1) catch return error.InvalidGraph;
        const offsets = try allocator.alloc(usize, offset_count);
        errdefer allocator.free(offsets);
        offsets[0] = 0;
        for (adjacency, 0..) |neighbors, node| {
            for (neighbors) |neighbor| {
                if (neighbor >= positions.len) return error.InvalidGraph;
            }
            offsets[node + 1] = std.math.add(usize, offsets[node], neighbors.len) catch return error.InvalidGraph;
        }

        const links = try allocator.alloc(Link, offsets[positions.len]);
        errdefer allocator.free(links);
        for (adjacency, 0..) |neighbors, node| {
            for (neighbors, 0..) |neighbor, i| {
                links[offsets[node] + i] = .{ .node = neighbor };
            }
        }

        return .{
            .allocator = allocator,
            .positions = node_positions,
            .offsets = offsets,
            .links = links,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.positions);
        self.allocator.free(self.offsets);
        self.allocator.free(self.links);
        self.positions = &.{};
        self.offsets = &.{};
        self.links = &.{};
    }

    /// Returns node positions from the nearest start node through the nearest goal node.
    /// The returned slice is owned by the caller and must be freed with `allocator`.
    pub fn find_path(
        self: *const @This(),
        allocator: std.mem.Allocator,
        start: vec3,
        goal: vec3,
    ) !?[]vec3 {
        if (self.positions.len == 0) return null;

        const start_node = self.closest_node(start);
        const goal_node = self.closest_node(goal);
        const node_path = (try find_node_path(
            vec3,
            allocator,
            self.positions,
            identity_position,
            self.offsets,
            self.links,
            start_node,
            goal_node,
        )) orelse return null;
        defer allocator.free(node_path);

        const path = try allocator.alloc(vec3, node_path.len);
        errdefer allocator.free(path);
        for (node_path, 0..) |node, i| {
            path[i] = self.positions[node];
        }
        return path;
    }

    fn closest_node(self: *const @This(), point: vec3) usize {
        var closest: usize = 0;
        var best_distance = distance_squared(point, self.positions[0]);
        for (self.positions[1..], 1..) |position, i| {
            const candidate_distance = distance_squared(point, position);
            if (candidate_distance < best_distance) {
                closest = i;
                best_distance = candidate_distance;
            }
        }
        return closest;
    }
};

fn same_position(a: vec3, b: vec3) bool {
    return a.x == b.x and a.y == b.y and a.z == b.z;
}

fn edge_key(a: u32, b: u32) u64 {
    const low = @min(a, b);
    const high = @max(a, b);
    return (@as(u64, low) << 32) | high;
}

fn distance_squared(a: vec3, b: vec3) f32 {
    return a.sub(b).length_sq();
}

fn distance(a: vec3, b: vec3) f32 {
    return a.sub(b).length();
}

fn closest_point_on_triangle(point: vec3, vertices: [3]vec3) vec3 {
    const a, const b, const c = vertices;
    const ab = b.sub(a);
    const ac = c.sub(a);
    const ap = point.sub(a);
    const d1 = ab.dot(ap);
    const d2 = ac.dot(ap);
    if (d1 <= 0 and d2 <= 0) return a;

    const bp = point.sub(b);
    const d3 = ab.dot(bp);
    const d4 = ac.dot(bp);
    if (d3 >= 0 and d4 <= d3) return b;

    const vc = d1 * d4 - d3 * d2;
    if (vc <= 0 and d1 >= 0 and d3 <= 0) {
        return a.add(ab.mult_scalar(d1 / (d1 - d3)));
    }

    const cp = point.sub(c);
    const d5 = ab.dot(cp);
    const d6 = ac.dot(cp);
    if (d6 >= 0 and d5 <= d6) return c;

    const vb = d5 * d2 - d1 * d6;
    if (vb <= 0 and d2 >= 0 and d6 <= 0) {
        return a.add(ac.mult_scalar(d2 / (d2 - d6)));
    }

    const va = d3 * d6 - d5 * d4;
    if (va <= 0 and d4 - d3 >= 0 and d5 - d6 >= 0) {
        return b.add(c.sub(b).mult_scalar((d4 - d3) / ((d4 - d3) + (d5 - d6))));
    }

    const denominator = 1.0 / (va + vb + vc);
    return a.add(ab.mult_scalar(vb * denominator).add(ac.mult_scalar(vc * denominator)));
}

fn heap_insert_or_decrease(
    heap: []usize,
    positions: []usize,
    heap_len: *usize,
    scores: []const f32,
    node: usize,
) void {
    var index = positions[node];
    if (index == std.math.maxInt(usize)) {
        index = heap_len.*;
        heap_len.* += 1;
        heap[index] = node;
        positions[node] = index;
    }

    while (index > 0) {
        const parent = (index - 1) / 2;
        if (!heap_less(heap[index], heap[parent], scores)) break;
        heap_swap(heap, positions, index, parent);
        index = parent;
    }
}

fn heap_pop(heap: []usize, positions: []usize, heap_len: *usize, scores: []const f32) usize {
    const result = heap[0];
    heap_len.* -= 1;
    positions[result] = std.math.maxInt(usize);
    if (heap_len.* == 0) return result;

    heap[0] = heap[heap_len.*];
    positions[heap[0]] = 0;
    var index: usize = 0;
    while (true) {
        const left = index * 2 + 1;
        if (left >= heap_len.*) break;
        const right = left + 1;
        const child = if (right < heap_len.* and heap_less(heap[right], heap[left], scores)) right else left;
        if (!heap_less(heap[child], heap[index], scores)) break;
        heap_swap(heap, positions, index, child);
        index = child;
    }
    return result;
}

fn heap_less(a: usize, b: usize, scores: []const f32) bool {
    if (scores[a] != scores[b]) return scores[a] < scores[b];
    return a < b;
}

fn heap_swap(heap: []usize, positions: []usize, a: usize, b: usize) void {
    std.mem.swap(usize, &heap[a], &heap[b]);
    positions[heap[a]] = a;
    positions[heap[b]] = b;
}

test "A star finds a route through shared navmesh edge" {
    const positions = [_]vec3{
        .{ .x = 0, .z = 0 },
        .{ .x = 2, .z = 0 },
        .{ .x = 2, .z = 2 },
        .{ .x = 0, .z = 0 },
        .{ .x = 2, .z = 2 },
        .{ .x = 0, .z = 2 },
    };
    const indices = [_]u32{ 0, 1, 2, 3, 4, 5 };
    var navmesh = try Pathfinder.init(std.testing.allocator, &positions, &indices);
    defer navmesh.deinit();

    const path = (try navmesh.find_path(std.testing.allocator, .{ .x = 0.2, .z = 0.2 }, .{ .x = 0.2, .z = 1.8 })).?;
    defer std.testing.allocator.free(path);

    try std.testing.expectEqual(@as(usize, 4), path.len);
    try std.testing.expectApproxEqAbs(@as(f32, 4.0 / 3.0), path[1].x, 0.000001);
    try std.testing.expectApproxEqAbs(@as(f32, 2.0 / 3.0), path[1].z, 0.000001);
    try std.testing.expectApproxEqAbs(@as(f32, 2.0 / 3.0), path[2].x, 0.000001);
    try std.testing.expectApproxEqAbs(@as(f32, 4.0 / 3.0), path[2].z, 0.000001);
}

test "A star reports disconnected navmesh as unreachable" {
    const positions = [_]vec3{
        .{ .x = 0, .z = 0 },
        .{ .x = 1, .z = 0 },
        .{ .x = 0, .z = 1 },
        .{ .x = 5, .z = 0 },
        .{ .x = 6, .z = 0 },
        .{ .x = 5, .z = 1 },
    };
    const indices = [_]u32{ 0, 1, 2, 3, 4, 5 };
    var navmesh = try Pathfinder.init(std.testing.allocator, &positions, &indices);
    defer navmesh.deinit();

    const path = try navmesh.find_path(std.testing.allocator, .{ .x = 0.1, .z = 0.1 }, .{ .x = 5.1, .z = 0.1 });
    try std.testing.expect(path == null);
}

test "graph pathfinder converts adjacency lists and finds a route" {
    const positions = [_]vec3{
        .{ .x = 0, .z = 0 },
        .{ .x = 1, .z = 0 },
        .{ .x = 2, .z = 0 },
        .{ .x = 0, .z = 2 },
    };
    const adjacency = [_][]const usize{
        &.{ 1, 3 },
        &.{ 0, 2 },
        &.{ 1, 3 },
        &.{ 0, 2 },
    };
    var graph = try GraphPathfinder.init(std.testing.allocator, &positions, &adjacency);
    defer graph.deinit();

    const path = (try graph.find_path(
        std.testing.allocator,
        .{ .x = 0.1, .z = 0.1 },
        .{ .x = 1.9, .z = 0.1 },
    )).?;
    defer std.testing.allocator.free(path);

    try std.testing.expectEqual(@as(usize, 3), path.len);
    try std.testing.expectEqual(positions[0], path[0]);
    try std.testing.expectEqual(positions[1], path[1]);
    try std.testing.expectEqual(positions[2], path[2]);
}

test "graph pathfinder rejects invalid neighbor indices" {
    const positions = [_]vec3{.{ .x = 0 }};
    const adjacency = [_][]const usize{&.{1}};

    try std.testing.expectError(
        error.InvalidGraph,
        GraphPathfinder.init(std.testing.allocator, &positions, &adjacency),
    );
}
