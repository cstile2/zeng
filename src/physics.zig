const std = @import("std");
const zeng = @import("zeng.zig");
const render = @import("render.zig");
const ecs = @import("ecs.zig");

const vec3 = zeng.vec3;
const vec2 = zeng.vec2;

pub const collider_type = enum {
    mesh,
    sphere,
    support_based,
};
pub const collider_info = struct {
    tag: collider_type = .sphere,
    support: *const fn (vec3, collider_info) vec3,
    data: *const anyopaque,
    matrix: zeng.world_matrix,
};

pub fn sphere(dir: vec3, coll: collider_info) vec3 {
    _ = coll; // autofix
    return dir.normalized();
}
pub fn point(dir: vec3, coll: collider_info) vec3 {
    _ = dir; // autofix
    _ = coll; // autofix
    return vec3.ZERO;
}
pub fn dual_point(dir: vec3, coll: collider_info) vec3 {
    _ = coll; // autofix
    if (dir.y >= 0) return vec3{ .y = 0.5 };
    return vec3{ .y = -0.5 };
}
pub fn cube(dir: vec3, coll: collider_info) vec3 {
    _ = coll; // autofix
    return vec3{
        .x = if (dir.x >= 0) 0.5 else -0.5,
        .y = if (dir.y >= 0) 0.5 else -0.5,
        .z = if (dir.z >= 0) 0.5 else -0.5,
    };
}
pub fn triangle(dir: vec3, coll: collider_info) vec3 {
    const tri_data = @as(*const [3]vec3, @alignCast(@ptrCast(coll.data)));

    var max_float = tri_data[0].dot(dir);
    var max_pos: vec3 = tri_data[0];
    for (tri_data.*) |vert| {
        if (vert.dot(dir) > max_float) {
            max_float = vert.dot(dir);
            max_pos = vert;
        }
    }

    return max_pos;
}

fn support(a_coll: collider_info, b_coll: collider_info, dir: vec3) vec3 {
    const a_dir = zeng.mat_mult_vec4(zeng.mat_invert(a_coll.matrix), dir.to_vec4(0.0)).to_vec3();
    const b_dir = zeng.mat_mult_vec4(zeng.mat_invert(b_coll.matrix), dir.to_vec4(0.0)).to_vec3();

    return vec3.sub(
        zeng.mat_mult_vec4(a_coll.matrix, a_coll.support(a_dir, a_coll).to_vec4(1.0)).to_vec3(),
        zeng.mat_mult_vec4(b_coll.matrix, b_coll.support(b_dir.neg(), b_coll).to_vec4(1.0)).to_vec3(),
    );
}
fn support_flat(a_coll: collider_info, b_coll: collider_info, direction: vec2, basis_right: vec3, basis_up: vec3) vec3 {
    const real_direction = basis_right.mult(direction.x).add(basis_up.mult(direction.y));
    return support(a_coll, b_coll, real_direction);
}
fn max_index(arr: anytype) usize {
    var max_idx: usize = 0;
    var max_value: f32 = arr[0];

    for (arr, 0..) |value, index| {
        if (value > max_value) {
            max_value = value;
            max_idx = index;
        }
    }

    return max_idx;
}
fn point_in_triangle(a: vec2, b: vec2, c: vec2, d: *f32) bool {
    const na = b.sub(a).perp_toward(a.sub(c)).normalized();
    const nb = c.sub(b).perp_toward(b.sub(a)).normalized();
    const nc = a.sub(c).perp_toward(c.sub(b)).normalized();

    const da = na.dot(a.neg());
    const db = nb.dot(b.neg());
    const dc = nc.dot(c.neg());

    d.* = @max(@max(da, db), dc);

    return @max(@max(da, db), dc) <= 0;
}

const eps: f32 = 0.000001;
const gjk_state = struct {
    cp: vec3 = undefined,
    tet: [4]vec3 = undefined,
    len: usize = undefined,
    dir: vec3,
    bad: bool = false,
};

const w_lambda = struct {
    [4]f32,
    [4]vec3,
    usize,
};

pub fn shape_cast(a_coll: collider_info, b_coll: collider_info, ray_direction: vec3, enter_t: *f32, exit_t: *f32, _error: *bool) bool {
    var tri2d: [3]vec2 = undefined;
    var tri3d: [3]vec3 = undefined;

    const dir3d: vec3 = ray_direction;
    var init = support(a_coll, b_coll, dir3d);

    var basis_up: vec3 = init.sub(init.project(ray_direction));
    basis_up = basis_up.normalized();
    const basis_right: vec3 = basis_up.cross(ray_direction).normalized();

    var dir = vec2{ .x = 1, .y = 0 };
    tri3d[0] = support_flat(a_coll, b_coll, dir, basis_right, basis_up);
    tri2d[0] = vec2{ .x = tri3d[0].dot(basis_right), .y = tri3d[0].dot(basis_up) };
    if (tri2d[0].dot(dir) < 0) return false;

    dir = tri2d[0].neg();
    tri3d[1] = support_flat(a_coll, b_coll, dir, basis_right, basis_up);
    tri2d[1] = vec2{ .x = tri3d[1].dot(basis_right), .y = tri3d[1].dot(basis_up) };
    if (tri2d[1].dot(dir) < 0) return false;

    dir = tri2d[1].sub(tri2d[0]).perp_toward(tri2d[0].neg());
    tri3d[2] = support_flat(a_coll, b_coll, dir, basis_right, basis_up);
    tri2d[2] = vec2{ .x = tri3d[2].dot(basis_right), .y = tri3d[2].dot(basis_up) };
    if (tri2d[2].dot(dir) < 0) return false;

    var it: u32 = 0;
    while (it < 30) {
        defer it += 1;
        const normals = [3]vec2{ tri2d[1].sub(tri2d[0]).perp_toward(tri2d[0].sub(tri2d[2])).normalized(), tri2d[2].sub(tri2d[1]).perp_toward(tri2d[1].sub(tri2d[0])).normalized(), tri2d[0].sub(tri2d[2]).perp_toward(tri2d[2].sub(tri2d[1])).normalized() };
        const distances = [3]f32{ normals[0].dot(tri2d[0].neg()), normals[1].dot(tri2d[1].neg()), normals[2].dot(tri2d[2].neg()) };

        const most_positive: usize = max_index(distances);

        if (distances[most_positive] <= 0) {
            var error_enter = false;
            var error_exit = false;
            enter_t.* = shape_cast_refine(a_coll, b_coll, tri3d, tri2d, ray_direction, basis_right, basis_up, true, &error_enter);
            exit_t.* = shape_cast_refine(a_coll, b_coll, tri3d, tri2d, ray_direction, basis_right, basis_up, false, &error_exit);
            _error.* = error_enter or error_exit;
            return true;
        }

        dir = normals[most_positive];

        const idx: usize = (most_positive + 2) % 3;
        tri3d[idx] = support_flat(a_coll, b_coll, dir, basis_right, basis_up);
        tri2d[idx] = vec2{ .x = tri3d[idx].dot(basis_right), .y = tri3d[idx].dot(basis_up) };

        if (tri2d[idx].dot(dir) < 0) return false;
    }
    return false;
}
fn shape_cast_refine(a_coll: collider_info, b_coll: collider_info, _tri3d: [3]vec3, _tri2d: [3]vec2, ray_direction: vec3, basis_right: vec3, basis_up: vec3, use_enter: bool, _error: *bool) f32 {
    var tri3d = _tri3d;
    var tri2d = _tri2d;
    _error.* = false;
    var it: usize = 0;
    while (it < 20) {
        defer it += 1;
        var normal = (tri3d[1].sub(tri3d[0])).cross(tri3d[2].sub(tri3d[0]));
        if ((normal.dot(ray_direction) < 0) != use_enter) normal = normal.neg();

        const point3d = support(a_coll, b_coll, normal);

        if ((point3d.sub(tri3d[0]).length_sq() < 0.00001) or (point3d.sub(tri3d[1]).length_sq() < 0.00001) or (point3d.sub(tri3d[2]).length_sq() < 0.00001)) break;

        const point2d = vec2{ .x = point3d.dot(basis_right), .y = point3d.dot(basis_up) };

        var D_a: f32 = undefined;
        var D_b: f32 = undefined;
        var D_c: f32 = undefined;
        if (point_in_triangle(point2d, tri2d[0], tri2d[1], &D_a)) {
            tri3d[2] = point3d;
            tri2d[2] = point2d;
            continue;
        }
        if (point_in_triangle(point2d, tri2d[1], tri2d[2], &D_b)) {
            tri3d[0] = point3d;
            tri2d[0] = point2d;
            continue;
        }
        if (point_in_triangle(point2d, tri2d[2], tri2d[0], &D_c)) {
            tri3d[1] = point3d;
            tri2d[1] = point2d;
            continue;
        }
        _error.* = true;
        break;
    }

    const final_normal = tri3d[1].sub(tri3d[0]).cross(tri3d[2].sub(tri3d[0]));

    const denom: f32 = ray_direction.dot(final_normal);
    if (@abs(denom) == 0) // < 0.000001f)
    {
        // std::cout << "PARALLELISH TRIANGLE ERROR: " << denom << std::endl;
        _error.* = true;
        return 0;
    }

    return (tri3d[0]).dot(final_normal) / denom;
}
pub fn shape_overlap(a_coll: collider_info, b_coll: collider_info) bool {
    var tet: [4]vec3 = undefined;

    var dir = vec3{ .x = 1 };
    tet[0] = support(a_coll, b_coll, dir);
    if (tet[0].dot(dir) < 0) return false;

    dir = tet[0].neg();
    tet[1] = support(a_coll, b_coll, dir);
    if (tet[1].dot(dir) < 0) return false;

    dir = tet[0].neg().reject_out(tet[1].sub(tet[0]));
    tet[2] = support(a_coll, b_coll, dir);
    if (tet[2].dot(dir) < 0) return false;

    dir = tet[1].sub(tet[0]).cross(tet[2].sub(tet[0]));
    if (tet[0].neg().dot(dir) < 0) dir = dir.neg();
    tet[3] = support(a_coll, b_coll, dir);
    if (tet[3].dot(dir) < 0) return false;

    var it: u32 = 0;
    var old_new_point = tet[3];
    while (it < 30) {
        defer it += 1;

        const normals = [4]vec3{
            create_normal(tet[0], tet[1], tet[2], tet[3]),
            create_normal(tet[1], tet[2], tet[3], tet[0]),
            create_normal(tet[2], tet[3], tet[0], tet[1]),
            create_normal(tet[3], tet[0], tet[1], tet[2]),
        };
        const distances = [4]f32{
            create_distance(normals[0], tet[0]),
            create_distance(normals[1], tet[1]),
            create_distance(normals[2], tet[2]),
            create_distance(normals[3], tet[3]),
        };

        const most_positive: usize = max_index(distances);

        if (distances[most_positive] <= 0) {
            return true;
        }

        dir = normals[most_positive];

        const other_point: usize = (most_positive + 3) % 4;
        const new_point = support(a_coll, b_coll, dir);

        if (new_point.sub(old_new_point).length_sq() < 0.0001) break;
        tet[other_point] = new_point;
        old_new_point = new_point;

        if (tet[other_point].dot(dir) < 0) return false;
    }
    return false;
}
pub fn shape_separation(a_coll: collider_info, b_coll: collider_info, info: render.triangle_debug_info, num: usize) vec3 {
    var tet: [4]vec3 = undefined;
    var dir = vec3{ .x = 1 };
    tet[0] = support(a_coll, b_coll, dir);
    var len: usize = 1;

    var cp: vec3 = tet[0];
    var it: u32 = 0;
    while (it < num) {
        defer it += 1;

        var new_state: gjk_state = undefined;
        if (len == 1) {
            new_state = .{ .cp = tet[0], .tet = tet, .len = len, .dir = tet[0].neg() };
        } else if (len == 2) {
            new_state = solve_simplex2(vec3.ZERO, tet[0], tet[1]);
        } else if (len == 3) {
            new_state = solve_simplex3(tet[0], tet[1], tet[2], vec3.ZERO);
        } else if (len == 4) {
            new_state = solve_simplex4(tet);
        } else unreachable;
        if (new_state.cp.length_sq() < eps) break;
        if (new_state.dir.length_sq() < eps) unreachable;
        if (new_state.cp.neg().dot(new_state.dir) <= 0) unreachable;

        cp = new_state.cp;
        dir = new_state.dir;

        const new_point = support(a_coll, b_coll, new_state.dir);
        // if (new_point.dot(dir) - eps <= cp.dot(dir)) break;
        if (redundant_point(tet, len, new_point)) break;
        tet = new_state.tet;
        len = new_state.len;
        push_front(&tet, &len, new_point);
    }
    // std.debug.print("{}\n", .{dir});
    // std.debug.print("{} {} {}\n", .{ len, it, cp.length() });
    // std.debug.print("{} {}\n", .{ len, it });
    for (0..len) |i| {
        for (i + 1..len) |j| {
            render.debug_draw_triangle(.{ tet[i], tet[j], tet[j] }, info);
        }
    }
    if (len == 1) render.debug_draw_triangle(.{ tet[0], tet[0].add(vec3.RIGHT.mult(0.05)), tet[0].add(vec3.UP.mult(0.05)) }, info);
    return cp;
}
pub fn shape_separation2(a_coll: collider_info, b_coll: collider_info, info: render.triangle_debug_info, num: usize) vec3 {
    var tet: [4]vec3 = undefined;
    var len: usize = 0;
    var v = vec3{ .x = 1 };
    var dir = v.neg();
    var it: u32 = 0;
    var W: gjk_state = undefined;
    W.len = 0;
    while (it < num) {
        defer it += 1;

        const new_point = support(a_coll, b_coll, dir);
        if (redundant_point(tet, len, new_point) or v.dot(v) - v.dot(new_point) <= eps) break;
        tet, len = union_w(W.tet, W.len, new_point);
        W = dist(tet, len);
        v = W.cp;
        dir = W.dir;
        if (W.len == 4 or v.length_sq() < eps) break;
    }
    std.debug.print("{} {}\n", .{ len, it });
    for (0..len) |i| {
        for (i + 1..len) |j| {
            render.debug_draw_triangle(.{ tet[i], tet[j], tet[j] }, info);
        }
    }
    if (len == 1) render.debug_draw_triangle(.{ tet[0], tet[0].add(vec3.RIGHT.mult(0.05)), tet[0].add(vec3.UP.mult(0.05)) }, info);
    return v;
}
fn dist(tet: [4]vec3, len: usize) gjk_state {
    var new_state: gjk_state = undefined;
    if (len == 1) {
        new_state = .{ .cp = tet[0], .tet = tet, .len = len, .dir = tet[0].neg() };
    } else if (len == 2) {
        new_state = solve_simplex2(vec3.ZERO, tet[0], tet[1]);
    } else if (len == 3) {
        new_state = solve_simplex3(tet[0], tet[1], tet[2], vec3.ZERO);
    } else if (len == 4) {
        new_state = solve_simplex4(tet);
    } else unreachable;
    return new_state;
}

fn create_normal(a: vec3, b: vec3, c: vec3, d: vec3) vec3 {
    var normal = b.sub(a).cross(c.sub(a));

    if (normal.dot(a.sub(d)) < 0) normal = normal.neg();
    return normal.normalized();
}
fn create_distance(normal: vec3, a: vec3) f32 {
    return normal.dot(a.neg());
}
fn solve_simplex2(p: vec3, a: vec3, b: vec3) gjk_state {
    const ab = b.sub(a);
    if (ab.length_sq() < eps) {
        // unreachable;
        if (a.length_sq() < b.length_sq()) return .{ .cp = a, .tet = .{ a, undefined, undefined, undefined }, .len = 1, .dir = a.neg() } else return .{ .cp = b, .tet = .{ b, undefined, undefined, undefined }, .len = 1, .dir = b.neg() };
    }
    const t = (p.sub(a)).dot(ab) / ab.dot(ab);
    if (t <= 0) return .{ .cp = a, .tet = .{ a, undefined, undefined, undefined }, .len = 1, .dir = a.neg() };
    if (t >= 1) return .{ .cp = b, .tet = .{ b, undefined, undefined, undefined }, .len = 1, .dir = b.neg() };
    return .{ .cp = a.add(ab.mult(t)), .tet = .{ a, b, undefined, undefined }, .len = 2, .dir = a.add(ab.mult(t)).neg() };
}
fn solve_simplex3(a: vec3, b: vec3, c: vec3, o: vec3) gjk_state {
    // const tet = [3]vec3{ a, b, c };
    const _n = b.sub(a).cross(c.sub(a));
    if (_n.length_sq() < eps) {
        // std.debug.print("N: {}\n", .{_n.length_sq()});
        // unreachable;
        const _a = solve_simplex2(vec3.ZERO, a, b);
        const _b = solve_simplex2(vec3.ZERO, b, c);
        const _c = solve_simplex2(vec3.ZERO, c, a);
        var res = _a;
        if (_b.cp.length_sq() < res.cp.length_sq()) res = _b;
        if (_c.cp.length_sq() < res.cp.length_sq()) res = _c;
        return res;
    }
    const n = _n.normalized();
    const a_perp = b.sub(a).cross(n).normalized();
    const a_dist = o.sub(a).dot(a_perp);
    const b_perp = c.sub(b).cross(n).normalized();
    const b_dist = o.sub(b).dot(b_perp);
    const c_perp = a.sub(c).cross(n).normalized();
    const c_dist = o.sub(c).dot(c_perp);
    const arr = [3]f32{ a_dist, b_dist, c_dist };
    const most_positive = max_index(arr);
    if (arr[most_positive] < 0) {
        return .{ .cp = o.sub(a).sub(o.sub(a).project(_n)).add(a), .tet = .{ a, b, c, undefined }, .len = 3, .dir = flip(_n, a, vec3.ZERO) };
    }

    // return solve_simplex2(o, tet[most_positive], tet[(most_positive + 1) % 3]);
    const _a = solve_simplex2(vec3.ZERO, a, b);
    const _b = solve_simplex2(vec3.ZERO, b, c);
    const _c = solve_simplex2(vec3.ZERO, c, a);
    var res = _a;
    if (_b.cp.length_sq() < res.cp.length_sq()) res = _b;
    if (_c.cp.length_sq() < res.cp.length_sq()) res = _c;
    return res;
}
fn solve_simplex4(tet: [4]vec3) gjk_state {
    const faces = [4][3]usize{
        [3]usize{ 0, 1, 2 },
        [3]usize{ 1, 2, 3 },
        [3]usize{ 2, 3, 0 },
        [3]usize{ 3, 0, 1 },
    };

    var closest_state: ?gjk_state = null;
    var min_dist_sq: f32 = std.math.floatMax(f32);

    for (faces) |face| {
        const state = solve_simplex3(
            tet[face[0]],
            tet[face[1]],
            tet[face[2]],
            vec3.ZERO,
        );

        const dist_sq = state.cp.length_sq();
        if (dist_sq < min_dist_sq) {
            min_dist_sq = dist_sq;
            closest_state = state;
        }
    }

    return closest_state orelse unreachable;
}
fn push_front(tet: *[4]vec3, len: *usize, new_point: vec3) void {
    if (len.* >= 4) unreachable;
    var idx = len.*;
    while (idx > 0) {
        defer idx -= 1;
        tet[idx] = tet[idx - 1];
    }
    tet[0] = new_point;
    len.* += 1;
}

fn flip(dir: vec3, p: vec3, o: vec3) vec3 {
    if (o.sub(p).dot(dir) < 0) return dir.neg();
    return dir;
}
fn redundant_point(tet: [4]vec3, len: usize, p: vec3) bool {
    for (0..len) |i| {
        // if (p.sub(tet[i]).length_sq() < eps) return true;
        if (p.x == tet[i].x and p.y == tet[i].y and p.z == tet[i].z) return true;
    }
    return false;
}

fn S3D(tet: [4]vec3, tet_len: usize) w_lambda {
    _ = tet_len;
    var W: [4]vec3 = undefined;
    var lambda: [4]f32 = undefined;
    var len: usize = undefined;
    const M: [16]f32 = .{
        tet[0].x, tet[1].x, tet[2].x, tet[3].x,
        tet[0].y, tet[1].y, tet[2].y, tet[3].y,
        tet[0].z, tet[1].z, tet[2].z, tet[3].z,
        1.0,      1.0,      1.0,      1.0,
    };
    const C_4 = .{
        zeng.mat3_determinant_col_major(zeng.mat4_minor3x3(M, 4, 0)),
        zeng.mat3_determinant_col_major(zeng.mat4_minor3x3(M, 4, 1)),
        zeng.mat3_determinant_col_major(zeng.mat4_minor3x3(M, 4, 2)),
        zeng.mat3_determinant_col_major(zeng.mat4_minor3x3(M, 4, 3)),
    };
    const det_M = C_4[0] + C_4[1] + C_4[2] + C_4[3];
    if (compare_signs(det_M, C_4[0]) and compare_signs(det_M, C_4[1]) and compare_signs(det_M, C_4[2]) and compare_signs(det_M, C_4[3])) {
        lambda = .{ C_4[0] / det_M, C_4[1] / det_M, C_4[2] / det_M, C_4[3] / det_M };
        W = tet;
        len = 4;
    } else {
        var d = std.math.floatMax(f32);
        for (1..4) |j| {
            if (compare_signs(det_M, -C_4[j])) {
                const ex_tet, const ex_len = excluding(tet, len, j);
                const lambda_, const W_, const len_ = S2D(ex_tet, ex_len);
                const d_ = linear_combine(W_, lambda_, len_).length_sq();
                if (d_ < d) {
                    W = W_;
                    lambda = lambda_;
                    len = len_;
                    d = d_;
                }
            }
        }
    }
    return .{ lambda, W, len };
}
fn S2D(tet: [4]vec3, len: usize) w_lambda {
    _ = tet; // autofix
    _ = len; // autofix
    return undefined;
}
fn S1D(tet: [4]vec3, len: usize) w_lambda {
    _ = tet; // autofix
    _ = len; // autofix
    return undefined;
}

fn signed_volumes(tet: [4]vec3, len: usize) w_lambda {
    if (len == 4) {
        return S3D(tet, len);
    } else if (len == 3) {
        return S2D(tet, len);
    } else if (len == 2) {
        return S1D(tet, len);
    } else {
        return .{ .coords = .{ 1, undefined, undefined, undefined }, .tet = tet, .len = 1 };
    }
}

fn gjk(a_coll: collider_info, b_coll: collider_info, v_0: vec3) vec3 {
    var v = v_0;
    var tet: [4]vec3 = .{ undefined, undefined, undefined, undefined };
    var tet_len: usize = 0;
    var W: [4]vec3 = .{ undefined, undefined, undefined, undefined };
    var W_len = 0;
    var it: u32 = 0;
    while (it < 99) {
        defer it += 1;
        const new_support = support(a_coll, b_coll, v.neg());
        if (redundant_point(tet, W_len, new_support) or v.dot(v) - v.dot(new_support) <= eps * eps) break;
        tet, tet_len = union_w(W, W_len, new_support);
        W, const lambda, W_len = signed_volumes(tet, tet_len);
        v = linear_combine(W, lambda, W_len);
        if (W_len == 4 or v.length_sq() < eps) break;
    }
    return v;
}

fn union_w(W: [4]vec3, W_len: usize, new_point: vec3) struct { [4]vec3, usize } {
    var ret: [4]vec3 = undefined;
    var ret_len: usize = undefined;
    var idx = W_len;
    while (idx > 0) {
        defer idx -= 1;
        ret[idx] = W[idx - 1];
    }
    ret[0] = new_point;
    ret_len = W_len + 1;
    return .{ ret, ret_len };
}
fn linear_combine(W: [4]vec3, lambda: [4]f32, len: usize) vec3 {
    var result = vec3.ZERO;
    for (0..len) |i| {
        result = result.add(W[i].mult(lambda[i]));
    }
    return result;
}
fn compare_signs(a: f32, b: f32) bool {
    if (a > 0 and b > 0) return true;
    if (a < 0 and b < 0) return true;
    return false;
}
fn excluding(tet: [4]vec3, len: usize, index: usize) struct { [4]vec3, usize } {
    var out: [4]vec3 = undefined;
    var out_len: usize = 0;
    for (0..len) |i| {
        if (i != index) {
            out[out_len] = tet[i];
            out_len += 1;
        }
    }
    return .{ out, out_len };
}

pub const raycast_result = struct {
    normal: vec3,
    t: f32,
    entity_id: ecs.entity_id,
};
pub fn ray_cast_triangle(ro: vec3, rd: vec3, v0: vec3, v1: vec3, v2: vec3, result: *raycast_result) bool {
    const v1v0 = v1.sub(v0);
    const v2v0 = v2.sub(v0);
    const rov0 = ro.sub(v0);
    const n = vec3.cross(v1v0, v2v0);
    const q = vec3.cross(rov0, rd);
    const d: f32 = 1.0 / vec3.dot(rd, n);
    const u: f32 = d * vec3.dot(q.neg(), v2v0);
    const v: f32 = d * vec3.dot(q, v1v0);
    result.t = d * vec3.dot(n.neg(), rov0);
    result.normal = n;
    if (result.t < 0.0) return false;
    if (u < 0.0 or v < 0.0 or (u + v) > 1.0) return false;
    return true;
}

pub fn ray_cast(ro: vec3, rd: vec3, physics_data: []collider_info, result: *raycast_result) bool {
    result.t = std.math.floatMax(f32);
    var hit = false;
    for (physics_data) |coll| {
        if (coll.tag == .mesh) {
            const positions, const indices = @as(*const struct { []vec3, []u32 }, @alignCast(@ptrCast(coll.data))).*;

            var curr_tri: usize = 0;
            while (curr_tri < indices.len) {
                defer curr_tri += 3;

                const a = zeng.mat_mult_vec4(coll.matrix, positions[indices[curr_tri + 0]].to_vec4(1.0)).to_vec3();
                const b = zeng.mat_mult_vec4(coll.matrix, positions[indices[curr_tri + 1]].to_vec4(1.0)).to_vec3();
                const c = zeng.mat_mult_vec4(coll.matrix, positions[indices[curr_tri + 2]].to_vec4(1.0)).to_vec3();

                var res: raycast_result = undefined;
                const is_hitting = ray_cast_triangle(ro, rd, a, b, c, &res);
                if (is_hitting) {
                    if (res.t < result.t) result.* = res;
                    hit = true;
                }
            }
        }
    }
    return hit;
}
