const std = @import("std");
const ECS = @import("ecs.zig");
const Velocity = struct {
    speed: u8 = 25,
};
const Sine_mover = struct {
    hor: u8 = 14,
    vert: u8 = 88,
};
const Mesh = struct {
    tris: u8 = 11,
    tex: u8 = 245,
    tick: u8 = 111,
};

test "Spawning" {
    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);

    var v: *Velocity = undefined;
    var s: Sine_mover = undefined;
    var m: *const Mesh = undefined;

    const A = try world.SpawnEntity(.{Velocity{ .speed = 100 }});
    try world.Get(A, &v);
    try std.testing.expectEqual(Velocity{ .speed = 100 }, v.*);

    const B = try world.SpawnEntity(.{ Sine_mover{ .hor = 10, .vert = 13 }, Mesh{ .tris = 0, .tick = 110, .tex = 1 } });
    try world.Get(B, .{ &s, &m });
    try std.testing.expectEqual(Sine_mover{ .hor = 10, .vert = 13 }, s);
    try std.testing.expectEqual(Mesh{ .tris = 0, .tick = 110, .tex = 1 }, m.*);

    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    const C = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    try world.Get(C, &s);
    try std.testing.expectEqual(Sine_mover{ .hor = 5, .vert = 6 }, s);

    const D = try world.SpawnEntity(.{ Sine_mover{ .hor = 250, .vert = 251 }, Mesh{ .tris = 252, .tick = 253, .tex = 254 }, Velocity{ .speed = 255 } });
    try world.Get(D, .{ &v, &m, &s });
    try std.testing.expectEqual(Velocity{ .speed = 255 }, v.*);
    try std.testing.expectEqual(Mesh{ .tris = 252, .tick = 253, .tex = 254 }, m.*);
    try std.testing.expectEqual(Sine_mover{ .hor = 250, .vert = 251 }, s);

    try world.Destroy();
}

test "SingleIteration" {
    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);

    {
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
        _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
        _ = try world.SpawnEntity(.{ Mesh{}, Velocity{} });
        _ = try world.SpawnEntity(.{ Mesh{}, Velocity{} });
        _ = try world.SpawnEntity(.{ Sine_mover{}, Mesh{}, Velocity{} });
        _ = try world.SpawnEntity(.{ Sine_mover{}, Mesh{}, Velocity{} });

        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});

        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});

        _ = try world.SpawnEntity(.{ Sine_mover{}, Mesh{} });
    }

    var count: u8 = 0;
    var curr = try ECS.QueryIterator.create(&world, .{Velocity});
    defer curr.destroy() catch unreachable;
    while (curr.next()) {
        const V_: []Velocity = try curr.field(Velocity);
        for (V_) |*v| {
            try std.testing.expectEqual(Velocity{}, v.*);
            v.speed = count;
            count += 1;
        }
    }
    try std.testing.expectEqual(count, 16);
    count = 0;
    curr.reset();
    while (curr.next()) {
        const V_: []Velocity = try curr.field(Velocity);
        for (V_) |*v| {
            try std.testing.expectEqual(count, v.speed);
            count += 1;
        }
    }

    try world.Destroy();
}

test "MultipleIteration" {
    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);

    {
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{}, Mesh{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{}, Mesh{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{}, Mesh{} });
        _ = try world.SpawnEntity(.{ Velocity{}, Sine_mover{}, Mesh{} });

        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});
        _ = try world.SpawnEntity(.{Velocity{}});

        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});
        _ = try world.SpawnEntity(.{Sine_mover{}});

        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});
        _ = try world.SpawnEntity(.{Mesh{}});

        _ = try world.SpawnEntity(.{ Velocity{}, Mesh{} });

        _ = try world.SpawnEntity(.{ Sine_mover{}, Mesh{} });
    }

    var count: u8 = 0;
    var curr = try ECS.QueryIterator.create(&world, .{ Sine_mover, Velocity });
    defer curr.destroy() catch unreachable;
    while (curr.next()) {
        const S_: []Sine_mover = try curr.field(Sine_mover);
        const V_: []Velocity = try curr.field(Velocity);
        for (S_, V_) |*s, *v| {
            try std.testing.expectEqual(Sine_mover{}, s.*);
            try std.testing.expectEqual(Velocity{}, v.*);
            s.vert = count + 0;
            s.hor = count + 1;
            v.speed = count + 2;
            count += 1;
        }
    }
    try std.testing.expectEqual(count, 16);
    count = 0;
    curr.reset();
    while (curr.next()) {
        const S_: []Sine_mover = try curr.field(Sine_mover);
        const V_: []Velocity = try curr.field(Velocity);
        for (S_, V_) |*s, *v| {
            try std.testing.expectEqual(count + 0, s.vert);
            try std.testing.expectEqual(count + 1, s.hor);
            try std.testing.expectEqual(count + 2, v.speed);
            count += 1;
        }
    }
    try world.Destroy();
}

test "EntityLookups" {
    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);
    defer world.Destroy() catch unreachable;

    const E = try world.SpawnEntity(.{ Velocity{}, Mesh{}, Sine_mover{} });

    var iter = try ECS.QueryIterator.create(&world, .{Velocity});
    defer iter.destroy() catch unreachable;

    var x: *Velocity = undefined;
    try iter.get(E, .{&x});
    try std.testing.expectEqual(Velocity{}, x.*);

    var iter2 = try ECS.QueryIterator.create(&world, .{ Velocity, Mesh, Sine_mover });
    defer iter2.destroy() catch unreachable;

    var a: Velocity = undefined;
    var b: *Mesh = undefined;
    var c: *const Sine_mover = undefined;
    try iter2.get(E, .{ &a, &b, &c });
    try std.testing.expectEqual(Velocity{}, a);
    try std.testing.expectEqual(Mesh{}, b.*);
    try std.testing.expectEqual(Sine_mover{}, c.*);

    var y: Mesh = undefined;
    try iter2.get(E, &y);
    try std.testing.expectEqual(Mesh{}, y);

    var z: *Sine_mover = undefined;
    try iter2.get(E, &z);
    try std.testing.expectEqual(Sine_mover{}, z.*);
}

test "Events" {
    const Event = struct {
        position: f32 = 34.0,
    };

    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);
    defer world.Destroy() catch unreachable;

    var events: [100]Event = .{Event{}} ** 100;
    var event_slice: []Event = events[0..0];

    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });
    _ = try world.SpawnEntity(.{ Sine_mover{}, Velocity{} });

    var count: u64 = 0;
    var curr = try ECS.QueryIterator.create(&world, .{ Sine_mover, Velocity });
    defer curr.destroy() catch unreachable;
    while (curr.next()) {
        const S_: []Sine_mover = try curr.field(Sine_mover);
        const V_: []Velocity = try curr.field(Velocity);
        for (S_, V_) |*s, *v| {
            try std.testing.expectEqual(Sine_mover{}, s.*);
            try std.testing.expectEqual(Velocity{}, v.*);
            event_slice.len += 1;
            event_slice[event_slice.len - 1].position = @floatFromInt(count);
            count += 1;
        }
    }
    count = 0;
    for (event_slice) |event| {
        try std.testing.expectEqual(@as(f32, @floatFromInt(count)), event.position);
        count += 1;
    }
}

test "ManyEntitiesOfSameArchetype" {
    const allocator = std.testing.allocator;
    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);
    defer world.Destroy() catch unreachable;

    var curr: u64 = 0;
    var E: ECS.EntityDataLocation = undefined;
    while (curr < 2048) {
        defer curr += 1;
        E = try world.SpawnEntity(.{ Sine_mover{}, Velocity{ .speed = @intCast(@mod(curr, 256)) } });
    }

    var curr2: u64 = 0;
    while (curr2 < curr) {
        defer curr2 += 1;
        var sm: Sine_mover = undefined;
        var v: Velocity = undefined;
        const F = ECS.EntityDataLocation{ .archetype_hash = E.archetype_hash, .row = curr2 };
        try world.Get(F, .{ &sm, &v });
        try std.testing.expectEqual(Sine_mover{}, sm);
        try std.testing.expectEqual(Velocity{ .speed = 99 }, v);
    }
}

fn Mockup() void {
    var iter = ECS.iterator(.{ Sine_mover, Velocity });

    // TESTING ENTIY ITERATION API

    // expose table disjointness - implementation specific
    // seems fastest, i like it but its verbose
    while (iter.next()) {
        const S_: []Sine_mover = iter.field(Sine_mover);
        const V_: []Velocity = iter.field(Velocity);
        for (S_, V_) |*s, *v| {
            // code here
            v.* += s.*;
            s.* += v.*;
        }
    }

    // go per entity - get copy of the data and auto send the mutations back
    // seems to be the slowest - copies must be made twice - compiler has no chance of shedding the copying
    // this is definitely not the way but it is an interesting option
    while (iter.next()) {
        var s: Sine_mover = iter.get_component(Sine_mover);
        defer iter.set_component(s);
        var v: Velocity = iter.get_component(Velocity);
        defer iter.set_component(v);

        // code here
        v += s;
        s += v;
    }

    // go per entity and get components by pointer - could modify to have copies
    // this should be close to or equal to the performance of the top example, looks very clean
    while (iter.next()) {
        const s: *Sine_mover = iter.get_ptr(Sine_mover);
        const v: *Velocity = iter.get_ptr(Velocity);

        // code here
        v.* += s.*;
        s.* += v.*;
    }

    // TESTING SINGLE ENTIY OPERATION API

    var query = ECS.Query(.{ Sine_mover, Velocity });
    const entity_ref = undefined;

    // get single component from single entity using entity reference
    const sine_mover = query.get(entity_ref, Sine_mover);
    _ = sine_mover;
}
