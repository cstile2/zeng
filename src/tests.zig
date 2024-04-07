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

    const A = try world.SpawnEntity(.{Velocity{ .speed = 100 }});
    try std.testing.expectEqual(Velocity{ .speed = 100 }, try world.GetComponent(A, Velocity));

    const B = try world.SpawnEntity(.{ Sine_mover{ .hor = 10, .vert = 13 }, Mesh{ .tris = 0, .tick = 110, .tex = 1 } });
    try std.testing.expectEqual(Sine_mover{ .hor = 10, .vert = 13 }, try world.GetComponent(B, Sine_mover));
    try std.testing.expectEqual(Mesh{ .tris = 0, .tick = 110, .tex = 1 }, try world.GetComponent(B, Mesh));

    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    _ = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    const C = try world.SpawnEntity(.{Sine_mover{ .hor = 5, .vert = 6 }});
    try std.testing.expectEqual(Sine_mover{ .hor = 5, .vert = 6 }, try world.GetComponent(C, Sine_mover));

    const D = try world.SpawnEntity(.{ Sine_mover{ .hor = 250, .vert = 251 }, Mesh{ .tris = 252, .tick = 253, .tex = 254 }, Velocity{ .speed = 255 } });
    try std.testing.expectEqual(Velocity{ .speed = 255 }, try world.GetComponent(D, Velocity));
    try std.testing.expectEqual(Mesh{ .tris = 252, .tick = 253, .tex = 254 }, try world.GetComponent(D, Mesh));
    try std.testing.expectEqual(Sine_mover{ .hor = 250, .vert = 251 }, try world.GetComponent(D, Sine_mover));

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
        const V_: []Velocity = curr.field(Velocity);
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
        const V_: []Velocity = curr.field(Velocity);
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
        const S_: []Sine_mover = curr.field(Sine_mover);
        const V_: []Velocity = curr.field(Velocity);
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
        const S_: []Sine_mover = curr.field(Sine_mover);
        const V_: []Velocity = curr.field(Velocity);
        for (S_, V_) |*s, *v| {
            try std.testing.expectEqual(count + 0, s.vert);
            try std.testing.expectEqual(count + 1, s.hor);
            try std.testing.expectEqual(count + 2, v.speed);
            count += 1;
        }
    }
    try world.Destroy();
}
