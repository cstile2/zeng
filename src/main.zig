const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const utils = @import("utils.zig");

// interop between comptime types and runtime component information
pub const ComponentTypes = [_]type{
    zeng.Mesh,
    zeng.Camera,
    zeng.SkinnedMesh,
    zeng.Transform,
    SineMover,
    CircleCollider,
};
pub const ECS = ecs.CompileECS(ComponentTypes);
pub const SineMover = struct {
    offset: f32 = 0.0,
};
pub const CircleCollider = struct {
    radius: f32 = 1.0,
};
pub const PlayerEvent = struct {
    amt: f32,
    num: u16,
};

pub const ResourceTypes = [_]type{
    Time,
};
pub const Time = struct {
    delta_time: f64,
    elapsed_time: f64,
};

pub fn Bundle(comptime types: anytype) type {
    return struct {
        query: ECS.Query(types),
        iterator: zeng.Iterator(types),
    };
}

pub const EngineAPIHelper = struct {
    world: *ECS.World,
    gd: *zeng.EngineState,
    res: *Resources,
    allocator: std.mem.Allocator,

    pub fn run_system(self: *EngineAPIHelper, comptime func: anytype) void {
        const t = @typeInfo(@TypeOf(func));

        const typ = comptime utils.TUP(utils.TYPETUP(t));
        var params: typ = undefined;

        inline for (&params) |*param| {
            if (@TypeOf(param.*) == *zeng.EngineState) { // global struct
                param.* = self.gd;
            } else if (comptime blk: {
                for (ResourceTypes) |type_| {
                    if (@TypeOf(param.*) == *type_) {
                        break :blk true;
                    }
                }
                break :blk false;
            }) { // resource
                param.* = self.res.get(@TypeOf(param.*.*));
            } else { // iterator
                const these_types = comptime @TypeOf(param.*.*).my_types;

                const p, const b = self.res.get_create(self.allocator, ECS.Query(these_types));
                if (b) {
                    p.* = try ECS.Query(these_types).create(self.world, self.allocator);
                }

                const q, const g = self.res.get_create(self.allocator, zeng.Iterator(these_types));
                if (g) {
                    q.* = try zeng.Iterator(these_types).create(p);
                } else {
                    q.reset();
                }

                param.* = q;
            }
        }

        @call(.auto, func, params) catch unreachable;
    }
};

const Resources = struct {
    ptrs: std.AutoArrayHashMap(usize, *anyopaque),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{ .ptrs = std.AutoArrayHashMap(usize, *anyopaque).init(allocator) };
    }
    pub fn deinit(self: *Resources) void {
        self.ptrs.deinit();
    }
    pub fn add(resources: *Resources, allocator: std.mem.Allocator, p: anytype) void {
        const new_guy = allocator.create(@TypeOf(p)) catch unreachable;
        new_guy.* = p;

        std.debug.print("pointer: {*}\n", .{new_guy});

        resources.ptrs.put(utils.type_id(@TypeOf(p)), @ptrCast(new_guy)) catch unreachable;
    }
    pub fn get(resources: *Resources, p: type) *p {
        return @alignCast(@ptrCast(resources.ptrs.getEntry(utils.type_id(p)).?.value_ptr.*));
    }

    pub fn get_create(resources: *Resources, allocator: std.mem.Allocator, p: type) struct { *p, bool } {
        var gotten: *p = undefined;
        var undef = false;
        if (resources.ptrs.contains(utils.type_id(p))) {
            gotten = @alignCast(@ptrCast(resources.ptrs.get(utils.type_id(p)).?));
        } else {
            undef = true;
            const new_guy = allocator.create(p) catch unreachable;
            resources.ptrs.put(utils.type_id(p), @ptrCast(new_guy)) catch unreachable;
            gotten = new_guy;
        }
        return .{ gotten, undef };
    }
};

pub fn main() !void {
    var is_server: bool = true;
    {
        std.debug.print("\nselect network mode:\n", .{});
        const stdin = std.io.getStdIn().reader();
        const thing = stdin.readBoundedBytes(1) catch unreachable;
        if (std.mem.eql(u8, thing.buffer[0..1], "s")) {
            std.debug.print("using server mode...\n", .{});
        } else if (std.mem.eql(u8, thing.buffer[0..1], "c")) {
            std.debug.print("using client mode...\n", .{});
            is_server = false;
        }
    }

    var gd: zeng.EngineState = undefined;
    try zeng.engine_start(&gd);
    defer zeng.engine_end(&gd);
    var world = ECS.World.init(gd.allocator);
    defer world.deinit() catch unreachable;
    var commands: zeng.Commands = .{ .remote_messages = undefined, .remote_messages_len = 0, .allocator = gd.allocator };

    var arena = std.heap.ArenaAllocator.init(gd.allocator); // this arena allocator is a hack - don't know if it is good to do
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var res = Resources.init(gd.allocator);
    defer res.deinit();

    res.add(arena_allocator, Time{ .delta_time = 0.16, .elapsed_time = 0.0 });

    var helper: EngineAPIHelper = .{ .world = &world, .gd = &gd, .res = &res, .allocator = arena_allocator };

    const shader_program_GPU = zeng.load_shader(gd.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const texture_GPU = zeng.load_texture("assets/images/uv_checker.png");
    const skin_shader = zeng.load_shader(gd.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");

    const new_camera_entity = try world.spawn(.{ zeng.Camera{ .projection_matrix = undefined }, zeng.identity_matrix(), CircleCollider{} });
    try world.get_component(new_camera_entity, .{ &gd.active_camera_matrix, &gd.active_camera });
    zeng.window_resize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));
    _ = zeng.instantiate_scene(&world, "assets/blender_files/main_scene.bin", gd.allocator, shader_program_GPU, texture_GPU);

    var skinned_mesh: zeng.SkinnedMesh = undefined;
    {
        var arena_ = std.heap.ArenaAllocator.init(gd.allocator); // this arena allocator is a hack - don't know if it is good to do
        defer arena_.deinit();
        const arena_allocator_ = arena_.allocator();

        const bytes = zeng.get_file_bytes("assets/gltf/yobot_anim.gltf", gd.allocator);
        defer gd.allocator.free(bytes);

        try zeng.parse_gltf(bytes, arena_allocator_);
        skinned_mesh = try zeng.use_parsed_gltf("assets/gltf/yobot_anim.bin", 1, skin_shader, texture_GPU, arena_allocator_);
    }

    var socket_and_address: zeng.networking.SocketAndAddress = undefined;
    if (is_server) {
        socket_and_address = try zeng.networking.make_udp_sock_and_address("127.0.0.1", 12345, true);
        try zeng.networking.bind_socket_and_address(socket_and_address);
    } else {
        socket_and_address = try zeng.networking.make_udp_sock_and_address("192.168.1.104", 12345, true);
    }
    defer std.os.close(socket_and_address.socket);

    _ = try world.spawn(.{
        zeng.identity_matrix(),
        SineMover{ .offset = 0.0 },
        skinned_mesh,
        CircleCollider{ .radius = 1.0 },
    });

    var t_down_last_frame: bool = false;

    while (!gd.active_window.shouldClose()) {
        zeng.glfw.pollEvents();
        if (is_server) {
            try zeng.networking.network_recieve_all(socket_and_address.socket, &world);
        }

        res.get(Time).elapsed_time += res.get(Time).delta_time;

        if ((gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press) and !t_down_last_frame) {
            _ = try world.spawn(.{
                zeng.identity_matrix(),
                SineMover{ .offset = 0.0 },
                skinned_mesh,
                CircleCollider{ .radius = 1.0 },
            });
            if (!is_server) {
                commands.remote_call(socket_and_address, rpc.TestNetMessage, .{14.30});
                commands.remote_event(socket_and_address, PlayerEvent{ .amt = 101.0101, .num = 2323 });
            }
        }
        t_down_last_frame = gd.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;

        helper.run_system(SineMoverSystem);
        helper.run_system(FlySystem);

        var q_4 = try ECS.Query(.{ zeng.Transform, CircleCollider }).create(&world, arena_allocator);
        // defer q_4.destroy() catch unreachable;
        try CircleCollisionSystem(&q_4);

        helper.run_system(MouseLookSystem);
        helper.run_system(RenderSystem);

        // client networking
        if (!is_server) {
            try zeng.networking.network_send_all(&commands);
        }

        zeng.late_frame_calculations(res.get(Time));
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn CircleCollisionSystem(q_4: *ECS.Query(.{ zeng.Transform, CircleCollider })) !void {
    var A = try zeng.Iterator(.{ zeng.Transform, CircleCollider }).create(q_4);
    while (A.next()) |_A| {
        const transformA, _ = _A;

        var B = try zeng.Iterator(.{ zeng.Transform, CircleCollider }).create(q_4);
        while (B.next()) |_B| {
            const transformB, _ = _B;

            if (transformA == transformB) {
                continue;
            }
            var delta = zeng.Vec3{ .x = transformA[12] - transformB[12], .y = transformA[13] - transformB[13], .z = transformA[14] - transformB[14] };
            if (delta.length_sq() > 0.0 and delta.length() < 1.2) {
                delta = delta.normalized().mult(1.2);

                transformB[12] = transformA[12] - delta.x;
                transformB[13] = transformA[13] - delta.y;
                transformB[14] = transformA[14] - delta.z;
            }
        }
    }
}

/// Make all entities with a SineMover component and a transform float around randomly
pub fn SineMoverSystem(time: *Time, sine_iterator: *zeng.Iterator(.{ zeng.Transform, SineMover })) !void {
    while (sine_iterator.next()) |transform_sine| {
        const transform, const sine_mover = transform_sine;

        const localtime = @as(f32, @floatCast(time.elapsed_time)) - sine_mover.offset;
        transform[14] += @sin(localtime * 3.0) * @as(f32, @floatCast(time.delta_time));
        transform[13] += @sin(localtime * 4.0) * @as(f32, @floatCast(time.delta_time));
        transform[12] += @cos(localtime * 5.0) * @as(f32, @floatCast(time.delta_time));
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn FlySystem(gd: *zeng.EngineState, time: *Time, it: *zeng.Iterator(.{ zeng.Transform, zeng.Camera })) !void {
    while (it.next()) |transform_camera| {
        const transform, _ = transform_camera;

        var speed: f32 = @floatCast(time.delta_time * 100.0);
        if (gd.active_window.getKey(zeng.glfw.Key.left_shift) == zeng.glfw.Action.press) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            transform[12] -= transform[0] * speed;
            transform[13] -= transform[1] * speed;
            transform[14] -= transform[2] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            transform[12] += transform[0] * speed;
            transform[13] += transform[1] * speed;
            transform[14] += transform[2] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.q) == zeng.glfw.Action.press) {
            transform[12] -= transform[4] * speed;
            transform[13] -= transform[5] * speed;
            transform[14] -= transform[6] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.e) == zeng.glfw.Action.press) {
            transform[12] += transform[4] * speed;
            transform[13] += transform[5] * speed;
            transform[14] += transform[6] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            transform[12] -= transform[8] * speed;
            transform[13] -= transform[9] * speed;
            transform[14] -= transform[10] * speed;
        }
        if (gd.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            transform[12] += transform[8] * speed;
            transform[13] += transform[9] * speed;
            transform[14] += transform[10] * speed;
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn MouseLookSystem(gd: *zeng.EngineState, look_iterator: *zeng.Iterator(.{ zeng.Transform, zeng.Camera })) !void {
    while (look_iterator.next()) |transform_camera| {
        const transform, _ = transform_camera;

        var pos: [3]f32 = undefined;
        pos[0] = transform[12];
        pos[1] = transform[13];
        pos[2] = transform[14];
        const rot_mat_hor = zeng.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.active_window.getCursorPos().xpos * -0.0015));
        const rot_mat_vert = zeng.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.active_window.getCursorPos().ypos * -0.0015));
        transform.* = zeng.multiply_matrices(rot_mat_hor, rot_mat_vert);
        transform[12] = pos[0];
        transform[13] = pos[1];
        transform[14] = pos[2];
    }
}

/// Render all entities with a transform and a mesh
pub fn RenderSystem(gd: *zeng.EngineState, render_iterator: *zeng.Iterator(.{ zeng.Transform, zeng.Mesh }), skinned_iterator: *zeng.Iterator(.{ zeng.Transform, zeng.SkinnedMesh })) !void {
    zeng.gl.clearColor(0.2, 0.3, 0.3, 1.0);
    zeng.gl.clear(zeng.gl.COLOR_BUFFER_BIT | zeng.gl.DEPTH_BUFFER_BIT);

    const inv_camera_matrix: [16]f32 = zeng.invert_matrix(gd.active_camera_matrix.*);

    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, gd.active_camera.projection_matrix, inv_camera_matrix);
    }

    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_skinned_mesh(skin.*, transform.*, gd.active_camera.projection_matrix, inv_camera_matrix);
    }

    gd.active_window.swapBuffers();
}
