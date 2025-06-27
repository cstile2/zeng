const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const phy = @import("physics.zig");
const aud = @import("audio.zig");
const util = @import("utils.zig");
const net = @import("networking.zig");

pub const COMPONENT_TYPES = [_]type{
    zeng.mesh,
    zeng.camera,
    zeng.skinned_mesh,
    zeng.world_matrix,
    sphere_collider,
    fly_component,
    follow_component,
    children,
    local_matrix,
    player,
    animation_component,
    zeng.skeleton,
    input_implement,
    rpc.input_message,
};
pub const sphere_collider = struct {
    radius: f32 = 1.0,
};
pub const fly_component = struct {};
pub const follow_component = struct {
    anchor_point: zeng.vec3,
    target: ecs.entity_id,
};
pub const animation_player = struct {
    time: f32,
    current_animation: usize,
    animations: []zeng.Animation,
    skeleton_ptr: *zeng.skeleton,
};
pub const children = struct {
    items: []ecs.entity_id,
};
pub const local_matrix = struct {
    transform: zeng.world_matrix = zeng.mat_identity,
};
pub const player = struct {
    velocity: zeng.vec3,
    old_velocity: zeng.vec3 = zeng.vec3.ZERO,
    ground_normal: zeng.vec3,
    grounded: bool,
    animation_controller: ecs.entity_id,
    tilt: zeng.vec3 = zeng.vec3.ZERO,
    camera: ecs.entity_id,
    rot_x: f64 = 0.0,
    rot_y: f64 = 0.0,
};
pub const animation_component = struct {
    time: f32,
    current_animation: usize,
};
pub const input_implement = struct {
    move_fn: *const fn (*zeng.engine_context) zeng.vec2,
    jump_fn: *const fn (*zeng.engine_context) bool,
    pub fn default_move_fn(ctx: *zeng.engine_context) zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (ctx.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            input_vect.x += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            input_vect.x += 1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            input_vect.y += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_move_fn2(ctx: *zeng.engine_context) zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (ctx.active_window.getKey(zeng.glfw.Key.left) == zeng.glfw.Action.press) {
            input_vect.x += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.right) == zeng.glfw.Action.press) {
            input_vect.x += 1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.up) == zeng.glfw.Action.press) {
            input_vect.y += -1;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.down) == zeng.glfw.Action.press) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_jump(ctx: *zeng.engine_context) bool {
        return (ctx.active_window.getKey(zeng.glfw.Key.space) == zeng.glfw.Action.press);
    }
    pub fn default_jump2(ctx: *zeng.engine_context) bool {
        return (ctx.active_window.getKey(zeng.glfw.Key.slash) == zeng.glfw.Action.press);
    }
};

pub const RESOURCE_TYPES = [_]type{
    ecs.world,
    zeng.engine_context,
    zeng.commands,
    time_res,
    input_res,
    main_camera_res,
    text_render_res,
    rect_render_res,
    networking_res,
    sound_res,
    std.Random.Xoshiro256,
    debug_res,
    animation_res,
    std.ArrayList(phy.collider_info),
    cube_tracker_res,
    events([]const u8),
    events([3]zeng.vec3),
    std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)),
    events(rpc.player_spawn_message),
    events(rpc.snapshot_message),
    events(rpc.input_message),
    events(rpc.client_tick),
    events(rpc.server_tick_offset),
    events(rpc.missed_input),
};
pub const time_res = struct {
    delta_time: f64,
    dt: f32,
    elapsed_time: f64,
    fixed_dt: f32,
};
pub const input_res = struct {
    t_down_last_frame: bool,
};
pub const main_camera_res = struct {
    matrix: *[16]f32,
    camera: *zeng.camera,
};
pub const text_render_res = struct {
    shader_program: u32,
    texture: u32,
    vao: u32,
    indices_len: c_int,
};
pub const rect_render_res = struct {
    shader_program: u32,
    vao: u32,
    indices_len: c_int,
};
pub const networking_res = struct {
    main_socket: zeng.net.socket_t,
    server_address: zeng.net.address_t,
    is_server: bool = undefined,
};
pub const sound_res = struct {
    first: aud.audio_sample_info,
};
pub const entity_dependency_res = struct {
    map: std.AutoHashMap(struct { u32, type }, *anyopaque), // gives direct reference to a component

    pub fn get(this: *entity_dependency_res, id: u32, comp_type: type) ?*comp_type {
        return @ptrCast(this.map.get(.{ id, comp_type }) orelse return null);
    }
};
pub const debug_res = @import("render.zig").triangle_debug_info;
pub const animation_res = std.ArrayList(zeng.Animation);
pub const cube_tracker_res = struct {
    map: std.AutoHashMap(phy.ivec3, void),
    cube_mesh: zeng.mesh,
    cube_mesh_data: *const anyopaque,
};

pub fn events(T: type) type {
    return struct {
        array: std.ArrayList(T),
        addresses: ?std.ArrayList(net.Address_t) = null,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{ .array = std.ArrayList(T).init(allocator) };
        }
        pub fn deinit(self: *@This()) void {
            self.array.deinit();
        }
        pub fn send(this: *@This(), event: T) void {
            this.array.append(event) catch unreachable;
        }
        pub fn send_with_address(this: *@This(), event: T, address: net.Address_t) void {
            this.array.append(event) catch unreachable;
            this.addresses.?.append(address) catch unreachable;
        }
        pub fn items(this: *@This()) []T {
            return this.array.items;
        }
        pub fn clear(this: *@This()) void {
            this.array.clearAndFree();
            if (this.addresses != null) this.addresses.?.clearAndFree();
        }
    };
}

pub fn ring_buffer(T: type) type {
    return struct {
        arr: [1000]T,

        pub fn set(self: *@This(), i: isize, x: T) void {
            const _i = if (i >= 0) i else 0;
            self.arr[@as(usize, @intCast(_i)) % self.arr.len] = x;
        }
        pub fn get(self: *@This(), i: isize) T {
            const _i = if (i >= 0) i else 0;
            return self.arr[@as(usize, @intCast(_i)) % self.arr.len];
        }
    };
}

pub const hi_res_tick = struct {
    tick: isize,
    float: f64,
};

pub fn main() !void {
    var ctx: zeng.engine_context = undefined;
    var res: zeng.resources_t = undefined;
    var world: ecs.world = undefined;
    var fet: zeng.resource_fetcher = undefined;
    try zeng.engine_start(&ctx, &res, &world, &fet);
    defer zeng.engine_end(&ctx, &res, &world);
    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});

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
    const main_socket, const _server_address = zeng.net.do_setup("192.168.1.104", 12345, is_server) catch unreachable;
    const server_address = net.Address_t{ .sockaddr = _server_address.any, .socklen = _server_address.getOsSockLen() };
    defer zeng.net.undo_setup(main_socket);

    const triangle_vao, const triangle_vbo = zeng.create_triangle_mesh();
    const cube_vao, const cube_len = zeng.create_cube_mesh_with_normals();
    const cube_collider_pos, const cube_collider_indices = zeng.create_cube_mesh_collider();
    const sky_shader = zeng.load_shader(ctx.allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const rect_shader = zeng.load_shader(ctx.allocator, "assets/shaders/rectangle_vertex.shader", "assets/shaders/rectangle_fragment.shader");
    const static_shader = zeng.load_shader(ctx.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.load_shader(ctx.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.load_shader(ctx.allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.load_texture("assets/images/uv_checker.png", true, false);
    const black_tex = zeng.load_texture("assets/images/black.png", true, false);
    _ = black_tex; // autofix
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .texture = uv_checker_tex }, .vao_gpu = cube_vao };

    var animations = animation_res.init(ctx.arena_allocator);
    res.insert_ptr(&animations);

    // const land_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "land", skin_shader, static_shader, uv_checker_tex);

    const model_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, model_root);
    world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2.ZERO, .dx = 0.0, .dy = 0.0 }, model_root);
    const E = find_child(&world, model_root, zeng.skinned_mesh, fet.fresh_query(.{children})).?;
    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.get(E, zeng.skinned_mesh).?.skeleton);
    world.get(model_root, player).?.animation_controller = world.get(E, zeng.skinned_mesh).?.skeleton;
    world.add(input_implement{ .move_fn = input_implement.default_move_fn, .jump_fn = input_implement.default_jump }, model_root);
    world.get(model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(model_root, zeng.world_matrix).?.*, zeng.vec3{ .y = 10.0 });

    // const castle_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "medieval", skin_shader, static_shader, uv_checker_tex);
    // world.get(castle_root, zeng.world_matrix).?.* = zeng.mat_tran(zeng.mat_scal(zeng.mat_identity, zeng.vec3.ONE.mult(3.5)), zeng.vec3{ .x = -10.0, .z = -6.0, .y = -2.0 });
    // const pistol_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "pistol", skin_shader, static_shader, black_tex);
    const map_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "outdoor_map_6_8_25", skin_shader, static_shader, uv_checker_tex);

    const square_vao, const square_indices_length = zeng.create_square_mesh();
    res.insert(text_render_res{ .shader_program = zeng.load_shader(ctx.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader"), .texture = zeng.load_texture("assets/images/sdf_font.png", false, true), .vao = square_vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands{ .random = res.get(std.Random.Xoshiro256).random(), .remote_messages = undefined, .remote_messages_len = 0, .allocator = ctx.allocator };
    defer commands.destroy();
    res.insert_ptr(&commands);
    res.insert(time_res{ .delta_time = 0.0001, .elapsed_time = 0.0, .dt = 0.006944, .fixed_dt = 1.0 / 60.0 }); // 0.006944
    res.insert(input_res{ .t_down_last_frame = false });
    res.insert(@as(main_camera_res, undefined));
    res.insert_ptr(&ctx);
    res.insert_ptr(&world);
    res.insert(networking_res{ .main_socket = main_socket, .server_address = _server_address, .is_server = is_server });
    res.insert(sound_res{ .first = aud.get_audio_file_data(zeng.get_file_bytes("assets/sounds/ahem.wav", ctx.arena_allocator)) catch unreachable });
    res.insert(@as(cube_tracker_res, undefined));
    res.insert(rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });

    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, follow_component{ .target = model_root, .anchor_point = zeng.mat_position(world.get(model_root, zeng.world_matrix).?.*) } });
    res.get(main_camera_res).matrix = world.get(main_camera, zeng.world_matrix).?;
    res.get(main_camera_res).camera = world.get(main_camera, zeng.camera).?;
    zeng.window_resize_handler(ctx.active_window, ctx.active_window.getSize().width, ctx.active_window.getSize().height);
    ctx.active_window.setInputModeCursor(.disabled);
    world.get(model_root, player).?.camera = main_camera;

    res.get(cube_tracker_res).map = std.AutoHashMap(phy.ivec3, void).init(ctx.allocator);
    defer res.get(cube_tracker_res).map.deinit();
    res.get(cube_tracker_res).cube_mesh = cube_mesh;

    zeng.global_colliders.?.append(zeng.cpu_mesh{ .indices = cube_collider_indices[0..], .positions = util.convert_float_slice_to_vec_slice(cube_collider_pos[0..]) }) catch unreachable;
    zeng.global_matrices.?.append(zeng.mat_identity) catch unreachable;

    var colliders = std.ArrayList(phy.collider_info).init(ctx.allocator);
    defer colliders.deinit();
    res.insert_ptr(&colliders);

    for (zeng.global_colliders.?.items, zeng.global_matrices.?.items) |_mesh, _matrix| {
        var curr_tri: usize = 0;
        while (curr_tri < _mesh.indices.len) {
            defer curr_tri += 3;
            const cool = ctx.arena_allocator.create(phy.mesh_triangle_data) catch unreachable;
            cool.positions = _mesh.positions;
            cool.indices = .{ _mesh.indices[curr_tri], _mesh.indices[curr_tri + 1], _mesh.indices[curr_tri + 2] };
            const collider = phy.collider_info{ .matrix = _matrix, .support = phy.mesh_triangle, .tag = .support_based, .data = @ptrCast(cool) };
            colliders.append(collider) catch unreachable;
        }
    }

    var spatial_hash_grid = std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)).init(ctx.allocator);
    defer spatial_hash_grid.deinit();
    res.insert_ptr(&spatial_hash_grid);

    phy.construct_spatial_hash_grid(colliders, &spatial_hash_grid, ctx.arena_allocator);

    // res.get(cube_tracker_res).cube_mesh_data = @ptrCast(&_data);

    var str_events = events([]const u8).init(ctx.allocator);
    defer str_events.deinit();
    res.insert_ptr(&str_events);
    var tri_events = events([3]zeng.vec3).init(ctx.allocator);
    defer tri_events.deinit();
    res.insert_ptr(&tri_events);
    var plyr_events = events(rpc.player_spawn_message).init(ctx.allocator);
    defer plyr_events.deinit();
    plyr_events.addresses = std.ArrayList(net.Address_t).init(plyr_events.array.allocator);
    res.insert_ptr(&plyr_events);
    var snap_events = events(rpc.state_correction).init(ctx.allocator);
    defer snap_events.deinit();
    snap_events.addresses = std.ArrayList(net.Address_t).init(snap_events.array.allocator);
    res.insert_ptr(&snap_events);
    var input_events = events(rpc.input_chunck).init(ctx.allocator);
    defer input_events.deinit();
    input_events.addresses = std.ArrayList(net.Address_t).init(input_events.array.allocator);
    res.insert_ptr(&input_events);
    var client_tick_events = events(rpc.client_tick).init(ctx.allocator);
    defer client_tick_events.deinit();
    client_tick_events.addresses = std.ArrayList(net.Address_t).init(input_events.array.allocator);
    res.insert_ptr(&client_tick_events);
    var server_tick_offset_events = events(rpc.server_tick_offset).init(ctx.allocator);
    defer server_tick_offset_events.deinit();
    server_tick_offset_events.addresses = std.ArrayList(net.Address_t).init(input_events.array.allocator);
    res.insert_ptr(&server_tick_offset_events);
    var missed_input_events = events(rpc.missed_input).init(ctx.allocator);
    defer missed_input_events.deinit();
    missed_input_events.addresses = std.ArrayList(net.Address_t).init(input_events.array.allocator);
    res.insert_ptr(&missed_input_events);

    var clients = std.AutoHashMap(net.Address_t, ecs.entity_id).init(ctx.allocator);
    defer clients.deinit();

    const client_info = struct {
        input_buffer: ring_buffer(rpc.input_message),
        player: ecs.entity_id,
    };

    var clients2 = std.AutoHashMap(net.Address_t, client_info).init(ctx.allocator);
    defer clients2.deinit();

    var top_children = std.ArrayList(ecs.entity_id).init(ctx.allocator);
    top_children.append(model_root) catch unreachable;
    // top_children.append(model_root2) catch unreachable;
    // top_children.append(pistol_root) catch unreachable;
    // top_children.append(castle_root) catch unreachable;
    // top_children.append(land_root) catch unreachable;
    top_children.append(map_root) catch unreachable;
    defer top_children.deinit();

    if (!is_server) {
        commands.remote_event_(main_socket, server_address, rpc.player_spawn_message{});
        zeng.net.SEND_NET_MESSAGES(&commands);
    }

    var input_buffer: ring_buffer(rpc.input_message) = undefined;
    var pos = ctx.active_window.getCursorPos();
    var last_x: f64 = 0.0;
    var last_y: f64 = 0.0;
    var dx: f64 = 0.0;
    var dy: f64 = 0.0;
    var first = true;
    var synced_time: f64 = 0.0;
    var timescale: f64 = 1.0;
    var sim_timescale: f64 = 1.0;
    var buffer_time: f64 = 0.0;
    var buffer_velocity: f64 = 0.0;
    var buffer_cooldown: f64 = 0.0;
    var warm = false;
    const fixed_delta: f64 = 1.0 / 60.0;
    var accum: f64 = 0.0;
    var tick: isize = 0;
    var draw_local_alignment: f64 = 0.0;
    var draw_time_alignment: f64 = 0.0;
    while (!ctx.active_window.shouldClose()) {
        zeng.start_of_frame();
        defer zeng.end_of_frame(&res);
        commands.time += res.get(time_res).delta_time;
        res.get(time_res).elapsed_time += res.get(time_res).delta_time;
        buffer_cooldown -= res.get(time_res).delta_time;
        if (buffer_cooldown < -10.0) {
            buffer_velocity = -0.004;
        } else if (buffer_cooldown < 0.0) {
            buffer_velocity = 0.0;
        }
        buffer_time += res.get(time_res).delta_time * buffer_velocity;

        zeng.net.RECIEVE_NET_MESSAGES(main_socket, &res);

        pos = ctx.active_window.getCursorPos();
        if (!first) {
            dx = pos.xpos - last_x;
            dy = pos.ypos - last_y;
        } else {
            first = false;
        }
        last_x = pos.xpos;
        last_y = pos.ypos;

        const world_matrix_q = fet.fresh_query(.{zeng.world_matrix});
        const children_q = fet.fresh_query(.{children});
        const local_matrix_q = fet.fresh_query(.{local_matrix});

        for (server_tick_offset_events.array.items, server_tick_offset_events.addresses.?.items) |server_tick_offset_event, _| {
            const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
            draw_time_alignment = time_offset;

            if (@abs(time_offset) > 0.7) {
                std.debug.print("time jump\n", .{});
                synced_time += time_offset;
            } else {
                timescale = 1.0 + (time_offset / 50.0);
            }
            warm = true;
        }
        server_tick_offset_events.clear();
        for (client_tick_events.array.items, client_tick_events.addresses.?.items) |client_tick_event, sockaddr| {
            commands.remote_event(main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accum, .client_time = client_tick_event.time });
        }
        client_tick_events.clear();

        if (warm) {
            const desired_time = synced_time + buffer_time;
            const my_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accum;
            const offset = desired_time - my_time;
            draw_local_alignment = offset;
            if (@abs(offset) > 0.5) {
                // input_tick = desired_input_tick;
                tick += @intFromFloat(offset * 60.0);
                std.debug.print("skipping time\n", .{});
            } else {
                sim_timescale = zeng.lerp(sim_timescale, 1.0 + offset / 5.0, 0.2);
            }
        }

        synced_time += res.get(time_res).delta_time * timescale;
        accum += res.get(time_res).delta_time * sim_timescale;
        while (accum >= fixed_delta) {
            accum -= fixed_delta;
            defer tick += 1;
            res.insert(debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = res.get(main_camera_res).camera.projection_matrix, .inv_camera_matrix = zeng.mat_invert(res.get(main_camera_res).matrix.*) });
            fet.run_system(fly_system);
            fet.run_system(ray_system);
            fet.run_system(spawn_system);
            fet.run_system(player_collision_system);

            // USE INPUT
            if (is_server) {
                var a = clients2.iterator();
                while (a.next()) |_c| {
                    const cool = _c.value_ptr.input_buffer.get(tick);
                    if (cool.tick == tick) {
                        const ent = _c.value_ptr.player;
                        const im = world.get(ent, rpc.input_message).?;
                        im.* = cool;
                    } else {
                        std.debug.print("missed input {}\n", .{tick});
                    }
                    if (_c.value_ptr.input_buffer.get(tick + 5).tick != tick + 5) {
                        commands.remote_event(main_socket, _c.key_ptr.*, rpc.missed_input{});
                    }
                }
            }
            world.get(model_root, rpc.input_message).?.* = rpc.input_message{ .tick = tick, .jump = input_implement.default_jump(&ctx), .move_vect = input_implement.default_move_fn(&ctx), .dx = dx, .dy = dy };
            if (!is_server) {
                input_buffer.set(tick, world.get(model_root, rpc.input_message).?.*);
                var snd: rpc.input_chunck = undefined;
                var curr: usize = 0;
                while (curr < snd.arr.len) {
                    defer curr += 1;
                    snd.arr[curr] = input_buffer.get(tick - @as(isize, @intCast(curr)));
                }
                commands.remote_event(main_socket, server_address, snd);
            }

            // SIMULATE PLAYER
            fet.run_system(player_logic_system);
            fet.run_system(follower_system);
            for (top_children.items) |ch| {
                sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
            }
            // world.get(pistol_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(main_camera, zeng.world_matrix).?.*, zeng.mat_mult_vec4(world.get(main_camera, zeng.world_matrix).?.*, zeng.vec4{ .x = 0.15, .y = -0.15, .z = -0.2 }).to_vec3());

            // for (str_events.array.items) |str| {
            //     zeng.draw_text(str, res.get(text_render_res));
            // }
            str_events.clear();

            // for (tri_events.array.items) |tri| {
            //     zeng.debug_draw_triangle(tri, res.get(debug_res).*);
            // }
            tri_events.clear();

            // bulk client/server communication stuff
            if (is_server) {
                for (plyr_events.array.items, plyr_events.addresses.?.items) |plyr_event, sockaddr| {
                    std.debug.print("Player connected: {}\n", .{plyr_event});

                    const _model_root = zeng.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
                    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, _model_root);
                    world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2{}, .dx = 0.0, .dy = 0.0 }, _model_root);
                    const _E = find_child(&world, _model_root, zeng.skinned_mesh, fet.fresh_query(.{children})).?;
                    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.get(_E, zeng.skinned_mesh).?.skeleton);
                    world.get(_model_root, player).?.animation_controller = world.get(_E, zeng.skinned_mesh).?.skeleton;
                    world.get(_model_root, zeng.world_matrix).?.* = zeng.mat_tran(world.get(_model_root, zeng.world_matrix).?.*, zeng.vec3{ .y = 60.0 });

                    world.get(_model_root, player).?.camera = world.spawn(.{zeng.mat_identity});

                    top_children.append(_model_root) catch unreachable;
                    clients.put(sockaddr, _model_root) catch unreachable;
                    clients2.put(sockaddr, client_info{ .input_buffer = ring_buffer(rpc.input_message){ .arr = undefined }, .player = _model_root }) catch unreachable;
                }
                plyr_events.clear();

                for (input_events.array.items, input_events.addresses.?.items) |input_event, sockaddr| {
                    const info = clients2.getPtr(sockaddr).?;

                    for (input_event.arr) |_input_event| {
                        if (_input_event.tick >= tick) {
                            info.input_buffer.set(_input_event.tick, _input_event);
                        } else {
                            // std.debug.print("late input tick\n", .{});
                        }
                    }
                }
                input_events.clear();

                var it = clients.iterator();
                while (it.next()) |thing| {
                    const P = world.get(thing.value_ptr.*, player).?.*;
                    const M = world.get(thing.value_ptr.*, zeng.world_matrix).?.*;
                    if (@as(usize, @intCast(tick)) % 4 == 0) commands.remote_event(main_socket, thing.key_ptr.*, rpc.state_correction{ .tick = tick, .state = P, .world_matrix = M });
                }
            } else {
                if (@as(usize, @intCast(tick)) % 200 == 0) {
                    commands.remote_event(main_socket, server_address, rpc.client_tick{ .time = synced_time });
                }
                for (snap_events.array.items, snap_events.addresses.?.items) |snap_event, _| {
                    const P = world.get(model_root, player).?;
                    const old = P.*;
                    const old2 = world.get(model_root, zeng.world_matrix).?.*;
                    const temp = P.animation_controller;
                    const temp2 = P.camera;
                    P.* = snap_event.state;
                    P.animation_controller = temp;
                    P.camera = temp2;
                    world.get(model_root, zeng.world_matrix).?.* = snap_event.world_matrix;

                    var _tick = snap_event.tick + 1;
                    while (_tick <= tick) {
                        defer _tick += 1;

                        var im = input_buffer.get(_tick);
                        if (im.tick != _tick) {
                            std.debug.print("missing buffered input for tick: {} {} {}\n", .{ im.tick, _tick, tick });
                            break;
                        }
                        simulate_collision(world.get(model_root, player).?, world.get(model_root, zeng.world_matrix).?, &ctx, &spatial_hash_grid, &tri_events, res.get(debug_res));
                        simulate_player(world.get(model_root, player).?, &im, world.get(model_root, zeng.world_matrix).?, res.get(time_res), fet.fresh_query(.{zeng.world_matrix}));
                    }

                    std.debug.print("{}\n", .{std.meta.eql(old.velocity, P.velocity) and std.meta.eql(old2, world.get(model_root, zeng.world_matrix).?.*)});

                    sync_transforms_children(model_root, world_matrix_q, children_q, local_matrix_q);
                }
                snap_events.clear();
                for (missed_input_events.array.items, missed_input_events.addresses.?.items) |missed_input_event, _| {
                    _ = missed_input_event; // autofix

                    if (warm and buffer_cooldown <= 0.0) {
                        buffer_velocity = 0.04;
                        std.debug.print("increasing buffer\n", .{});
                        buffer_cooldown = 0.3;
                    }
                }
                missed_input_events.clear();
            }
        }

        res.get(main_camera_res).matrix = world.get(main_camera, zeng.world_matrix).?;
        res.get(main_camera_res).camera = world.get(main_camera, zeng.camera).?;

        zeng.gl.useProgram(sky_shader);
        zeng.gl.bindVertexArray(square_vao);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_world_space"), 1, zeng.gl.FALSE, res.get(main_camera_res).matrix);
        zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_perspective"), 1, zeng.gl.FALSE, &res.get(main_camera_res).camera.projection_matrix);
        zeng.gl.drawElements(zeng.gl.TRIANGLES, square_indices_length, zeng.gl.UNSIGNED_INT, null);
        zeng.gl.clear(zeng.gl.DEPTH_BUFFER_BIT);

        zeng.draw_mesh(cube_mesh, zeng.mat_identity, res.get(main_camera_res).camera.projection_matrix, zeng.mat_invert(res.get(main_camera_res).matrix.*));

        fet.run_system(render_system);

        var buffer: [64]u8 = undefined;
        zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / res.get(time_res).delta_time}) catch unreachable, res.get(text_render_res), -0.9, 0.8);
        if (!is_server) {
            zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{synced_time}) catch unreachable, res.get(text_render_res), -0.9, 0.7);
            zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_time}) catch unreachable, res.get(text_render_res), -0.9, 0.6);
            zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{sim_timescale}) catch unreachable, res.get(text_render_res), -0.9, 0.5);
            zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_velocity}) catch unreachable, res.get(text_render_res), -0.9, 0.4);

            zeng.draw_rect(ctx, res.get(rect_render_res), 0, 400, 100, 2, zeng.color_f32.WHITE);
            zeng.draw_rect(ctx, res.get(rect_render_res), 0, 400, 2, 60, zeng.color_f32.WHITE);
            zeng.draw_rect(ctx, res.get(rect_render_res), @as(f32, @floatCast(draw_time_alignment * 100.0)), 400, 4, 30, zeng.color_f32.LIME);

            zeng.draw_rect(ctx, res.get(rect_render_res), 500, 400, 100, 2, zeng.color_f32.WHITE);
            zeng.draw_rect(ctx, res.get(rect_render_res), 500, 400, 2, 60, zeng.color_f32.WHITE);
            zeng.draw_rect(ctx, res.get(rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 100.0)), 400, 4, 30, zeng.color_f32.LIME);
        } else {
            zeng.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accum}) catch unreachable, res.get(text_render_res), -0.9, 0.7);
        }

        commands.process_commands(&world);
        zeng.net.SEND_NET_MESSAGES(&commands);
        ctx.active_window.swapBuffers();
    }
}

/// Spawns entities random: *std.Random.Xoshiro256,
pub fn spawn_system(ctx: *zeng.engine_context, sound_list: *sound_res, commands: *zeng.commands, input: *input_res, netres: *networking_res) !void {
    _ = commands; // autofix
    const t_down = ctx.active_window.getKey(zeng.glfw.Key.t) == zeng.glfw.Action.press;
    defer input.t_down_last_frame = t_down;

    if (t_down and !input.t_down_last_frame) {
        if (!netres.is_server) {
            // commands.remote_call(netres.main_socket, netres.server_address, rpc.CTS_print_hello, .{});
            // commands.remote_call(netres.main_socket, netres.server_address, rpc.CTS_print_int, .{85});
        }

        aud.play_sound(sound_list.first, .one_shot);
    }
}

/// Make all entities with a CircleCollider collide with each other
pub fn circle_collision_system(q: *ecs.query(.{ zeng.world_matrix, sphere_collider })) !void {
    var A = q.iterator();
    while (A.next()) |_A| {
        const transformA, _ = _A;
        var B = A;
        while (B.next()) |_B| {
            const transformB, _ = _B;
            if (transformA == transformB) unreachable;

            const p_a = zeng.mat_position(transformA.*);
            const p_b = zeng.mat_position(transformB.*);

            const radius = 0.6;
            const delta = p_a.sub(p_b);
            if (delta.length_sq() > 0.0 and delta.length() < 2.0 * radius) {
                const push = (2.0 * radius - delta.length()) * 0.5;
                zeng.mat_position_set(transformB, p_b.add(delta.normalized().mult(-push)));
                zeng.mat_position_set(transformA, p_a.add(delta.normalized().mult(push)));
            }
        }
    }
}

/// Make all entities with a camera and a transform component fly around like a spectator
pub fn fly_system(ctx: *zeng.engine_context, cam: *main_camera_res, time: *time_res, q: *ecs.query(.{ zeng.world_matrix, fly_component })) !void {
    _ = time; // autofix
    var it = q.iterator();
    while (it.next()) |transform_flyer| {
        const transform, _ = transform_flyer;

        var speed: f32 = 0.2; //@floatCast(time.delta_time * 100.0);
        if (ctx.active_window.getKey(zeng.glfw.Key.left_shift) == zeng.glfw.Action.press) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.a) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.d) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam.matrix.*).mult(speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.q) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.e) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam.matrix.*).mult(speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.w) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam.matrix.*).mult(-speed)));
        }
        if (ctx.active_window.getKey(zeng.glfw.Key.s) == zeng.glfw.Action.press) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam.matrix.*).mult(speed)));
        }
    }
}

/// Make all entities with a transform and a camera component rotate using FPS mouse controls
pub fn mouse_look_system(ctx: *zeng.engine_context, q: *ecs.query(.{ zeng.world_matrix, zeng.camera })) !void {
    var look_iterator = q.iterator();
    while (look_iterator.next()) |transform_camera| {
        const transform, _ = transform_camera;

        const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(ctx.active_window.getCursorPos().xpos * -0.0015));
        const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(ctx.active_window.getCursorPos().ypos * -0.0015));
        transform.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(transform.*));
    }
}

pub fn camera_rotation(matrix: *zeng.world_matrix, x: f64, y: f64) void {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.0015));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.0015));
    matrix.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(matrix.*));
}

/// Render all entities with a transform and a mesh
pub fn render_system(world: *ecs.world, cam: *main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam.matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.draw_mesh(mesh.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.draw_animated_skinned_mesh(world, skin.*, transform.*, cam.camera.projection_matrix, inv_camera_matrix);
    }

    // zeng.draw_text("the end is never the end is never the end is never the ", ui_ren);

}

/// Makes all follower entities follow their target
pub fn follower_system(T: *time_res, follower_q: *ecs.query(.{ follow_component, zeng.world_matrix }), followee_q: *ecs.query(.{ player, zeng.world_matrix })) !void {
    var follower_it = follower_q.iterator();
    while (follower_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.mat_position(followee_q.get(cam_follower.target, zeng.world_matrix).?.*);

        cam_follower.anchor_point = zeng.vec3.lerp(cam_follower.anchor_point, target_position, 5.0 * T.fixed_dt);

        zeng.mat_position_set(cam_transform, cam_follower.anchor_point.add(zeng.mat_forward(cam_transform.*).mult(2.5)).add(zeng.vec3{ .y = 0.5 }));
    }
}

pub fn simulate_collision(plyr: *player, world_matrix: *zeng.world_matrix, ctx: *zeng.engine_context, spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info)), tri_ev: *events([3]zeng.vec3), debug: *debug_res) void {
    _ = tri_ev; // autofix
    const b_coll = phy.collider_info{ .data = undefined, .matrix = world_matrix.*, .support = phy.dual_point };
    const old_grounded = plyr.grounded;
    var closest_dist = std.math.floatMax(f32);
    var cloest_point: zeng.vec3 = undefined;
    var combined_normal = zeng.vec3.ZERO;
    var combined_normal_count: usize = 0;
    plyr.grounded = false;

    const right, const left, const up, const down, const forward, const backward = phy.collider_bounds(b_coll);

    var collection = std.ArrayList(std.ArrayList(*phy.collider_info)).init(ctx.allocator);
    defer collection.deinit();

    var already_checked = std.AutoHashMap(*phy.collider_info, void).init(ctx.allocator);
    defer already_checked.deinit();

    var i: isize = left;
    while (i <= right) {
        defer i += 1;

        var j: isize = down;
        while (j <= up) {
            defer j += 1;

            var k: isize = backward;
            while (k <= forward) {
                defer k += 1;

                // const vec = zeng.vec3{
                //     .x = @as(f32, @floatFromInt(i)) * phy.GRID_SIZE,
                //     .y = @as(f32, @floatFromInt(j)) * phy.GRID_SIZE,
                //     .z = @as(f32, @floatFromInt(k)) * phy.GRID_SIZE,
                // };
                // tri_ev.send(.{ vec, vec.add(zeng.vec3.UP.mult(0.1)), vec.add(zeng.vec3.RIGHT.mult(0.1)) });

                const guy = spatial_hash_grid.get(.{ i, j, k });
                if (guy != null) collection.append(guy.?) catch unreachable;
            }
        }
    }

    for (collection.items) |Q| {
        for (Q.items) |coll| {
            if (already_checked.contains(coll)) continue;
            already_checked.put(coll, void{}) catch unreachable;

            if (coll.tag != .support_based) unreachable; // just for now

            // const coll_data = @as(*const phy.mesh_triangle_data, @alignCast(@ptrCast(coll.data)));

            // tri_ev.send(.{
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[0]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[1]].to_vec4(1.0)).to_vec3(),
            //     zeng.mat_mult_vec4(coll.matrix, coll_data.positions[coll_data.indices[2]].to_vec4(1.0)).to_vec3(),
            // });

            const p = phy.shape_separation(coll.*, b_coll, debug.*, 10);
            if (p.length() < 0.35) {
                if (p.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
                    plyr.grounded = true;
                    plyr.ground_normal = p.neg().normalized();
                }
                world_matrix.* = zeng.mat_tran(world_matrix.*, p.add(p.neg().normalized().mult(0.35)));
                combined_normal = combined_normal.add(p.neg().normalized());
                combined_normal_count += 1;
            }
            if (p.length() < closest_dist) {
                cloest_point = p;
                closest_dist = p.length();
            }
        }
    }

    if (old_grounded and !plyr.grounded and closest_dist < 0.5) {
        if (cloest_point.neg().normalized().dot(zeng.vec3.UP) > 0.5) {
            plyr.grounded = true;
            plyr.ground_normal = cloest_point.neg().normalized();
            world_matrix.* = zeng.mat_tran(world_matrix.*, cloest_point.add(cloest_point.neg().normalized().mult(0.35)));
        }
    }
    if (combined_normal_count > 0) {
        plyr.velocity = plyr.velocity.slide(combined_normal);
    }
}

pub fn player_collision_system(ctx: *zeng.engine_context, player_q: *ecs.query(.{ player, zeng.world_matrix }), debug: *debug_res, tri_ev: *events([3]zeng.vec3), spatial_hash_grid: *std.AutoHashMap(phy.ivec3, std.ArrayList(*phy.collider_info))) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const plyr, const world_matrix = player_curr;
        simulate_collision(plyr, world_matrix, ctx, spatial_hash_grid, tri_ev, debug);
    }
}

pub fn simulate_player(p: *player, im: *const rpc.input_message, m: *zeng.world_matrix, T: *time_res, camera_q: *ecs.query(.{zeng.world_matrix})) void {
    p.rot_x += im.dx;
    p.rot_y += im.dy;

    const cam_matrix = camera_q.get(p.camera, zeng.world_matrix).?;

    camera_rotation(cam_matrix, p.rot_x, p.rot_y);

    if (im.jump and p.grounded) {
        p.velocity = p.velocity.add(zeng.vec3{ .y = 6 });
        p.grounded = false;
        p.ground_normal = zeng.vec3.UP;
    }

    const acc: f32 = 20.0;
    const basis_right = zeng.mat_right(cam_matrix.*).slide(p.ground_normal).normalized();
    const basis_forward = basis_right.cross(p.ground_normal);
    var move_vect = basis_right.mult(im.move_vect.x).add(basis_forward.mult(im.move_vect.y));

    var tilt = zeng.vec3.ZERO;
    if (p.grounded) {
        if (im.move_vect.length() > 0.1) {
            if (p.velocity.length_sq() > 0.01) {
                const g = move_vect.sub(p.velocity.normalized()).clamp(1.0);
                const h = g.mult(2.0).add(move_vect).normalized();
                var h_v = h.project(p.velocity);
                const h_h = h.sub(h_v);
                if (p.velocity.length() > 3.8 and h_v.dot(p.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                tilt = h_v.add(h_h);
                p.velocity = p.velocity.add(h_v.add(h_h).mult(acc * T.fixed_dt));
            } else {
                p.velocity = p.velocity.add(move_vect.mult(acc * T.fixed_dt));
            }
        } else {
            tilt = p.velocity.neg().clamp(1.0);
            p.velocity = p.velocity.add(p.velocity.neg().clamp(acc * T.fixed_dt));
        }
    } else {
        p.velocity = p.velocity.add(zeng.vec3.UP.mult(-9.8 * T.fixed_dt));
        p.ground_normal = zeng.vec3.UP;
        p.velocity = p.velocity.add(move_vect.mult(acc * 0.3 * T.fixed_dt));
        p.velocity = p.velocity.slide(zeng.vec3.UP).add(p.velocity.project(zeng.vec3.UP));
    }
    p.tilt = p.tilt.lerp(tilt, 8.0 * T.fixed_dt);
    m.* = zeng.mat_tran(m.*, p.velocity.mult(T.fixed_dt));

    if (p.velocity.slide(zeng.vec3.UP).length() > 0.05) {
        p.old_velocity = p.old_velocity.slerp(p.velocity.slide(zeng.vec3.UP).normalized(), 8 * T.fixed_dt);
    }
    if (p.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
        const _up = (zeng.vec3.UP.add(p.tilt.mult(0.3))).normalized();
        m.* = zeng.mat_rebasis(m.*, _up.cross(p.old_velocity.slide(_up)).normalized(), _up, p.old_velocity.slide(_up).normalized());
    }
}

pub fn player_logic_system(T: *time_res, player_q: *ecs.query(.{ player, rpc.input_message, zeng.world_matrix }), animator_q: *ecs.query(.{ zeng.skeleton, animation_component }), ctx: *zeng.engine_context, animations: *animation_res, camera_q: *ecs.query(.{zeng.world_matrix})) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const p, const im: *rpc.input_message, const m = player_curr;

        simulate_player(p, im, m, T, camera_q);

        const anim = animator_q.get(p.animation_controller, animation_component).?;
        const skel = animator_q.get(p.animation_controller, zeng.skeleton).?;
        const blend = p.velocity.div(3.0).clamp(1.0).length();

        anim.time += T.fixed_dt / zeng.lerp(animations.items[1].duration, animations.items[4].duration, blend);
        while (anim.time > 1.0) {
            anim.time -= 1.0;
        }

        const rotations = ctx.allocator.alloc(zeng.quat, skel.bone_parent_indices.len) catch unreachable;
        const translations = ctx.allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
        const scales = ctx.allocator.alloc(zeng.vec3, skel.bone_parent_indices.len) catch unreachable;
        get_animation_pose_with_weight(&animations.items[4], anim.time, .{ rotations, translations, scales }, blend);
        add_animation_pose_with_weight(&animations.items[1], anim.time, .{ rotations, translations, scales }, 1.0 - blend);
        normalize_pose(.{ rotations, translations, scales });
        apply_pose(skel, .{ rotations, translations, scales });
        ctx.allocator.free(rotations);
        ctx.allocator.free(translations);
        ctx.allocator.free(scales);
    }
}

pub fn ray_system(ctx: *zeng.engine_context, world: *ecs.world, camera: *main_camera_res, colliders: *std.ArrayList(phy.collider_info), cube_tracker: *cube_tracker_res, str_events: *events([]const u8)) !void {
    _ = str_events; // autofix
    _ = world; // autofix
    _ = cube_tracker; // autofix
    const ro = zeng.mat_position(camera.matrix.*);
    const rd = zeng.mat_forward(camera.matrix.*).neg();
    var min_result: phy.raycast_result = undefined;
    const hit = phy.ray_cast(ro, rd, colliders.items, &min_result);
    if (hit and ctx.active_window.getKey(zeng.glfw.Key.f) == zeng.glfw.Action.press) {
        const new_pos = ro.add(rd.mult(min_result.t)).add(min_result.normal.normalized());
        const new_pos_rounded = zeng.vec3{
            .x = @round(new_pos.x / 2.0) * 2.0,
            .y = @round(new_pos.y / 2.0) * 2.0,
            .z = @round(new_pos.z / 2.0) * 2.0,
        };
        _ = new_pos_rounded; // autofix
        // if (!cube_tracker.map.contains(.{ @intFromFloat(new_pos_rounded.x), @intFromFloat(new_pos_rounded.y), @intFromFloat(new_pos_rounded.z) })) {
        //     _ = world.spawn(.{ cube_tracker.cube_mesh, zeng.mat_tran(zeng.mat_identity, new_pos_rounded) });
        //     cube_tracker.map.put(.{ @intFromFloat(new_pos_rounded.x), @intFromFloat(new_pos_rounded.y), @intFromFloat(new_pos_rounded.z) }, void{}) catch unreachable;
        //     colliders.append(phy.collider_info{ .data = cube_tracker.cube_mesh_data, .matrix = zeng.mat_tran(zeng.mat_identity, new_pos_rounded), .support = undefined, .tag = .mesh }) catch unreachable;
        // }
    }
    // if (hit) str_events.send("hit") else str_events.send("no");
    // f_pressed = ctx.active_window.getKey(zeng.glfw.Key.f) == zeng.glfw.Action.press;
}

// Extra stuff
pub fn find_child(world: *ecs.world, parent: ecs.entity_id, component_type: type, q_children: *ecs.query(.{children})) ?ecs.entity_id {
    if (world.get(parent, component_type) != null) return parent;

    const childrens = world.get(parent, children) orelse return null;
    for (childrens.items) |_c| {
        const res = find_child(world, _c, component_type, q_children);
        if (res != null) return res.?;
    }

    return null;
}
pub fn sync_transforms_children(id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn sync_transforms_recursive(parent_global: zeng.world_matrix, id: ecs.entity_id, q_transform: *ecs.query(.{zeng.world_matrix}), q_children: *ecs.query(.{children}), q_local_transform: *ecs.query(.{local_matrix})) void {
    const local = q_local_transform.get(id, local_matrix) orelse return;
    const global = q_transform.get(id, zeng.world_matrix) orelse return;
    global.* = zeng.mat_mult(parent_global, local.transform);

    const childrens = q_transform.get(id, children) orelse return;
    for (childrens.items) |_c| {
        sync_transforms_recursive(global.*, _c, q_transform, q_children, q_local_transform);
    }
}
pub fn binary_search(inputs: []const f32, time: f32) usize {
    if (time <= inputs[0]) return 0;
    if (time >= inputs[inputs.len - 1]) return inputs.len - 2;

    var left: usize = 0;
    var right: usize = inputs.len - 1;

    while (left < right - 1) {
        const mid = left + (right - left) / 2;
        if (inputs[mid] <= time) {
            left = mid;
        } else {
            right = mid;
        }
    }
    return left;
}
pub fn animation_pose(anim: *animation_player, delta_time: f32) void {
    const skeleton = anim.skeleton_ptr;
    anim.time += delta_time;
    while (anim.time > anim.animations[anim.current_animation].duration) {
        anim.time -= anim.animations[anim.current_animation].duration;
    }
    var rotations: [100]zeng.quat = .{zeng.quat.IDENTITY} ** 100;
    var translations: [100]zeng.vec3 = .{zeng.vec3.ZERO} ** 100;
    var scales: [100]zeng.vec3 = .{zeng.vec3.ONE} ** 100;

    for (anim.animations[anim.current_animation].channels) |channel| {
        const time = binary_search(channel.inputs, anim.time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[time], channel.inputs[time + 1], anim.time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[time].nlerp(channel.outputs.rotation[time + 1], lerp_amount);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[time].lerp(channel.outputs.translation[time + 1], lerp_amount);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[time].lerp(channel.outputs.scale[time + 1], lerp_amount);
        }
    }

    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotations[curr]), zeng.mat_scal(zeng.mat_identity, scales[curr])), translations[curr]);
        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }
        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}
pub fn get_animation_pose_with_weight(animation: *zeng.Animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .translation) {
            translations[channel.target] = channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight);
        } else if (channel.outputs == .scale) {
            scales[channel.target] = channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight);
        }
    }
}
pub fn add_animation_pose_with_weight(animation: *zeng.Animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
    const time = time_norm * animation.duration;
    const rotations = pose[0];
    const translations = pose[1];
    const scales = pose[2];
    for (animation.channels) |channel| {
        const idx = binary_search(channel.inputs, time);
        const lerp_amount = zeng.inv_lerp(channel.inputs[idx], channel.inputs[idx + 1], time);

        if (channel.outputs == .rotation) {
            rotations[channel.target] = rotations[channel.target].add2(channel.outputs.rotation[idx].nlerp(channel.outputs.rotation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .translation) {
            translations[channel.target] = translations[channel.target].add(channel.outputs.translation[idx].lerp(channel.outputs.translation[idx + 1], lerp_amount).mult(weight));
        } else if (channel.outputs == .scale) {
            scales[channel.target] = scales[channel.target].add(channel.outputs.scale[idx].lerp(channel.outputs.scale[idx + 1], lerp_amount).mult(weight));
        }
    }
}
pub fn normalize_pose(pose: zeng.skeleton_pose) void {
    const rotations = pose[0];
    for (rotations) |*r| {
        r.* = r.normalize();
    }
}
pub fn apply_pose(skeleton: *zeng.skeleton, pose: zeng.skeleton_pose) void {
    var curr: usize = 0;
    while (curr < skeleton.bone_parent_indices.len) {
        skeleton.local_bone_matrices[curr] = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(pose[0][curr]), zeng.mat_scal(zeng.mat_identity, pose[2][curr])), pose[1][curr]);
        const parent_index = skeleton.bone_parent_indices[curr];
        if (parent_index != -1) {
            skeleton.local_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[@intCast(parent_index)], skeleton.local_bone_matrices[curr]);
        }
        skeleton.model_bone_matrices[curr] = zeng.mat_mult(skeleton.local_bone_matrices[curr], skeleton.inverse_bind_matrices[curr]);
        curr += 1;
    }
}

// need quality collision system - gjk[O] + epa[ ] + shapecast[X] + plane method[ ] -> need spatial acceleration [0]
// make audio system more robust and accurate
// robust text rendering
// better rendering - lights
// better material system
