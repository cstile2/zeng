const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const rpc = @import("rpc.zig");
const phy = @import("physics.zig");
const aud = @import("audio.zig");
const util = @import("utils.zig");
const net = @import("networking.zig");
const gl = zeng.gl;
const c = zeng.c;

pub const component_name = enum {
    mesh,
    camera,
    skinned_mesh,
    world_matrix,
    sphere_collider,
    flyer,
    follower,
    children,
    local_matrix,
    player,
    animation_state,
    skeleton,
    input_implement,
    input_data,
    floater,
    snapshot_interpolator,
};

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
    floater_component,
    snapshot_interpolator,
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
    // rot_x: f64 = 0.0,
    // rot_y: f64 = 0.0,
};
pub const animation_component = struct {
    time: f32,
    current_animation: usize,
};
pub const input_implement = struct {
    move_fn: *const fn () zeng.vec2,
    jump_fn: *const fn () bool,
    pub fn default_move_fn() zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (zeng.get_key(.a)) {
            input_vect.x += -1;
        }
        if (zeng.get_key(.d)) {
            input_vect.x += 1;
        }
        if (zeng.get_key(.w)) {
            input_vect.y += -1;
        }
        if (zeng.get_key(.s)) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_move_fn2() zeng.vec2 {
        var input_vect = zeng.vec2.ZERO;
        if (zeng.get_key(.left)) {
            input_vect.x += -1;
        }
        if (zeng.get_key(.right)) {
            input_vect.x += 1;
        }
        if (zeng.get_key(.up)) {
            input_vect.y += -1;
        }
        if (zeng.get_key(.down)) {
            input_vect.y += 1;
        }
        return input_vect.clamp(1);
    }
    pub fn default_jump() bool {
        return zeng.get_key(.space);
    }
    pub fn default_jump2() bool {
        return zeng.get_key(.y);
    }
};
pub const floater_component = struct {};
pub const snapshot_interpolator = struct {
    buffer: ring_buffer(struct { position: zeng.vec3, tick: isize }),
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
    events(rpc.world_update),
};
pub const time_res = struct {
    delta_time: f64,
    dt: f32,
    fixed_delta_time: f64,
    fixed_dt: f32,
};
pub const input_res = struct {
    t_down_last_frame: bool,
};
pub const main_camera_res = struct {
    id: ecs.entity_id,
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
    server_address: zeng.net.Address,
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
pub const animation_res = std.ArrayList(zeng.loader.Animation);
pub const cube_tracker_res = struct {
    map: std.AutoHashMap(phy.ivec3, void),
    cube_mesh: zeng.mesh,
    cube_mesh_data: *const anyopaque,
};

pub fn events(T: type) type {
    return struct {
        array: std.ArrayList(T),
        addresses: ?std.ArrayList(net.sockaddr_socklen_t) = null,

        pub fn init(allocator: std.mem.Allocator, networked: bool) @This() {
            var ret = @This(){ .array = std.ArrayList(T).init(allocator) };
            if (networked) ret.addresses = std.ArrayList(net.sockaddr_socklen_t).init(allocator);
            return ret;
        }
        pub fn deinit(self: *@This()) void {
            self.array.deinit();
        }
        pub fn send(this: *@This(), event: T) void {
            this.array.append(event) catch unreachable;
        }
        pub fn send_with_address(this: *@This(), event: T, address: net.sockaddr_socklen_t) void {
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
        arr: [800]T,

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
const client_info = struct {
    input_buffer: ring_buffer(rpc.input_message),
    player: ecs.entity_id,
};

pub var global_world_ptr: *ecs.world = undefined;
pub var global_model_root: ecs.entity_id = undefined;

pub fn main() !void {
    var ctx: zeng.engine_context = undefined;
    var res: zeng.resources_t = undefined;
    var world: ecs.world = undefined;
    var fet: zeng.resource_fetcher = undefined;
    try zeng.engine_start(&ctx, &res, &world, &fet);
    defer zeng.engine_end(&ctx, &res, &world);
    _ = try std.Thread.spawn(.{}, aud.audio_engine_run, .{});
    global_world_ptr = &world;

    var is_server: bool = true;
    std.debug.print("\nselect network mode:\n", .{});
    var stdin = std.fs.File.stdin().readerStreaming(&.{});
    var buff: [1024]u8 = undefined;
    _ = stdin.read(&buff) catch unreachable;
    if (std.mem.eql(u8, buff[0..1], "s")) {
        std.debug.print("using server mode...\n", .{});
    } else if (std.mem.eql(u8, buff[0..1], "c")) {
        std.debug.print("using client mode...\n", .{});
        is_server = false;
    }

    const main_socket, const _server_address = zeng.net.do_setup("127.0.0.1", 12345, is_server) catch unreachable;
    defer zeng.net.undo_setup(main_socket);
    const server_address = net.sockaddr_socklen_t{ .sockaddr = _server_address.any, .socklen = @intCast(_server_address.getOsSockLen()) };

    const triangle_vao, const triangle_vbo = zeng.loader.create_triangle_mesh();
    const cube_vao, const cube_len = zeng.loader.create_cube_mesh_with_normals();
    var cube_collider_pos, var cube_collider_indices = zeng.loader.create_cube_mesh_collider();
    const sky_shader = zeng.loader.load_shader(ctx.allocator, "assets/shaders/sky_vertex.shader", "assets/shaders/sky_fragment.shader");
    const rect_shader = zeng.loader.load_shader(ctx.allocator, "assets/shaders/rectangle_vertex.shader", "assets/shaders/rectangle_fragment.shader");
    const static_shader = zeng.loader.load_shader(ctx.allocator, "assets/shaders/basic_vertex.shader", "assets/shaders/basic_fragment.shader");
    const skin_shader = zeng.loader.load_shader(ctx.allocator, "assets/shaders/skinned_vertex.shader", "assets/shaders/basic_fragment.shader");
    const debug_shader = zeng.loader.load_shader(ctx.allocator, "assets/shaders/debug_vertex.shader", "assets/shaders/debug_fragment.shader");
    const uv_checker_tex = zeng.loader.load_texture("assets/images/uv_checker.png", true, false);
    const black_tex = zeng.loader.load_texture("assets/images/black.png", true, false);
    const cube_mesh = zeng.mesh{ .indices_length = cube_len, .indices_type = zeng.gl.UNSIGNED_INT, .material = zeng.material{ .shader_program = static_shader, .texture = uv_checker_tex }, .vao_gpu = cube_vao };
    const gun_shot = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/gun_shot.wav", ctx.arena_allocator)) catch unreachable;
    const bell = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/bell.wav", ctx.arena_allocator)) catch unreachable;

    var animations = animation_res.init(ctx.arena_allocator);
    res.insert_ptr(&animations);

    const pistol_root = zeng.loader.auto_import(&ctx, &world, &res, "assets/gltf/", "pistol", skin_shader, static_shader, black_tex);
    const map_root = zeng.loader.auto_import(&ctx, &world, &res, "assets/gltf/", "outdoor_map_6_8_25", skin_shader, static_shader, uv_checker_tex);
    const cube_root = world.spawn(.{ cube_mesh, zeng.mat_tran(zeng.mat_identity, .{ .x = -7.0, .y = 2.0 }), floater_component{} });
    if (!is_server) world.add(snapshot_interpolator{ .buffer = undefined }, cube_root);

    const model_root = zeng.loader.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
    world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, model_root);
    world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2.ZERO, .rot_x = 0.0, .rot_y = 0.0 }, model_root);
    const E = find_component_of_type(&world, model_root, .skinned_mesh, fet.fresh_query(.{children})).?;
    world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.id_get(E, .skinned_mesh).?.skeleton);
    world.id_get(model_root, .player).?.animation_controller = world.id_get(E, .skinned_mesh).?.skeleton;
    world.add(input_implement{ .move_fn = input_implement.default_move_fn, .jump_fn = input_implement.default_jump }, model_root);
    world.id_get(model_root, .world_matrix).?.* = zeng.mat_tran(world.id_get(model_root, .world_matrix).?.*, zeng.vec3{ .y = 10.0 });
    world.remove(zeng.skinned_mesh, E);
    var found_entity = find_component_of_type(&world, model_root, .skinned_mesh, fet.fresh_query(.{children}));
    while (found_entity) |_| {
        world.remove(zeng.skinned_mesh, found_entity.?);
        found_entity = find_component_of_type(&world, model_root, .skinned_mesh, fet.fresh_query(.{children}));
    }
    global_model_root = model_root;

    var model_root2: ecs.entity_id = undefined;
    if (!is_server) {
        model_root2 = zeng.loader.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
        const F = find_component_of_type(&world, model_root2, .skinned_mesh, fet.fresh_query(.{children})).?;
        world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.id_get(F, .skinned_mesh).?.skeleton);
        world.id_get(model_root2, .world_matrix).?.* = zeng.mat_tran(world.id_get(model_root2, .world_matrix).?.*, zeng.vec3{ .y = 10.0 });
        world.add(snapshot_interpolator{ .buffer = undefined }, model_root2);
    }

    const square_vao, const square_indices_length = zeng.loader.create_square_mesh();
    res.insert(text_render_res{ .shader_program = zeng.loader.load_shader(ctx.allocator, "assets/shaders/screen.shader", "assets/shaders/screenfrag.shader"), .texture = zeng.loader.load_texture("assets/images/sdf_font.png", false, true), .vao = square_vao, .indices_len = square_indices_length });
    res.insert(std.Random.DefaultPrng.init(123));
    var commands = zeng.commands{ .reliable_messages = undefined, .reliable_message_seqs = std.AutoHashMap(usize, void).init(ctx.allocator), .random = res.get(std.Random.Xoshiro256).random(), .remote_messages = undefined, .remote_messages_len = 0, .allocator = ctx.allocator };
    defer commands.destroy();
    res.insert_ptr(&commands);
    res.insert(input_res{ .t_down_last_frame = false });
    res.insert_ptr(&ctx);
    res.insert_ptr(&world);
    res.insert(networking_res{ .main_socket = main_socket, .server_address = _server_address, .is_server = is_server });
    res.insert(sound_res{ .first = aud.get_audio_file_data(zeng.loader.get_file_bytes("assets/sounds/ahem.wav", ctx.arena_allocator)) catch unreachable });
    res.insert(@as(cube_tracker_res, undefined));
    res.insert(rect_render_res{ .shader_program = rect_shader, .vao = square_vao, .indices_len = square_indices_length });
    const main_camera = world.spawn(.{ zeng.camera{ .projection_matrix = undefined }, zeng.mat_identity, follow_component{ .target = model_root, .anchor_point = zeng.mat_position(world.id_get(model_root, .world_matrix).?.*) } });
    res.insert(debug_res{ .vao = triangle_vao, .vbo = triangle_vbo, .debug_shader = debug_shader, .projection_matrix = world.id_get(main_camera, .camera).?.projection_matrix, .inv_camera_matrix = zeng.mat_invert(world.id_get(main_camera, .world_matrix).?.*) });
    res.insert(main_camera_res{ .id = main_camera });
    world.id_get(model_root, .player).?.camera = main_camera;
    zeng.window_resize_handler(&res, ctx.width, ctx.height);
    var client_map = std.AutoHashMap(net.sockaddr_socklen_t, client_info).init(ctx.allocator);
    defer client_map.deinit();

    res.get(cube_tracker_res).map = std.AutoHashMap(phy.ivec3, void).init(ctx.allocator);
    defer res.get(cube_tracker_res).map.deinit();
    res.get(cube_tracker_res).cube_mesh = cube_mesh;

    zeng.loader.global_colliders.?.append(zeng.cpu_mesh{ .indices = cube_collider_indices[0..], .positions = util.convert_float_slice_to_vec_slice(cube_collider_pos[0..]) }) catch unreachable;
    zeng.loader.global_matrices.?.append(zeng.mat_identity) catch unreachable;
    var colliders = std.ArrayList(phy.collider_info).init(ctx.allocator);
    defer colliders.deinit();
    res.insert_ptr(&colliders);
    for (zeng.loader.global_colliders.?.items, zeng.loader.global_matrices.?.items) |_mesh, _matrix| {
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
    var str_events = events([]const u8).init(ctx.allocator, false);
    defer str_events.deinit();
    res.insert_ptr(&str_events);
    var tri_events = events([3]zeng.vec3).init(ctx.allocator, false);
    defer tri_events.deinit();
    res.insert_ptr(&tri_events);
    var player_join_events = events(rpc.player_spawn_message).init(ctx.allocator, true);
    defer player_join_events.deinit();
    res.insert_ptr(&player_join_events);
    var snap_events = events(rpc.state_correction).init(ctx.allocator, true);
    defer snap_events.deinit();
    res.insert_ptr(&snap_events);
    var input_events = events(rpc.input_chunck).init(ctx.allocator, true);
    defer input_events.deinit();
    res.insert_ptr(&input_events);
    var client_tick_events = events(rpc.client_tick).init(ctx.allocator, true);
    defer client_tick_events.deinit();
    res.insert_ptr(&client_tick_events);
    var server_tick_offset_events = events(rpc.server_tick_offset).init(ctx.allocator, true);
    defer server_tick_offset_events.deinit();
    res.insert_ptr(&server_tick_offset_events);
    var missed_input_events = events(rpc.missed_input).init(ctx.allocator, true);
    defer missed_input_events.deinit();
    res.insert_ptr(&missed_input_events);
    var world_update_events = events(rpc.world_update).init(ctx.allocator, true);
    defer world_update_events.deinit();
    res.insert_ptr(&world_update_events);

    var top_children = std.ArrayList(ecs.entity_id).init(ctx.allocator);
    top_children.append(model_root) catch unreachable;
    top_children.append(pistol_root) catch unreachable;
    top_children.append(map_root) catch unreachable;
    if (!is_server) top_children.append(model_root2) catch unreachable;
    defer top_children.deinit();

    if (!is_server) {
        commands.remote_event(main_socket, server_address, rpc.player_spawn_message{}, .reliable);
    }

    const fixed_rate: f64 = 60.0;
    const fixed_delta: f64 = 1.0 / fixed_rate;
    res.insert(time_res{ .delta_time = 0.006944, .dt = 0.006944, .fixed_delta_time = fixed_delta, .fixed_dt = @floatCast(fixed_delta) });
    res.get(time_res).fixed_dt = @floatCast(fixed_delta);

    var client_input_buffer: ring_buffer(rpc.input_message) = undefined;
    var synced_time: f64 = 0.0;
    var timescale: f64 = 1.0;
    var sim_timescale: f64 = 1.0;
    var buffer_time: f64 = 0.0;
    var buffer_velocity: f64 = 0.0;
    var buffer_cooldown: f64 = 0.0;
    var warm = false;
    var accum: f64 = 0.0;
    var tick: isize = 0;
    var draw_local_alignment: f64 = 0.0;
    var draw_time_alignment: f64 = 0.0;
    var draw_rtt: f64 = 0.0;
    var draw_resims: usize = 0;
    var mouse_pressed_last_frame = false;
    var mouse_pressed = false;
    var interpolated_tick_delta: f64 = 0.0;
    while (true) {
        zeng.start_of_frame();
        if (zeng.quit) break;
        defer zeng.end_of_frame(&res);
        commands.time += res.get(time_res).delta_time;
        zeng.net.recieve_net_messages(main_socket, &res, &commands);

        if (zeng.get_mouse_button(.left) and zeng.get_key(.k)) {
            zeng.lock_cursor_to_window(ctx.hwnd);
            zeng.hide_cursor();
        }

        const world_matrix_q = fet.fresh_query(.{zeng.world_matrix});
        const children_q = fet.fresh_query(.{children});
        const local_matrix_q = fet.fresh_query(.{local_matrix});

        // SERVER SIDE
        for (server_tick_offset_events.array.items, server_tick_offset_events.addresses.?.items) |server_tick_offset_event, _| {
            const rtt = synced_time - server_tick_offset_event.client_time;
            draw_rtt = rtt;

            const time_offset = server_tick_offset_event.server_time - (server_tick_offset_event.client_time + synced_time) * 0.5;
            draw_time_alignment = time_offset;

            if (@abs(time_offset) > 0.7) {
                std.debug.print("time jump\n", .{});
                synced_time += time_offset;
            } else {
                timescale = 1.0 + (time_offset / 50.0);
            }
            if (!warm) {
                buffer_time = 0.4;
            }
            warm = true;
        }
        server_tick_offset_events.clear();
        for (input_events.array.items, input_events.addresses.?.items) |input_event, sockaddr| {
            const info = client_map.getPtr(sockaddr) orelse break;

            for (input_event.arr) |_input_event| {
                if (_input_event.tick >= tick) {
                    info.input_buffer.set(_input_event.tick, _input_event);
                } else {
                    // std.debug.print("late input tick\n", .{});
                }
            }
        }
        input_events.clear();
        for (player_join_events.array.items, player_join_events.addresses.?.items) |plyr_event, sockaddr| {
            std.debug.print("Player connected: {}\n", .{plyr_event});

            const _model_root = zeng.loader.auto_import(&ctx, &world, &res, "assets/gltf/", "static_test", skin_shader, static_shader, uv_checker_tex);
            world.add(player{ .velocity = zeng.vec3.ZERO, .ground_normal = zeng.vec3.UP, .grounded = false, .animation_controller = undefined, .camera = undefined }, _model_root);
            world.add(rpc.input_message{ .tick = 0, .jump = false, .move_vect = zeng.vec2{}, .rot_x = 0.0, .rot_y = 0.0 }, _model_root);
            const _E = find_component_of_type(&world, _model_root, .skinned_mesh, fet.fresh_query(.{children})).?;
            world.add(animation_component{ .time = 0.0, .current_animation = 0 }, world.id_get(_E, .skinned_mesh).?.skeleton);
            world.id_get(_model_root, .player).?.animation_controller = world.id_get(_E, .skinned_mesh).?.skeleton;
            world.id_get(_model_root, .world_matrix).?.* = zeng.mat_tran(world.id_get(_model_root, .world_matrix).?.*, zeng.vec3{ .y = 60.0 });

            world.id_get(_model_root, .player).?.camera = world.spawn(.{zeng.mat_identity});

            top_children.append(_model_root) catch unreachable;
            client_map.put(sockaddr, client_info{ .input_buffer = ring_buffer(rpc.input_message){ .arr = undefined }, .player = _model_root }) catch unreachable;
        }
        player_join_events.clear();
        for (client_tick_events.array.items, client_tick_events.addresses.?.items) |client_tick_event, sockaddr| {
            commands.remote_event(main_socket, sockaddr, rpc.server_tick_offset{ .server_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accum, .client_time = client_tick_event.time }, .unreliable);
        }
        client_tick_events.clear();

        // CLIENT SIDE
        for (snap_events.array.items, snap_events.addresses.?.items) |snap_event, _| {
            const P = world.id_get(model_root, .player).?;
            const temp = P.animation_controller;
            const temp2 = P.camera;
            P.* = snap_event.state;
            P.animation_controller = temp;
            P.camera = temp2;
            world.id_get(model_root, .world_matrix).?.* = snap_event.world_matrix;

            var _tick = snap_event.tick + 1;
            while (_tick < tick) {
                defer _tick += 1;

                var im = client_input_buffer.get(_tick);
                if (im.tick != _tick) {
                    std.debug.print("missing buffered input for tick: {} {} {}\n", .{ im.tick, _tick, tick });
                    break;
                }
                simulate_collision(world.id_get(model_root, .player).?, world.id_get(model_root, .world_matrix).?, &ctx, &spatial_hash_grid, &tri_events, res.get(debug_res));
                simulate_player(world.id_get(model_root, .player).?, &im, world.id_get(model_root, .world_matrix).?, res.get(time_res));
            }
            draw_resims = @intCast(@max(tick - snap_event.tick, 0));

            sync_transforms_children(model_root, world_matrix_q, children_q, local_matrix_q);
        }
        snap_events.clear();
        for (missed_input_events.array.items, missed_input_events.addresses.?.items) |_, _| {
            if (warm and buffer_cooldown <= 0.0) {
                buffer_velocity = 0.04;
                std.debug.print("increasing buffer\n", .{});
                buffer_cooldown = 0.3;
            }
        }
        missed_input_events.clear();
        for (world_update_events.array.items, world_update_events.addresses.?.items) |world_update_event, _| {
            const SI = world.id_get(model_root2, .snapshot_interpolator).?;
            SI.buffer.set(world_update_event.tick, .{ .position = zeng.mat_position(world_update_event.server_player_matrix), .tick = world_update_event.tick });

            const SI2 = world.id_get(cube_root, .snapshot_interpolator).?;
            SI2.buffer.set(world_update_event.tick, .{ .position = world_update_event.cube_pos, .tick = world_update_event.tick });

            const target = @as(f64, @floatFromInt(world_update_event.tick - tick));
            interpolated_tick_delta = target;
        }
        world_update_events.clear();
        if (!is_server) {
            const integer_tick_float: f32 = @floatFromInt(tick);
            const fractional_tick_float: f32 = @floatCast(accum * fixed_rate);
            const _tick_float = integer_tick_float + fractional_tick_float;
            const tick_float = zeng.lerp(@as(f32, @floatCast(synced_time * fixed_rate)), _tick_float, -1.0); // double check this is a good idea - it is technical scalable
            const start_A: isize = @intFromFloat(tick_float);
            const start_B: isize = @intFromFloat(tick_float + 1.0);

            const interp_q = fet.fresh_query(.{ snapshot_interpolator, zeng.world_matrix });
            var interp_it = interp_q.iterator();
            while (interp_it.next()) |curr| {
                const SI, const M = curr;

                var A: isize = start_A;
                while (A > start_A - 50) {
                    if (SI.buffer.get(A).tick == A) break;
                    A -= 1;
                }

                var B: isize = start_B;
                while (B < start_A + 50) {
                    if (SI.buffer.get(B).tick == B) break;
                    B += 1;
                }

                if (SI.buffer.get(A).tick == A and SI.buffer.get(B).tick == B) {
                    const float_A: f32 = @floatFromInt(A);
                    const float_B: f32 = @floatFromInt(B);

                    const t_value = zeng.inv_lerp(float_A, float_B, tick_float);

                    zeng.mat_position_set(M, SI.buffer.get(A).position.lerp(SI.buffer.get(B).position, t_value));
                } else {
                    std.debug.print("interp error\n", .{});
                }
            }
        }

        if (warm) { // network time sync
            buffer_cooldown -= res.get(time_res).delta_time;
            if (buffer_cooldown < -10.0) {
                buffer_velocity = -0.002;
            } else if (buffer_cooldown < 0.0) {
                buffer_velocity = 0.0;
            }
            buffer_time += res.get(time_res).delta_time * buffer_velocity;

            const desired_time = synced_time + buffer_time;
            const my_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accum;
            const offset = desired_time - my_time;
            draw_local_alignment = offset;
            if (@abs(offset) > 0.5) {
                tick += @intFromFloat(offset * fixed_rate);
                std.debug.print("skipping time\n", .{});
            } else {
                sim_timescale = zeng.lerp(sim_timescale, 1.0 + offset / 5.0, 0.2);
            }
        }

        mouse_pressed = mouse_pressed or zeng.get_mouse_button(.left) and !mouse_pressed_last_frame;
        mouse_pressed_last_frame = zeng.get_mouse_button(.left);

        synced_time += res.get(time_res).delta_time * timescale;
        accum += res.get(time_res).delta_time * sim_timescale;
        while (accum >= fixed_delta) {
            accum -= fixed_delta;
            defer tick += 1;
            fet.run_system(fly_system);
            fet.run_system(ray_system);
            fet.run_system(spawn_system);
            fet.run_system(player_collision_system);

            // USE INPUT
            if (is_server) {
                var a = client_map.iterator();
                while (a.next()) |_c| {
                    const cool = _c.value_ptr.input_buffer.get(tick);
                    if (cool.tick == tick) {
                        const ent = _c.value_ptr.player;
                        const input = world.id_get(ent, .input_data).?;
                        input.* = cool;
                    } else {
                        std.debug.print("missed input {}\n", .{tick});
                    }
                    if (_c.value_ptr.input_buffer.get(tick + 10).tick != tick + 10) {
                        commands.remote_event(main_socket, _c.key_ptr.*, rpc.missed_input{}, .unreliable);
                    }
                }
            }
            const __input = world.id_get(model_root, .input_data).?;
            __input.* = rpc.input_message{ .tick = tick, .jump = input_implement.default_jump(), .move_vect = input_implement.default_move_fn(), .rot_x = __input.rot_x, .rot_y = __input.rot_y };
            if (!is_server) {
                client_input_buffer.set(tick, world.id_get(model_root, .input_data).?.*);
                var snd: rpc.input_chunck = undefined;
                var curr: usize = 0;
                while (curr < snd.arr.len) {
                    defer curr += 1;
                    snd.arr[curr] = client_input_buffer.get(tick - @as(isize, @intCast(curr)));
                }
                commands.remote_event(main_socket, server_address, snd, .unreliable);
            }
            if (is_server) {
                const floater_q = fet.fresh_query(.{ floater_component, zeng.world_matrix });
                var floater_it = floater_q.iterator();
                while (floater_it.next()) |curr| {
                    _, const M = curr;
                    zeng.mat_position_set(M, .{ .x = -7.0, .y = 2.0, .z = @mod(@as(f32, @floatFromInt(tick)) * 0.01, 1.0) * 20.0 - 5.0 });
                }
            }

            // SIMULATE PLAYER
            fet.run_system(player_logic_system);
            fet.run_system(follower_system);
            fet.run_system(animation_system);

            // bulk client/server communication stuff
            if (is_server) {
                var it = client_map.iterator();
                while (it.next()) |thing| {
                    const P = world.id_get(thing.value_ptr.player, .player).?.*;
                    const M = world.id_get(thing.value_ptr.player, .world_matrix).?.*;
                    if (@as(usize, @intCast(tick)) % 4 == 0) commands.remote_event(main_socket, thing.key_ptr.*, rpc.state_correction{ .tick = tick, .state = P, .world_matrix = M }, .unreliable);
                }

                it = client_map.iterator();
                while (it.next()) |thing| {
                    if (@as(usize, @intCast(tick)) % 1 == 0) {
                        commands.remote_event(main_socket, thing.key_ptr.*, rpc.world_update{ .cube_pos = zeng.mat_position(world.id_get(cube_root, .world_matrix).?.*), .server_player_matrix = world.id_get(model_root, .world_matrix).?.*, .tick = tick }, .unreliable);
                    }
                }
            } else {
                if (@as(usize, @intCast(tick)) % 60 == 0) {
                    commands.remote_event(main_socket, server_address, rpc.client_tick{ .time = synced_time }, .unreliable);
                }
            }

            if (mouse_pressed) { // shoot logic
                mouse_pressed = false;
                aud.play_sound(gun_shot, .one_shot);

                var enter_t: f32 = undefined;
                var exit_t: f32 = undefined;
                var _error: bool = undefined;

                var it = client_map.iterator();
                while (it.next()) |thing| {
                    const a_coll = phy.collider_info{ .data = undefined, .matrix = world.id_get(thing.value_ptr.player, .world_matrix).?.*, .support = &phy.player_capsule, .tag = .support_based };
                    const b_coll = phy.collider_info{ .data = undefined, .matrix = world.id_get(main_camera, .world_matrix).?.*, .support = &phy.point, .tag = .support_based };

                    const result = phy.shape_cast(a_coll, b_coll, zeng.mat_forward(world.id_get(main_camera, .world_matrix).?.*).neg(), &enter_t, &exit_t, &_error);
                    if (result) aud.play_sound(bell, .one_shot);
                }
            }
        }

        const _input = world.get(model_root, rpc.input_message).?;
        matrix_use_rotations(world.get(main_camera, zeng.world_matrix).?, _input.rot_x, _input.rot_y);

        world.get(pistol_root, zeng.world_matrix).?.* = zeng.mat_tran(world.id_get(main_camera, .world_matrix).?.*, zeng.mat_mult_vec4(world.id_get(main_camera, .world_matrix).?.*, zeng.vec4{ .x = 0.15, .y = -0.15, .z = -0.2 }).to_vec3());
        for (top_children.items) |ch| {
            sync_transforms_children(ch, world_matrix_q, children_q, local_matrix_q);
        }

        const camera_ = world.get(main_camera, zeng.camera).?;
        const camera_matrix_ = world.id_get(main_camera, .world_matrix).?;
        draw_sky(sky_shader, square_vao, square_indices_length, camera_matrix_.*, camera_);

        zeng.render.draw_mesh(cube_mesh, zeng.mat_identity, camera_.projection_matrix, zeng.mat_invert(camera_matrix_.*));
        fet.run_system(render_system);

        var buffer: [64]u8 = undefined;
        zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.4}", .{1.0 / res.get(time_res).delta_time}) catch unreachable, res.get(text_render_res), -0.9, 0.8);
        if (!is_server) {
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{synced_time}) catch unreachable, res.get(text_render_res), -0.9, 0.7);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_time}) catch unreachable, res.get(text_render_res), -0.9, 0.6);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{sim_timescale}) catch unreachable, res.get(text_render_res), -0.9, 0.5);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{buffer_velocity}) catch unreachable, res.get(text_render_res), -0.9, 0.4);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{draw_rtt}) catch unreachable, res.get(text_render_res), -0.9, 0.2);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "resims: {}", .{draw_resims}) catch unreachable, res.get(text_render_res), -0.9, 0.1);
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "interp: {d:.6}", .{interpolated_tick_delta}) catch unreachable, res.get(text_render_res), -0.9, 0.0);

            zeng.render.draw_rect(ctx, res.get(rect_render_res), 0, 400, 100, 2, zeng.render.color.WHITE);
            zeng.render.draw_rect(ctx, res.get(rect_render_res), 0, 400, 2, 60, zeng.render.color.WHITE);
            zeng.render.draw_rect(ctx, res.get(rect_render_res), @as(f32, @floatCast(draw_time_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);

            zeng.render.draw_rect(ctx, res.get(rect_render_res), 500, 400, 100, 2, zeng.render.color.WHITE);
            zeng.render.draw_rect(ctx, res.get(rect_render_res), 500, 400, 2, 60, zeng.render.color.WHITE);
            zeng.render.draw_rect(ctx, res.get(rect_render_res), 500 + @as(f32, @floatCast(draw_local_alignment * 200.0)), 400, 4, 30, zeng.render.color.LIME);
        } else {
            zeng.render.draw_text(std.fmt.bufPrint(buffer[0..], "{d:.6}", .{@as(f64, @floatFromInt(tick)) * fixed_delta + accum}) catch unreachable, res.get(text_render_res), -0.9, 0.7);
        }
        for (str_events.array.items) |str| {
            zeng.render.draw_text(str, res.get(text_render_res), 0, 0);
        }
        str_events.clear();
        for (tri_events.array.items) |tri| {
            zeng.render.debug_draw_triangle(tri, res.get(debug_res).*);
        }
        tri_events.clear();

        var ticker_display_time: f64 = undefined;
        if (is_server) {
            ticker_display_time = @as(f64, @floatFromInt(tick)) * fixed_delta + accum;
        } else {
            ticker_display_time = synced_time;
        }
        zeng.render.draw_rect(ctx, res.get(rect_render_res), @floatCast(@mod(ticker_display_time * 100.0, 100.0) - 400), 400, 10, 10, zeng.render.color.YELLOW);
        zeng.render.draw_rect(ctx, res.get(rect_render_res), 0, 0, 6, 6, zeng.render.color.BLACK);
        zeng.render.draw_rect(ctx, res.get(rect_render_res), 0, 0, 4, 4, zeng.render.color.WHITE);

        commands.process_commands(&world);
        zeng.net.send_net_messages(&commands, res.get(time_res).delta_time);
        _ = zeng.c.SwapBuffers(ctx.hdc);
    }
}

pub fn draw_sky(sky_shader: u32, square_vao: u32, square_indices_length: c_int, camera_matrix: zeng.world_matrix, camera: *zeng.camera) void {
    zeng.gl.useProgram(sky_shader);
    zeng.gl.bindVertexArray(square_vao);
    zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_world_space"), 1, zeng.gl.FALSE, camera_matrix[0..].ptr);
    zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_perspective"), 1, zeng.gl.FALSE, &camera.projection_matrix);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, square_indices_length, zeng.gl.UNSIGNED_INT, null);
    zeng.gl.clear(zeng.gl.DEPTH_BUFFER_BIT);
}

/// Spawns entities random: *std.Random.Xoshiro256,
pub fn spawn_system(sound_list: *sound_res, commands: *zeng.commands, input: *input_res, netres: *networking_res) !void {
    _ = commands; // autofix
    const t_down = zeng.get_key(.t);
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
pub fn fly_system(cam: *main_camera_res, world: *ecs.world, q: *ecs.query(.{ zeng.world_matrix, fly_component })) !void {
    const cam_matrix = world.id_get(cam.id, .world_matrix).?;

    var it = q.iterator();
    while (it.next()) |transform_flyer| {
        const transform, _ = transform_flyer;

        var speed: f32 = 0.2; //@floatCast(time.delta_time * 100.0);
        if (zeng.get_key(.shift)) {
            speed *= 0.2;
        } else {
            speed *= 0.05;
        }
        if (zeng.get_key(.a)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.d)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_right(cam_matrix.*).mult(speed)));
        }
        if (zeng.get_key(.q)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.e)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_up(cam_matrix.*).mult(speed)));
        }
        if (zeng.get_key(.w)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult(-speed)));
        }
        if (zeng.get_key(.s)) {
            zeng.mat_position_set(transform, zeng.mat_position(transform.*).add(zeng.mat_forward(cam_matrix.*).mult(speed)));
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

/// Render all entities with a transform and a mesh
pub fn render_system(world: *ecs.world, cam: *main_camera_res, render_q: *ecs.query(.{ zeng.world_matrix, zeng.mesh }), skinned_q: *ecs.query(.{ zeng.world_matrix, zeng.skinned_mesh })) !void {
    const cam_matrix = world.id_get(cam.id, .world_matrix).?;
    const cam_cam = world.id_get(cam.id, .camera).?;

    const inv_camera_matrix: [16]f32 = zeng.mat_invert(cam_matrix.*);

    var render_iterator = render_q.iterator();
    while (render_iterator.next()) |transform_mesh| {
        const transform, const mesh = transform_mesh;

        zeng.render.draw_mesh(mesh.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix);
    }

    var skinned_iterator = skinned_q.iterator();
    while (skinned_iterator.next()) |transform_skin| {
        const transform, const skin = transform_skin;

        zeng.render.draw_animated_skinned_mesh(world, skin.*, transform.*, cam_cam.projection_matrix, inv_camera_matrix);
    }
}

/// Makes all follower entities follow their target
pub fn follower_system(T: *time_res, follower_q: *ecs.query(.{ follow_component, zeng.world_matrix }), followee_q: *ecs.query(.{ player, zeng.world_matrix })) !void {
    _ = T; // autofix
    var follower_it = follower_q.iterator();
    while (follower_it.next()) |cam_curr| {
        const cam_follower, const cam_transform = cam_curr;

        const target_position = zeng.mat_position(followee_q.get(cam_follower.target, zeng.world_matrix).?.*);

        cam_follower.anchor_point = target_position; //zeng.vec3.lerp(cam_follower.anchor_point, target_position, 5.0 * T.fixed_dt);

        // zeng.mat_position_set(cam_transform, cam_follower.anchor_point.add(zeng.mat_forward(cam_transform.*).mult(2.5)).add(zeng.vec3{ .y = 0.5 }));
        zeng.mat_position_set(cam_transform, cam_follower.anchor_point.add(zeng.vec3{ .y = 0.75 }));
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

pub fn simulate_player(_player: *player, input: *const rpc.input_message, matrix: *zeng.world_matrix, time: *time_res) void {
    const rotated_matrix = _player_matrix_from_rotations(input.rot_x, input.rot_y);

    if (input.jump and _player.grounded) {
        _player.velocity = _player.velocity.add(zeng.vec3{ .y = 6 });
        _player.grounded = false;
        _player.ground_normal = zeng.vec3.UP;
    }

    const acc: f32 = 60.0;
    const basis_right = zeng.mat_right(rotated_matrix).slide(_player.ground_normal).normalized();
    const basis_forward = basis_right.cross(_player.ground_normal);
    var move_vect = basis_right.mult(input.move_vect.x).add(basis_forward.mult(input.move_vect.y));

    var tilt = zeng.vec3.ZERO;
    if (_player.grounded) {
        if (input.move_vect.length() > 0.1) {
            if (_player.velocity.length_sq() > 0.01) {
                const g = move_vect.sub(_player.velocity.normalized()).clamp(1.0);
                const h = g.mult(2.0).add(move_vect).normalized();
                var h_v = h.project(_player.velocity);
                const h_h = h.sub(h_v);
                if (_player.velocity.length() > 3.8 and h_v.dot(_player.velocity) > 0.0) h_v = zeng.vec3.ZERO;
                tilt = h_v.add(h_h);
                _player.velocity = _player.velocity.add(h_v.add(h_h).mult(acc * time.fixed_dt));
            } else {
                _player.velocity = _player.velocity.add(move_vect.mult(acc * time.fixed_dt));
            }
        } else {
            tilt = _player.velocity.neg().clamp(1.0);
            _player.velocity = _player.velocity.add(_player.velocity.neg().clamp(acc * time.fixed_dt));
        }
    } else {
        _player.velocity = _player.velocity.add(zeng.vec3.UP.mult(-9.8 * time.fixed_dt));
        _player.ground_normal = zeng.vec3.UP;
        _player.velocity = _player.velocity.add(move_vect.mult(acc * 0.1 * time.fixed_dt));
        _player.velocity = _player.velocity.slide(zeng.vec3.UP).add(_player.velocity.project(zeng.vec3.UP));
    }
    _player.tilt = _player.tilt.lerp(tilt, 8.0 * time.fixed_dt);
    matrix.* = zeng.mat_tran(matrix.*, _player.velocity.mult(time.fixed_dt));

    if (_player.velocity.slide(zeng.vec3.UP).length() > 0.05) {
        _player.old_velocity = _player.old_velocity.slerp(_player.velocity.slide(zeng.vec3.UP).normalized(), 8 * time.fixed_dt);
    }
    if (_player.old_velocity.slide(zeng.vec3.UP).length() > 0.05) {
        const _up = (zeng.vec3.UP.add(_player.tilt.mult(0.3))).normalized();
        matrix.* = zeng.mat_rebasis(matrix.*, _up.cross(_player.old_velocity.slide(_up)).normalized(), _up, _player.old_velocity.slide(_up).normalized());
    }

    if (zeng.mat_position(matrix.*).y < -30.0) {
        zeng.mat_position_set(matrix, .{ .y = 20.0, .x = -5.0 });
        _player.velocity = zeng.vec3.ZERO;
    }
}

pub fn player_logic_system(time: *time_res, player_q: *ecs.query(.{ player, rpc.input_message, zeng.world_matrix }), animator_q: *ecs.query(.{ zeng.skeleton, animation_component }), ctx: *zeng.engine_context, animations: *animation_res) !void {
    var player_it = player_q.iterator();
    while (player_it.next()) |player_curr| {
        const _player, const input: *rpc.input_message, const matrix = player_curr;

        simulate_player(_player, input, matrix, time);

        const anim = animator_q.get(_player.animation_controller, animation_component).?;
        const skel = animator_q.get(_player.animation_controller, zeng.skeleton).?;
        const blend = _player.velocity.div(3.0).clamp(1.0).length();

        anim.time += time.fixed_dt / zeng.lerp(animations.items[1].duration, animations.items[4].duration, blend);
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

pub fn animation_system(T: *time_res, animator_q: *ecs.query(.{ zeng.skeleton, animation_component }), ctx: *zeng.engine_context, animations: *animation_res) !void {
    var it = animator_q.iterator();
    while (it.next()) |it_curr| {
        const skel, const anim = it_curr;

        const blend = 0.0; // p.velocity.div(3.0).clamp(1.0).length();

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

pub fn ray_system(world: *ecs.world, cam: *main_camera_res, colliders: *std.ArrayList(phy.collider_info), cube_tracker: *cube_tracker_res, str_events: *events([]const u8)) !void {
    _ = cube_tracker; // autofix
    _ = str_events;
    const cam_matrix = world.id_get(cam.id, .world_matrix).?;
    const ro = zeng.mat_position(cam_matrix.*);
    const rd = zeng.mat_forward(cam_matrix.*).neg();
    var min_result: phy.raycast_result = undefined;
    const hit = phy.ray_cast(ro, rd, colliders.items, &min_result);
    if (hit and zeng.get_key(.f)) {
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
pub fn find_component_of_type(world: *ecs.world, parent: ecs.entity_id, comptime name: component_name, q_children: *ecs.query(.{children})) ?ecs.entity_id {
    if (world.id_get(parent, name) != null) return parent;

    const childrens = world.id_get(parent, .children) orelse return null;
    for (childrens.items) |child| {
        const res = find_component_of_type(world, child, name, q_children);
        if (res != null) return res;
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
pub fn get_animation_pose_with_weight(animation: *zeng.loader.Animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
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
pub fn add_animation_pose_with_weight(animation: *zeng.loader.Animation, time_norm: f32, pose: zeng.skeleton_pose, weight: f32) void {
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
pub fn matrix_use_rotations(matrix: *zeng.world_matrix, x: f64, y: f64) void {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    matrix.* = zeng.mat_tran(zeng.mat_mult(rot_mat_hor, rot_mat_vert), zeng.mat_position(matrix.*));
}
pub fn _player_matrix_from_rotations(x: f64, y: f64) zeng.world_matrix {
    const rot_mat_hor = zeng.mat_axis_angle(zeng.vec3.UP, @floatCast(x * -0.003));
    const rot_mat_vert = zeng.mat_axis_angle(zeng.vec3.RIGHT, @floatCast(y * -0.003));
    return zeng.mat_mult(rot_mat_hor, rot_mat_vert);
}

// MISSING FEATURES:
// implement reliable messages
// add more flexible custom serialization functions to allow for dynamic data
// use dynamic data to send variable input messages
// use dynamic data to send snapshots for every replicated entity

// OPTIONAL REFACTORING:
// overhaul resources to use enums for names
// redo events, especially net events - add functionality to make them not so boiler-platey if possible

// NECESSARY IMPROVEMENTS
// make audio system more robust and accurate
// robust text rendering
// better rendering - lights
// better material system
