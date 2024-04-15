const std = @import("std");
const Engine = @import("engine.zig");
const ecs = @import("ecs.zig");

pub const SineMover = struct {};

pub const TypeRegistry = [_]type{
    Engine.Camera,
    Engine.Mesh,
    Engine.Name,
    Engine.Transform,
    SineMover,
};
pub const ECS = ecs.CompileECS(TypeRegistry);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    var world: ECS.ECSWorld = undefined;
    world.InitEmptyWorld(allocator);
    defer world.Destroy() catch unreachable;

    // initialization
    var gd: Engine.GlobalData = undefined;
    gd.allocator = allocator;
    gd.window_width = 800;
    gd.window_height = 500;
    try Engine.InitializeStuff(&gd);
    defer Engine.DeinitializeStuff(&gd);

    // import shader from files and create a program stored in shader_program_GPU
    gd.shader_program_GPU = undefined;
    {
        // get code from vertex shader file as a string
        const vert_shader_code = Engine.GetBytesFromFile("assets/shaders/basic.shader", allocator);
        defer allocator.free(vert_shader_code);

        // take vertex shader code > send to GPU > compile
        const vertex_shader_GPU: u32 = Engine.gl.createShader(Engine.gl.VERTEX_SHADER);
        defer Engine.gl.deleteShader(vertex_shader_GPU);
        Engine.gl.shaderSource(vertex_shader_GPU, 1, &vert_shader_code.ptr, &@intCast(vert_shader_code.len));
        Engine.gl.compileShader(vertex_shader_GPU);

        // check for opengl compilation errors
        {
            var infoLog: [512]u8 = undefined;
            Engine.gl.getShaderInfoLog(vertex_shader_GPU, 512, null, &infoLog);
            std.debug.print("{s}\n", .{infoLog});
        }

        // get code from fragment shader file as a string
        var frag_shader_code = Engine.GetBytesFromFile("assets/shaders/fragment.shader", allocator);
        defer allocator.free(frag_shader_code);

        // take fragment shader code > send to GPU > compile
        const frag_shader_GPU: u32 = Engine.gl.createShader(Engine.gl.FRAGMENT_SHADER);
        defer Engine.gl.deleteShader(frag_shader_GPU);
        Engine.gl.shaderSource(frag_shader_GPU, 1, &frag_shader_code.ptr, &@intCast(frag_shader_code.len));
        Engine.gl.compileShader(frag_shader_GPU);

        // check for opengl compilation errors
        {
            var infoLog: [512]u8 = undefined;
            Engine.gl.getShaderInfoLog(frag_shader_GPU, 512, null, &infoLog);
            std.debug.print("{s}\n", .{infoLog});
        }

        // create shader program > attach vertex + fragment shaders
        gd.shader_program_GPU = Engine.gl.createProgram();
        Engine.gl.attachShader(gd.shader_program_GPU, vertex_shader_GPU);
        Engine.gl.attachShader(gd.shader_program_GPU, frag_shader_GPU);
        Engine.gl.linkProgram(gd.shader_program_GPU);
    }

    // create a texture from file
    Engine.c.stbi_set_flip_vertically_on_load(1);
    gd.texture_GPU = undefined;
    {
        // load image texture via stb_image library
        var width: i32 = undefined;
        var height: i32 = undefined;
        var num_channels: i32 = undefined;
        const image_data: [*c]u8 = Engine.c.stbi_load("assets/images/uv_checker.png", &width, &height, &num_channels, 3);
        defer Engine.c.stbi_image_free(image_data);

        // create texture location > bind > set filtering > put the array data into the texture > generate mips
        Engine.gl.genTextures(1, &gd.texture_GPU);
        Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, gd.texture_GPU);
        defer Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, 0);
        Engine.gl.texParameteri(Engine.gl.TEXTURE_2D, Engine.gl.TEXTURE_MIN_FILTER, Engine.gl.NEAREST);
        Engine.gl.texParameteri(Engine.gl.TEXTURE_2D, Engine.gl.TEXTURE_MAG_FILTER, Engine.gl.NEAREST);
        //Engine.gl.pixelStorei(Engine.gl.UNPACK_ALIGNMENT, 1);
        Engine.gl.texImage2D(Engine.gl.TEXTURE_2D, 0, Engine.gl.RGB, width, height, 0, Engine.gl.RGB, Engine.gl.UNSIGNED_BYTE, image_data);
        Engine.gl.generateMipmap(Engine.gl.TEXTURE_2D);
    }

    // make mouse invisible and locked / enable raw mouse motion / enable depth buffer & backface culling
    gd.active_window.setInputModeCursor(Engine.glfw.Window.InputModeCursor.disabled);
    gd.active_window.setInputModeRawMouseMotion(true);
    gd.elapsed_time = 0.0;

    // create camera entity / use it as main camera / initialize it thru window resize callback
    const cam = try world.SpawnEntity(.{
        @as(Engine.Transform, Engine.identity()),
        Engine.Camera{ .projection_matrix = undefined },
    });
    try world.Get(cam, .{ &gd.active_camera_matrix, &gd.active_camera });
    Engine.OnWindowResize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    // import the scene
    _ = Engine.ImportModelAsset2(&world, "assets/blender_files/custom_export.bin", allocator, gd.shader_program_GPU, gd.texture_GPU);

    // repeat until user closes the window
    while (!gd.active_window.shouldClose()) {
        const start_frame_time = std.time.nanoTimestamp();
        defer {
            Engine.glfw.pollEvents();
            gd.frame_delta = @as(f64, @floatFromInt(@divTrunc(std.time.nanoTimestamp() - start_frame_time, 1000))) / 1000000.0;
        }

        // SYSTEMS
        {
            // first system
            gd.cur_pos = gd.active_window.getCursorPos();
            gd.elapsed_time += @floatCast(gd.frame_delta);

            // t button press / import scene
            if (gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press and !gd.t_down_consume) {
                const input_read: []u8 = Engine.GetBytesFromFile("assets/extras/command_input.txt", std.heap.c_allocator);
                defer std.heap.c_allocator.free(input_read);

                var imported = Engine.ImportModelAsset2(&world, "assets/blender_files/simple.bin", gd.allocator, gd.shader_program_GPU, gd.texture_GPU);

                try world.SetComponent(SineMover{}, &imported);
            }
            gd.t_down_consume = gd.active_window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press;

            var query_sine_mover = try ECS.QueryIterator.create(&world, .{ Engine.Transform, SineMover });
            defer query_sine_mover.destroy() catch unreachable;
            while (query_sine_mover.next()) {
                const Ts = try query_sine_mover.field(Engine.Transform);
                for (Ts) |*transform| {
                    transform[14] += @sin(gd.elapsed_time * 3.0) * @as(f32, @floatCast(gd.frame_delta)) * 5.0;
                    transform[12] += @cos(gd.elapsed_time * 5.0) * @as(f32, @floatCast(gd.frame_delta)) * 5.0;
                }
            }

            var query_spectator_movement = try ECS.QueryIterator.create(&world, .{ Engine.Transform, Engine.Camera });
            defer query_spectator_movement.destroy() catch unreachable;
            while (query_spectator_movement.next()) {
                const Ts = try query_spectator_movement.field(Engine.Transform);
                for (Ts) |*transform| {
                    var speed: f32 = @floatCast(gd.frame_delta * 100.0);
                    if (gd.active_window.getKey(Engine.glfw.Key.left_shift) == Engine.glfw.Action.press) {
                        speed *= 0.2;
                    } else {
                        speed *= 0.05;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.a) == Engine.glfw.Action.press) {
                        transform[12] -= transform[0] * speed;
                        transform[13] -= transform[1] * speed;
                        transform[14] -= transform[2] * speed;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.d) == Engine.glfw.Action.press) {
                        transform[12] += transform[0] * speed;
                        transform[13] += transform[1] * speed;
                        transform[14] += transform[2] * speed;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.q) == Engine.glfw.Action.press) {
                        transform[12] -= transform[4] * speed;
                        transform[13] -= transform[5] * speed;
                        transform[14] -= transform[6] * speed;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.e) == Engine.glfw.Action.press) {
                        transform[12] += transform[4] * speed;
                        transform[13] += transform[5] * speed;
                        transform[14] += transform[6] * speed;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.w) == Engine.glfw.Action.press) {
                        transform[12] -= transform[8] * speed;
                        transform[13] -= transform[9] * speed;
                        transform[14] -= transform[10] * speed;
                    }
                    if (gd.active_window.getKey(Engine.glfw.Key.s) == Engine.glfw.Action.press) {
                        transform[12] += transform[8] * speed;
                        transform[13] += transform[9] * speed;
                        transform[14] += transform[10] * speed;
                    }
                }
            }

            var query_mouse_look = try ECS.QueryIterator.create(&world, .{ Engine.Transform, Engine.Camera });
            defer query_mouse_look.destroy() catch unreachable;
            while (query_mouse_look.next()) {
                const Ts = try query_mouse_look.field(Engine.Transform);
                for (Ts) |*transform| {
                    var pos: [3]f32 = undefined;
                    pos[0] = transform[12];
                    pos[1] = transform[13];
                    pos[2] = transform[14];
                    const rot_mat_hor = Engine.axis_angle_to_matrix(.{ .x = 0, .y = 1, .z = 0 }, @floatCast(gd.cur_pos.xpos * -0.0015));
                    const rot_mat_vert = Engine.axis_angle_to_matrix(.{ .x = 1, .y = 0, .z = 0 }, @floatCast(gd.cur_pos.ypos * -0.0015));
                    transform.* = Engine.multiply_matrices(rot_mat_hor, rot_mat_vert);
                    transform[12] = pos[0];
                    transform[13] = pos[1];
                    transform[14] = pos[2];
                }
            }

            // render system
            Engine.gl.clearColor(0.2, 0.3, 0.3, 1.0);
            Engine.gl.clear(Engine.gl.COLOR_BUFFER_BIT | Engine.gl.DEPTH_BUFFER_BIT);
            const inv_camera_matrix: [16]f32 = Engine.InvertMatrix(gd.active_camera_matrix.*);

            var query = try ECS.QueryIterator.create(&world, .{ Engine.Mesh, Engine.Transform });
            defer query.destroy() catch unreachable;
            while (query.next()) {
                const Ms = try query.field(Engine.Mesh);
                const Ts = try query.field(Engine.Transform);
                for (Ms, Ts) |mesh, transform| {
                    Engine.DrawMesh2(mesh, transform, gd.active_camera.projection_matrix, inv_camera_matrix);
                }
            }
            gd.active_window.swapBuffers();
        }
    }

    for (gd.entity_slice) |entity| {
        allocator.free((entity.name orelse continue));
    }
}
