const std = @import("std");
const Engine = @import("engine.zig");

// GLFW + GL
fn glGetProcAddress(p: Engine.glfw.GLProc, proc: [:0]const u8) ?Engine.gl.FunctionPointer {
    _ = p;
    return Engine.glfw.getProcAddress(proc);
}
fn errorCallback(error_code: Engine.glfw.ErrorCode, description: [:0]const u8) void {
    std.log.err("Engine.glfw error: {}: {s}\n", .{ error_code, description });
}
fn glLogError() !void {
    var err: Engine.gl.GLenum = Engine.gl.getError();
    // const hasErrored = err != Engine.gl.NO_ERROR;
    while (err != Engine.gl.NO_ERROR) {
        const errorString = switch (err) {
            Engine.gl.INVALID_ENUM => "INVALID_ENUM",
            Engine.gl.INVALID_VALUE => "INVALID_VALUE",
            Engine.gl.INVALID_OPERATION => "INVALID_OPERATION",
            Engine.gl.OUT_OF_MEMORY => "OUT_OF_MEMORY",
            Engine.gl.INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
            else => "unknown error",
        };

        std.log.err("Found OpenGL error: {s}", .{errorString});

        err = Engine.gl.getError();
    }
}
pub fn OnWindowResize(window: Engine.glfw.Window, width: i32, height: i32) void {
    std.debug.print("Window has been resized\n", .{});
    Engine.gl.viewport(0, 0, width, height);
    if (window.getUserPointer(Engine.GlobalData)) |gd| {
        gd.window_width = @intCast(width);
        gd.window_height = @intCast(height);
        gd.active_camera.projection_matrix = Engine.perspective_projection_matrix(1.3, @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height)), 0.01, 100.0);
    }
}

// Commands
pub fn RunCommand(gd: *Engine.GlobalData, input_read: []const u8) void {
    const separated: [][]u8 = Engine.SeparateText(input_read, ';');
    defer {
        for (separated) |string| {
            std.heap.c_allocator.free(string);
        }
        std.heap.c_allocator.free(separated);
    }
    for (separated) |sub_command| {
        const parsed: [][]u8 = Engine.SeparateText(sub_command, ' ');
        defer {
            for (parsed) |string| {
                std.heap.c_allocator.free(string);
            }
            std.heap.c_allocator.free(parsed);
        }
        if (std.mem.eql(u8, parsed[0], "import")) {
            if (parsed.len >= 2) {
                const ents = Engine.ImportModelAsset(parsed[1], std.heap.c_allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);
                defer std.heap.c_allocator.free(ents);
            } else {
                std.debug.print("No path specified", .{});
            }
        } else if (std.mem.eql(u8, parsed[0], "freeze") and parsed.len == 1) { // pauses everything in the game except a spectator camera
            std.debug.print("FREEZE!\n", .{});
            gd.frozen = !gd.frozen;
        } else if (std.mem.eql(u8, parsed[0], "add")) { // adds a component to an entity
            if (parsed.len >= 3) {
                if (std.fmt.parseInt(u32, parsed[1], 10)) |parsed_int| {
                    const index: u32 = parsed_int;
                    std.debug.print("modifying index: {any}\n", .{index});
                    Engine.AddComponent(&gd.entity_slice[index], parsed[2]) catch {
                        std.debug.print("Could not add component: '{s}'", .{parsed[2]});
                    };
                } else |_| {
                    std.debug.print("Invalid numerical field in command\n", .{});
                }
            } else {
                std.debug.print("Too few arguments for 'add' command\n", .{});
            }
        } else if (std.mem.eql(u8, parsed[0], "remove")) { // removes a component from an entity
            if (parsed.len >= 3) {
                if (std.fmt.parseInt(u32, parsed[1], 10)) |parsed_int| {
                    const index: u32 = parsed_int;
                    std.debug.print("modifying index: {any}\n", .{index});
                    Engine.RemoveComponent(&gd.entity_slice[index], parsed[2]) catch {
                        std.debug.print("Could not remove component: '{s}'", .{parsed[2]});
                    };
                } else |_| {
                    std.debug.print("Invalid numerical field in command\n", .{});
                }
            } else {
                std.debug.print("Too few arguments for 'remove' command\n", .{});
            }
        } else { // unrecognized command
            std.debug.print("Command not recognized: '{s}'\n", .{sub_command});
        }
    }
}

// Main
pub fn main() !void {
    // set glfw error callback
    Engine.glfw.setErrorCallback(errorCallback);
    if (!Engine.glfw.init(.{})) {
        std.log.err("failed to initialize GLFW: {?s}", .{Engine.glfw.getErrorString()});
        std.process.exit(1);
    }
    defer Engine.glfw.terminate();

    // initialize shared data
    var gd: Engine.GlobalData = undefined;
    gd.window_width = 800;
    gd.window_height = 500;

    // create our window
    var window = Engine.glfw.Window.create(gd.window_width, gd.window_height, "colsens game window!", null, null, .{
        .opengl_profile = .opengl_core_profile,
        .context_version_major = 4,
        .context_version_minor = 0,
    }) orelse {
        std.log.err("failed to create GLFW window: {?s}", .{Engine.glfw.getErrorString()});
        std.process.exit(1);
    };
    defer window.destroy();
    window.setSizeCallback(OnWindowResize);
    window.setUserPointer(&gd);

    // init glfw and opengl
    Engine.glfw.makeContextCurrent(window);
    const proc: Engine.glfw.GLProc = undefined;
    try Engine.gl.load(proc, glGetProcAddress);

    // tell opengl the correct viewport size in pixels
    Engine.gl.viewport(0, 0, @intCast(gd.window_width), @intCast(gd.window_height));
    Engine.c.stbi_set_flip_vertically_on_load(1);

    // import shader from files and create a program stored in shader_program_GPU
    gd.shader_program_GPU = undefined;
    {
        // get code from vertex shader file as a string
        const vert_shader_code = Engine.GetBytesFromFile("assets/shaders/basic.shader", std.heap.c_allocator);
        defer std.heap.c_allocator.free(vert_shader_code);

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
        var frag_shader_code = Engine.GetBytesFromFile("assets/shaders/fragment.shader", std.heap.c_allocator);
        defer std.heap.c_allocator.free(frag_shader_code);

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

    // make mouse invisible and locked > enable raw mouse motion > enable depth buffer & backface culling
    window.setInputModeCursor(Engine.glfw.Window.InputModeCursor.disabled);
    window.setInputModeRawMouseMotion(true);
    Engine.gl.enable(Engine.gl.DEPTH_TEST);
    Engine.gl.enable(Engine.gl.CULL_FACE);

    // initialize state
    var t_pressed_last_frame: bool = false;
    gd.active_window = &window;
    gd.elapsed_time = 0.0;

    // create the array of meshes to be rendered each frame
    var entity_array: [32]Engine.Entity = undefined;
    gd.entity_slice = entity_array[0..0];

    // create camera entity
    Engine.CreateEntity(&gd.entity_slice, Engine.Entity{ .mesh = .{ .vao_gpu = 0, .indices_length = 0, .material = undefined }, .world_matrix = Engine.identity(), .camera = Engine.Camera{ .projection_matrix = undefined }, .component_flags = Engine.ComponentFlags{ .camera = true, .ghost = true } });
    // make this camera the one to be used for rendering
    gd.active_camera_matrix = &gd.entity_slice[gd.entity_slice.len - 1].world_matrix;
    gd.active_camera = &gd.entity_slice[gd.entity_slice.len - 1].camera;
    // run window resize to initialize stuff
    OnWindowResize(window, @intCast(gd.window_width), @intCast(gd.window_height));

    // create system schedule
    var systems_array: [32]*const fn (*Engine.GlobalData) void = undefined;
    var systems_slice: []*const fn (*Engine.GlobalData) void = systems_array[0..0];
    Engine.AddSystem(Engine.SYSTEM_Input, &systems_slice);
    Engine.AddSystem(Engine.SYSTEM_Constant, &systems_slice);
    Engine.AddSystem(Engine.SYSTEM_SineMover, &systems_slice);
    Engine.AddSystem(Engine.SYSTEM_Ghost, &systems_slice);
    Engine.AddSystem(Engine.SYSTEM_CameraControls, &systems_slice);
    Engine.AddSystem(Engine.SYSTEM_MeshDrawer, &systems_slice);

    // run a command to import the scene
    _ = Engine.ImportModelAsset("assets/blender_files/custom_export.bin", std.heap.c_allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);

    // repeat until user closes the window
    while (!window.shouldClose()) {
        // get start time
        const start_frame_time = std.time.nanoTimestamp();

        // run command when t is pressed
        if (window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press and !t_pressed_last_frame) {
            const input_read: []u8 = Engine.GetBytesFromFile("assets/extras/command_input.txt", std.heap.c_allocator);
            defer std.heap.c_allocator.free(input_read);

            RunCommand(&gd, input_read);

            // _ = Engine.ImportModelAsset("assets/blender_files/simple.bin", std.heap.c_allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);

            // for (gd.entity_slice) |*entity| {
            //     if (std.mem.eql(u8, entity.name, "circ")) {
            //         entity.world_matrix[13] += 10.0;
            //         entity.component_flags.sine_mover = true;
            //     }
            // }
        }
        t_pressed_last_frame = window.getKey(Engine.glfw.Key.t) == Engine.glfw.Action.press;

        // call all systems
        for (systems_slice) |system| {
            if (!gd.frozen or system == &Engine.SYSTEM_CameraControls or system == &Engine.SYSTEM_Ghost or system == &Engine.SYSTEM_Input) {
                system(&gd);
            }
        }

        // poll events
        Engine.glfw.pollEvents();

        // calculate frame delta
        gd.frame_delta = @as(f64, @floatFromInt(@divTrunc(std.time.nanoTimestamp() - start_frame_time, 1000))) / 1000000.0;
    }
}
