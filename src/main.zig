const std = @import("std");
const glfw = @import("mach-glfw");
const gl = @import("gl");
const c = @cImport({
    @cInclude("stb_image.h");
});
const Engine = struct {
    usingnamespace @import("loader.zig");
    usingnamespace @import("data_types.zig");
    const systems = @import("systems.zig");
};

pub const DataBase = struct {
    time: f32 = 0.0,
    active_window: *glfw.Window,
    active_camera_matrix: *[16]f32,
    active_camera: *Engine.Camera,
    cur_pos: glfw.Window.CursorPos,
    entity_slice: []Engine.Entity,
    frame_delta: f64 = 0.001,
    frozen: bool = false,
    shader_program_GPU: u32,
    texture_GPU: u32,
};

// Add/Remove components
pub fn SetComponent(entity: *Engine.Entity, component: []u8, comptime value: bool) !void {
    if (std.mem.eql(u8, component, "sine_mover")) {
        entity.component_flags.sine_mover = value;
    } else if (std.mem.eql(u8, component, "ghost")) {
        entity.component_flags.ghost = value;
    } else {
        return error.InvalidParam;
    }
}
pub fn AddComponent(entity: *Engine.Entity, component: []u8) !void {
    return SetComponent(entity, component, true);
}
pub fn RemoveComponent(entity: *Engine.Entity, component: []u8) !void {
    return SetComponent(entity, component, false);
}

// GLFW + GL
fn glGetProcAddress(p: glfw.GLProc, proc: [:0]const u8) ?gl.FunctionPointer {
    _ = p;
    return glfw.getProcAddress(proc);
}
fn errorCallback(error_code: glfw.ErrorCode, description: [:0]const u8) void {
    std.log.err("glfw error: {}: {s}\n", .{ error_code, description });
}
fn glLogError() !void {
    var err: gl.GLenum = gl.getError();
    // const hasErrored = err != gl.NO_ERROR;
    while (err != gl.NO_ERROR) {
        const errorString = switch (err) {
            gl.INVALID_ENUM => "INVALID_ENUM",
            gl.INVALID_VALUE => "INVALID_VALUE",
            gl.INVALID_OPERATION => "INVALID_OPERATION",
            gl.OUT_OF_MEMORY => "OUT_OF_MEMORY",
            gl.INVALID_FRAMEBUFFER_OPERATION => "INVALID_FRAMEBUFFER_OPERATION",
            else => "unknown error",
        };

        std.log.err("Found OpenGL error: {s}", .{errorString});

        err = gl.getError();
    }
}
pub fn OnWindowResize(window: glfw.Window, width: i32, height: i32) void {
    std.debug.print("Window has been resized\n", .{});
    gl.viewport(0, 0, width, height);
    if (window.getUserPointer([16]f32)) |ptr| {
        ptr.* = Engine.perspective_projection_matrix(1.3, @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height)), 0.01, 100.0);
    }
}

// Commands
pub fn RunCommand(db: *DataBase, input_read: []const u8) void {
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
                Engine.ImportModelAsset(parsed[1], std.heap.c_allocator, db.shader_program_GPU, db.texture_GPU, &db.entity_slice);
            } else {
                std.debug.print("No path specified", .{});
            }
        }
        if (std.mem.eql(u8, parsed[0], "freeze") and parsed.len == 1) { // pauses everything in the game except a spectator camera
            std.debug.print("FREEZE!\n", .{});
            db.frozen = !db.frozen;
        } else if (std.mem.eql(u8, parsed[0], "add")) { // adds a component to an entity
            if (parsed.len >= 3) {
                if (std.fmt.parseInt(u32, parsed[1], 10)) |parsed_int| {
                    const index: u32 = parsed_int;
                    std.debug.print("modifying index: {any}\n", .{index});
                    AddComponent(&db.entity_slice[index], parsed[2]) catch {
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
                    RemoveComponent(&db.entity_slice[index], parsed[2]) catch {
                        std.debug.print("Could not remove component: '{s}'", .{parsed[2]});
                    };
                } else |_| {
                    std.debug.print("Invalid numerical field in command\n", .{});
                }
            } else {
                std.debug.print("Too few arguments for 'remove' command\n", .{});
            }
        } else { // unrecognized command
            std.debug.print("Command not recognized\n", .{});
        }
    }
}

// Main
pub fn main() !void {
    glfw.setErrorCallback(errorCallback);
    if (!glfw.init(.{})) {
        std.log.err("failed to initialize GLFW: {?s}", .{glfw.getErrorString()});
        std.process.exit(1);
    }
    defer glfw.terminate();

    // create our window
    var window = glfw.Window.create(1200, 900, "colsens game window!", null, null, .{
        .opengl_profile = .opengl_core_profile,
        .context_version_major = 4,
        .context_version_minor = 0,
    }) orelse {
        std.log.err("failed to create GLFW window: {?s}", .{glfw.getErrorString()});
        std.process.exit(1);
    };
    defer window.destroy();

    // allow correct window resizing > other stuff
    window.setSizeCallback(OnWindowResize);
    glfw.makeContextCurrent(window);
    const proc: glfw.GLProc = undefined;
    try gl.load(proc, glGetProcAddress);

    // tell opengl the correct viewport size in pixels
    gl.viewport(0, 0, 1200, 900);
    c.stbi_set_flip_vertically_on_load(1);

    // initialize shared data
    var db: DataBase = undefined;

    // import shader from files and create a program stored in shader_program_GPU
    db.shader_program_GPU = gl.createProgram();
    {
        // get code from vertex shader file as a string (c style).
        const vert_shader_code = Engine.GetBytesFromFile("assets/shaders/basic.shader", std.heap.c_allocator);
        defer std.heap.c_allocator.free(vert_shader_code);

        // take vertex shader code > send to GPU > compile
        const vertex_shader_GPU: u32 = gl.createShader(gl.VERTEX_SHADER);
        defer gl.deleteShader(vertex_shader_GPU);
        gl.shaderSource(vertex_shader_GPU, 1, &vert_shader_code.ptr, &@intCast(vert_shader_code.len));
        gl.compileShader(vertex_shader_GPU);

        // get any shader compile errors from OpenGL
        {
            var infoLog: [512]u8 = undefined;
            gl.getShaderInfoLog(vertex_shader_GPU, 512, null, &infoLog);
            std.debug.print("{s}\n", .{infoLog});
        }

        // get code from fragment shader file as a string (c style).
        var frag_shader_code = Engine.GetBytesFromFile("assets/shaders/fragment.shader", std.heap.c_allocator);
        defer std.heap.c_allocator.free(frag_shader_code);

        // take fragment shader code > send to GPU > compile
        const frag_shader_GPU: u32 = gl.createShader(gl.FRAGMENT_SHADER);
        defer gl.deleteShader(frag_shader_GPU);
        gl.shaderSource(frag_shader_GPU, 1, &frag_shader_code.ptr, &@intCast(frag_shader_code.len));
        gl.compileShader(frag_shader_GPU);

        // get any shader compile errors from OpenGL
        {
            var infoLog: [512]u8 = undefined;
            gl.getShaderInfoLog(frag_shader_GPU, 512, null, &infoLog);
            std.debug.print("{s}\n", .{infoLog});
        }

        // create shader program > attach vertex + fragment shaders
        gl.attachShader(db.shader_program_GPU, vertex_shader_GPU);
        gl.attachShader(db.shader_program_GPU, frag_shader_GPU);
        gl.linkProgram(db.shader_program_GPU);
    }

    // create a texture from file
    db.texture_GPU = undefined;
    {
        // load image texture via stb_image library
        var width: i32 = undefined;
        var height: i32 = undefined;
        var num_channels: i32 = undefined;
        const image_data: [*c]u8 = c.stbi_load("assets/images/uv_checker.png", &width, &height, &num_channels, 3);
        defer c.stbi_image_free(image_data);

        // create texture location > bind > set filtering > put the array data into the texture > generate mips
        gl.genTextures(1, &db.texture_GPU);
        gl.bindTexture(gl.TEXTURE_2D, db.texture_GPU);
        defer gl.bindTexture(gl.TEXTURE_2D, 0);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        //gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGB, width, height, 0, gl.RGB, gl.UNSIGNED_BYTE, image_data);
        gl.generateMipmap(gl.TEXTURE_2D);
    }

    // make mouse invisible and locked > enable raw mouse motion > enable depth buffer & backface culling
    window.setInputModeCursor(glfw.Window.InputModeCursor.disabled);
    window.setInputModeRawMouseMotion(true);
    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.CULL_FACE);

    // initialize state
    var t_pressed_last_frame: bool = false;
    db.active_window = &window;
    db.time = 0.0;

    // create the array of meshes to be rendered each frame
    var entity_array: [32]Engine.Entity = undefined;
    db.entity_slice = entity_array[0..0];

    // create camera entity
    Engine.CreateEntity(&db.entity_slice, Engine.Entity{ .vao_gpu = 0, .indices_length = 0, .material = undefined, .world_matrix = Engine.identity(), .camera = Engine.Camera{ .projection_matrix = Engine.perspective_projection_matrix(1.3, 12.0 / 9.0, 0.01, 100.0) }, .component_flags = Engine.ComponentFlags{ .camera = true, .ghost = true } });
    // make this camera the one to be used for rendering
    db.active_camera_matrix = &db.entity_slice[db.entity_slice.len - 1].world_matrix;
    db.active_camera = &db.entity_slice[db.entity_slice.len - 1].camera;
    window.setUserPointer(&db.active_camera.projection_matrix);

    // create system schedule
    var systems_array: [32]*const fn (*DataBase) void = undefined;
    var systems_slice: []*const fn (*DataBase) void = systems_array[0..0];
    Engine.systems.AddSystem(Engine.systems.SYSTEM_Input, &systems_slice);
    Engine.systems.AddSystem(Engine.systems.SYSTEM_Constant, &systems_slice);
    Engine.systems.AddSystem(Engine.systems.SYSTEM_SineMover, &systems_slice);
    Engine.systems.AddSystem(Engine.systems.SYSTEM_Ghost, &systems_slice);
    Engine.systems.AddSystem(Engine.systems.SYSTEM_CameraControls, &systems_slice);

    RunCommand(&db, "import assets/blender_files/custom_export.bin");

    // repeat until user closes the window
    while (!window.shouldClose()) {
        // get start time
        const start_frame_time = std.time.nanoTimestamp();

        // run command when t is pressed
        if (window.getKey(glfw.Key.t) == glfw.Action.press and !t_pressed_last_frame) {
            const input_read: []u8 = Engine.GetBytesFromFile("assets/extras/command_input.txt", std.heap.c_allocator);
            defer std.heap.c_allocator.free(input_read);
            RunCommand(&db, input_read);
        }
        t_pressed_last_frame = window.getKey(glfw.Key.t) == glfw.Action.press;

        // call all systems
        for (systems_slice) |system| {
            if (!db.frozen or system == &Engine.systems.SYSTEM_CameraControls or system == &Engine.systems.SYSTEM_Ghost or system == &Engine.systems.SYSTEM_Input) {
                system(&db);
            }
        }
        Engine.systems.SYSTEM_MeshDrawer(&db); // render should be last

        // poll events
        glfw.pollEvents();

        // calculate frame delta
        db.frame_delta = @as(f64, @floatFromInt(@divTrunc(std.time.nanoTimestamp() - start_frame_time, 100))) / 10000000.0;
    }
}
