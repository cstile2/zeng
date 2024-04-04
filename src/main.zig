const std = @import("std");
const Engine = @import("engine.zig");
const ECS = @import("ecs.zig");

pub const Sine_mover = struct {
    x: u32,
};
pub const Velocity = struct {
    x: f32 = 1.1,
    y: f32 = 4.4,
    z: f32 = 9.9,
};

pub fn main() !void {
    std.debug.print("\n{}\n", .{comptime ECS.ComponentHash(Sine_mover)});

    var world: ECS.ECSWorld = undefined;
    world.Init(std.heap.c_allocator);

    var edl: ECS.EntityDataLocation = undefined;
    var edl2: ECS.EntityDataLocation = undefined;
    if (world.AddEntity(.{ Sine_mover{ .x = 143 }, Velocity{} }, std.heap.c_allocator)) |new_guy| {
        edl = new_guy;
    }
    if (world.AddEntity(.{ Sine_mover{ .x = 143 }, Velocity{ .x = -10.0, .y = 1.2, .z = 99.9 } }, std.heap.c_allocator)) |new_guy| {
        edl2 = new_guy;
    }

    if (world.GetComponent(Sine_mover, edl)) |comp| {
        std.debug.print("GOT: {}\n", .{comp});
    }
    if (world.GetComponent(Velocity, edl)) |comp| {
        std.debug.print("GOT: {}\n", .{comp});
    }
    if (world.GetComponent(Sine_mover, edl2)) |comp| {
        std.debug.print("GOT: {}\n", .{comp});
    }
    if (world.GetComponent(Velocity, edl2)) |comp| {
        std.debug.print("GOT: {}\n", .{comp});
    }

    // var AS: ECS.ArchetypeTable = undefined;
    // AS.Init(64);
    // AS.AddComponentType(std.heap.c_allocator, Sine_mover);
    // AS.AddComponentType(std.heap.c_allocator, Velocity);

    // AS.AddEntity(.{
    //     Sine_mover{ .x = 143 },
    //     Velocity{},
    // });

    // const E: ECS.EntityDataLocation = .{ .table_index = 0, .archetype_hash = comptime ECS.ComponentHash(Sine_mover) & ECS.ComponentHash(Velocity) };
    // _ = E; // autofix

    // std.debug.print("got value: {}\n", .{(AS.GetComponent(Velocity, 0) catch unreachable).*});
    // std.debug.print("got value: {}\n", .{(AS.GetComponent(Sine_mover, 0) catch unreachable).*});

    // initialization
    var gd: Engine.GlobalData = undefined;
    gd.window_width = 800;
    gd.window_height = 500;
    try Engine.InitializeStuff(&gd);
    defer Engine.DeinitializeStuff(&gd);

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
    Engine.CreateEntity(&gd.entity_slice, Engine.Entity{ .mesh = .{ .vao_gpu = 0, .indices_length = 0, .material = undefined }, .transform = Engine.identity(), .camera = Engine.Camera{ .projection_matrix = undefined }, .component_flags = Engine.ComponentFlags{ .camera = true, .ghost = true } });
    gd.active_camera_matrix = &gd.entity_slice[gd.entity_slice.len - 1].transform;
    gd.active_camera = &gd.entity_slice[gd.entity_slice.len - 1].camera;
    Engine.OnWindowResize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    // run a command to import the scene
    _ = Engine.ImportModelAsset("assets/blender_files/custom_export.bin", std.heap.c_allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);

    // repeat until user closes the window
    while (!gd.active_window.shouldClose()) {
        const start_frame_time = std.time.nanoTimestamp();
        defer {
            Engine.glfw.pollEvents();
            gd.frame_delta = @as(f64, @floatFromInt(@divTrunc(std.time.nanoTimestamp() - start_frame_time, 1000))) / 1000000.0;
        }

        Engine.BigUpdate(&gd);
    }
}
