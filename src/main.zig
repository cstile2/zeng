const std = @import("std");
const Engine = @import("engine.zig");

// fn Mockup() void {
//     var iter = ECS.iterator(.{ Sine_mover, Velocity });

//     // expose table disjointness - implementation specific
//     // seems fastest
//     while (iter.next()) {
//         const S_: []Sine_mover = iter.field(Sine_mover);
//         const V_: []Velocity = iter.field(Velocity);
//         for (S_, V_) |*s, *v| {
//             v += s;
//             s += v;
//         }
//     }

//     // go per entity - get copy of the data and auto send the mutations back
//     // seems to be the slowest - copies must be made twice
//     while (iter.next()) {
//         var s: Sine_mover = iter.get_component(Sine_mover);
//         defer iter.set_component(s);
//         var v: Velocity = iter.get_component(Velocity);
//         defer iter.set_component(v);

//         v += s;
//         s += v;
//     }

//     // go per entity and get components by pointer - could modify to have copies
//     // seems optimizable (unlikely?)
//     while (iter.next()) {
//         const s: *Sine_mover = iter.get_ptr(Sine_mover);
//         const v: *Velocity = iter.get_ptr(Velocity);

//         v.* += s.*;
//         s.* += v.*;
//     }
// }

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

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
    Engine.CreateEntity(&gd.entity_slice, Engine.Entity{ .mesh = .{ .vao_gpu = 0, .indices_length = 0, .material = undefined }, .name = null, .transform = Engine.identity(), .camera = Engine.Camera{ .projection_matrix = undefined }, .component_flags = Engine.ComponentFlags{ .camera = true, .ghost = true } });
    gd.active_camera_matrix = &gd.entity_slice[gd.entity_slice.len - 1].transform;
    gd.active_camera = &gd.entity_slice[gd.entity_slice.len - 1].camera;
    Engine.OnWindowResize(gd.active_window, @intCast(gd.window_width), @intCast(gd.window_height));

    // import the scene
    const list = Engine.ImportModelAsset("assets/blender_files/custom_export.bin", allocator, gd.shader_program_GPU, gd.texture_GPU, &gd.entity_slice);
    allocator.free(list);

    // repeat until user closes the window
    while (!gd.active_window.shouldClose()) {
        const start_frame_time = std.time.nanoTimestamp();
        defer {
            Engine.glfw.pollEvents();
            gd.frame_delta = @as(f64, @floatFromInt(@divTrunc(std.time.nanoTimestamp() - start_frame_time, 1000))) / 1000000.0;
        }

        Engine.BigUpdate(&gd);
    }

    for (gd.entity_slice) |entity| {
        allocator.free((entity.name orelse continue));
    }
}

