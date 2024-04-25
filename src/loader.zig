const std = @import("std");
const Engine = @import("engine.zig");
const ecs = @import("ecs.zig");
const ECS = @import("main.zig").ECS;

pub fn GetBytesFromFile(filepath: []const u8, allocator: std.mem.Allocator) []u8 {
    // open file from filepath > close after done
    const file = std.fs.cwd().openFile(filepath, .{}) catch unreachable;
    defer file.close();

    // get size of the file (in bytes)
    const stat = file.stat() catch unreachable;

    // read the file and store it into a dynamically allocated array of u8 > return as a slice
    const result = file.reader().readAllAlloc(allocator, stat.size) catch unreachable;
    return result[0..];
}

pub fn SeparateText(text: []const u8, comptime delimiter: u8, allocator: std.mem.Allocator) [][]u8 {
    var ret = allocator.alloc([]u8, 50) catch unreachable;
    var ret_count: u64 = 0;
    var buffer = [_]u8{'x'} ** 1024;
    var offset: u64 = 0;
    var length: u32 = 0;
    for (text, 0..) |char, i| {
        if (char == delimiter) {
            if (length > 0) {
                ret[ret_count] = allocator.dupe(u8, buffer[0..length]) catch unreachable;
                ret_count += 1;
            }
            offset = i + 1;
            length = 0;
        } else {
            buffer[i - offset] = char;
            length += 1;
        }
    }
    if (length > 0) {
        ret[ret_count] = allocator.dupe(u8, buffer[0..length]) catch unreachable;
        ret_count += 1;
    }

    return ret[0..ret_count];
}

pub fn SpawnModels(world: *ECS.ECSWorld, filepath: anytype, allocator: std.mem.Allocator, shader_program_GPU: u32, texture_GPU: u32) ecs.EntityDataLocation {
    var ret: ecs.EntityDataLocation = undefined;

    // open file from filepath > close after done
    const file = std.fs.cwd().openFile(filepath, .{}) catch unreachable;
    defer file.close();
    // get size of the file (in bytes)
    const stat = file.stat() catch unreachable;
    // read the file and store it into a dynamically allocated array of u8
    var data_bytes = file.reader().readAllAlloc(allocator, stat.size) catch unreachable;
    defer allocator.free(data_bytes);
    // std.debug.print("{}, size: {}\n", .{ @TypeOf(data_bytes), data_bytes.len });
    var curr_byte: u32 = 0;
    var index: u8 = 0;
    while (curr_byte < data_bytes.len) {
        defer index += 1;

        // create vao mesh
        var VAO: u32 = undefined;
        Engine.gl.genVertexArrays(1, &VAO);
        Engine.gl.bindVertexArray(VAO);
        defer {
            Engine.gl.bindVertexArray(0);
            Engine.gl.bindBuffer(Engine.gl.ELEMENT_ARRAY_BUFFER, 0);
            Engine.gl.bindBuffer(Engine.gl.ARRAY_BUFFER, 0);
        }

        // read current place as a u32 > increment current byte by size of this u32 (4 bytes)
        const size_name: u32 = @as(*u32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4]))).*;
        const padded_size: u32 = @divTrunc(size_name + 3, 4) * 4; // (len(obj.name) + 3) // 4 * 4
        curr_byte = curr_byte + 4;
        // std.debug.print("size of name: {}\n", .{size_name});

        // read in the name (there may be padding so that an alignment of 4 is maintained)
        // const name_data = data_bytes[curr_byte .. curr_byte + padded_size];
        curr_byte = curr_byte + padded_size;
        // std.debug.print("'{s}' :\n", .{name_data});

        // read current place as a u32 > increment current byte by size of this u32 (4 bytes)
        var sizea: u32 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&sizea))[0..4], data_bytes[curr_byte .. curr_byte + 4]);

        curr_byte = curr_byte + 4;
        // std.debug.print("sizea of curr: {}\n", .{sizea});

        // read a bunch of floats (amount is 'sizea') which represents the model's vertex data
        const mesh_vertex_data = data_bytes[curr_byte .. curr_byte + sizea];
        curr_byte = curr_byte + sizea;

        // read current place as a u32 > increment current byte by size of this u32 (4 bytes)
        const sizeb: u32 = @as(*u32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4]))).*;
        curr_byte = curr_byte + 4;
        // std.debug.print("sizeb of curr: {}\n", .{sizeb});

        // read a bunch of integers (amount is 'sizea') which represents the model's triangle indexes
        const mesh_indices_data = data_bytes[curr_byte .. curr_byte + sizeb];
        curr_byte = curr_byte + sizeb;

        // read the default position of this mesh node
        const transform_position = @as(*[3]f32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4 * 3])));
        curr_byte = curr_byte + 4 * 3;

        // read the default rotation quaternion of this mesh node
        const transform_rotation_data = @as(*[4]f32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4 * 4])));
        curr_byte = curr_byte + 4 * 4;
        const transform_rotation = Engine.Quat{ .x = transform_rotation_data[0], .y = transform_rotation_data[1], .z = transform_rotation_data[2], .w = transform_rotation_data[3] };

        // create vertex buffer object (holds vertex data) > bind it > store the data from vertices array
        var VBO: u32 = undefined;
        Engine.gl.genBuffers(1, &VBO);
        Engine.gl.bindBuffer(Engine.gl.ARRAY_BUFFER, VBO);
        defer Engine.gl.bindBuffer(Engine.gl.ARRAY_BUFFER, 0);
        Engine.gl.bufferData(Engine.gl.ARRAY_BUFFER, sizea, @ptrCast(mesh_vertex_data), Engine.gl.STATIC_DRAW);

        // create element buffer object (indices array) > bind it > store the data from the indices array
        var EBO: u32 = undefined;
        Engine.gl.genBuffers(1, &EBO);
        Engine.gl.bindBuffer(Engine.gl.ELEMENT_ARRAY_BUFFER, EBO);
        Engine.gl.bufferData(Engine.gl.ELEMENT_ARRAY_BUFFER, sizeb, @ptrCast(mesh_indices_data), Engine.gl.STATIC_DRAW);

        // data layout is : position.xyz, normal.xyz, uv.xy, repeat
        Engine.gl.vertexAttribPointer(0, 3, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(0)); // position
        Engine.gl.vertexAttribPointer(1, 3, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32))); // normal
        Engine.gl.vertexAttribPointer(2, 2, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32))); // uv
        Engine.gl.enableVertexAttribArray(0);
        Engine.gl.enableVertexAttribArray(1);
        Engine.gl.enableVertexAttribArray(2);

        // create a transformation matrix using the position + rotation read from the file
        var transform: [16]f32 = Engine.identity();
        transform = Engine.multiply_matrices(Engine.QuatToMatrix(transform_rotation), transform);
        transform[12..15].* = transform_position.*;

        ret = world.SpawnEntity(.{
            Engine.Mesh{
                .vao_gpu = VAO,
                .indices_length = @divTrunc(@as(i32, @intCast(sizeb)), 4),
                .material = Engine.Material{ .shader_program_GPU = shader_program_GPU, .texture_GPU = texture_GPU },
            },
            transform,
        }) catch unreachable;
    }
    return ret;
}

pub fn LoadShader(allocator: std.mem.Allocator, vertex_path: anytype, fragment_path: anytype) u32 {
    var ret: u32 = undefined;

    // get code from vertex shader file as a string
    const vert_shader_code = Engine.GetBytesFromFile(vertex_path, allocator);
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
    var frag_shader_code = Engine.GetBytesFromFile(fragment_path, allocator);
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
    ret = Engine.gl.createProgram();
    Engine.gl.attachShader(ret, vertex_shader_GPU);
    Engine.gl.attachShader(ret, frag_shader_GPU);
    Engine.gl.linkProgram(ret);

    return ret;
}

pub fn LoadTexture(path: anytype) u32 {
    var ret: u32 = undefined;

    // load image texture via stb_image library
    var width: i32 = undefined;
    var height: i32 = undefined;
    var num_channels: i32 = undefined;
    const image_data: [*c]u8 = Engine.c.stbi_load(path, &width, &height, &num_channels, 3);
    defer Engine.c.stbi_image_free(image_data);

    // create texture location > bind > set filtering > put the array data into the texture > generate mips
    Engine.gl.genTextures(1, &ret);
    Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, ret);
    defer Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, 0);
    Engine.gl.texParameteri(Engine.gl.TEXTURE_2D, Engine.gl.TEXTURE_MIN_FILTER, Engine.gl.NEAREST);
    Engine.gl.texParameteri(Engine.gl.TEXTURE_2D, Engine.gl.TEXTURE_MAG_FILTER, Engine.gl.NEAREST);
    //Engine.gl.pixelStorei(Engine.gl.UNPACK_ALIGNMENT, 1);
    Engine.gl.texImage2D(Engine.gl.TEXTURE_2D, 0, Engine.gl.RGB, width, height, 0, Engine.gl.RGB, Engine.gl.UNSIGNED_BYTE, image_data);
    Engine.gl.generateMipmap(Engine.gl.TEXTURE_2D);

    return ret;
}
