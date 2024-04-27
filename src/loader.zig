const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const ECS = @import("main.zig").ECS;

// TODO: add gltf loader (if it's not too complex)

pub fn get_file_bytes(filepath: []const u8, allocator: std.mem.Allocator) []u8 {
    // open file from filepath > close after done
    const file = std.fs.cwd().openFile(filepath, .{}) catch unreachable;
    defer file.close();

    // get size of the file (in bytes)
    const stat = file.stat() catch unreachable;

    // read the file and store it into a dynamically allocated array of u8 > return as a slice
    const result = file.reader().readAllAlloc(allocator, stat.size) catch unreachable;
    return result[0..];
}

pub fn separate_text(text: []const u8, comptime delimiter: u8, allocator: std.mem.Allocator) [][]u8 {
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

pub fn spawn_models(world: *ECS.ECSWorld, filepath: anytype, allocator: std.mem.Allocator, shader_program_GPU: u32, texture_GPU: u32) ecs.EntityDataLocation {
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
        zeng.gl.genVertexArrays(1, &VAO);
        zeng.gl.bindVertexArray(VAO);
        defer {
            zeng.gl.bindVertexArray(0);
            zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, 0);
            zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, 0);
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
        const transform_rotation = zeng.Quat{ .x = transform_rotation_data[0], .y = transform_rotation_data[1], .z = transform_rotation_data[2], .w = transform_rotation_data[3] };

        // create vertex buffer object (holds vertex data) > bind it > store the data from vertices array
        var VBO: u32 = undefined;
        zeng.gl.genBuffers(1, &VBO);
        zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
        defer zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, 0);
        zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, sizea, @ptrCast(mesh_vertex_data), zeng.gl.STATIC_DRAW);

        // create element buffer object (indices array) > bind it > store the data from the indices array
        var EBO: u32 = undefined;
        zeng.gl.genBuffers(1, &EBO);
        zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
        zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, sizeb, @ptrCast(mesh_indices_data), zeng.gl.STATIC_DRAW);

        // data layout is : position.xyz, normal.xyz, uv.xy, repeat
        zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(0)); // position
        zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32))); // normal
        zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32))); // uv
        zeng.gl.enableVertexAttribArray(0);
        zeng.gl.enableVertexAttribArray(1);
        zeng.gl.enableVertexAttribArray(2);

        // create a transformation matrix using the position + rotation read from the file
        var transform: [16]f32 = zeng.identity_matrix();
        transform = zeng.multiply_matrices(zeng.quaternion_to_matrix(transform_rotation), transform);
        transform[12..15].* = transform_position.*;

        ret = world.spawn(.{
            zeng.Mesh{
                .vao_gpu = VAO,
                .indices_length = @divTrunc(@as(i32, @intCast(sizeb)), 4),
                .material = zeng.Material{ .shader_program_GPU = shader_program_GPU, .texture_GPU = texture_GPU },
            },
            transform,
        }) catch unreachable;
    }
    return ret;
}

pub fn load_shader(allocator: std.mem.Allocator, vertex_path: anytype, fragment_path: anytype) u32 {
    var ret: u32 = undefined;

    // get code from vertex shader file as a string
    const vert_shader_code = zeng.get_file_bytes(vertex_path, allocator);
    defer allocator.free(vert_shader_code);

    // take vertex shader code > send to GPU > compile
    const vertex_shader_GPU: u32 = zeng.gl.createShader(zeng.gl.VERTEX_SHADER);
    defer zeng.gl.deleteShader(vertex_shader_GPU);
    zeng.gl.shaderSource(vertex_shader_GPU, 1, &vert_shader_code.ptr, &@intCast(vert_shader_code.len));
    zeng.gl.compileShader(vertex_shader_GPU);

    // check for opengl compilation errors
    {
        var infoLog: [512]u8 = undefined;
        zeng.gl.getShaderInfoLog(vertex_shader_GPU, 512, null, &infoLog);
        std.debug.print("{s}\n", .{infoLog});
    }

    // get code from fragment shader file as a string
    var frag_shader_code = zeng.get_file_bytes(fragment_path, allocator);
    defer allocator.free(frag_shader_code);

    // take fragment shader code > send to GPU > compile
    const frag_shader_GPU: u32 = zeng.gl.createShader(zeng.gl.FRAGMENT_SHADER);
    defer zeng.gl.deleteShader(frag_shader_GPU);
    zeng.gl.shaderSource(frag_shader_GPU, 1, &frag_shader_code.ptr, &@intCast(frag_shader_code.len));
    zeng.gl.compileShader(frag_shader_GPU);

    // check for opengl compilation errors
    {
        var infoLog: [512]u8 = undefined;
        zeng.gl.getShaderInfoLog(frag_shader_GPU, 512, null, &infoLog);
        std.debug.print("{s}\n", .{infoLog});
    }

    // create shader program > attach vertex + fragment shaders
    ret = zeng.gl.createProgram();
    zeng.gl.attachShader(ret, vertex_shader_GPU);
    zeng.gl.attachShader(ret, frag_shader_GPU);
    zeng.gl.linkProgram(ret);

    return ret;
}

pub fn load_texture(path: anytype) u32 {
    var ret: u32 = undefined;

    // load image texture via stb_image library
    var width: i32 = undefined;
    var height: i32 = undefined;
    var num_channels: i32 = undefined;
    const image_data: [*c]u8 = zeng.c.stbi_load(path, &width, &height, &num_channels, 3);
    defer zeng.c.stbi_image_free(image_data);

    // create texture location > bind > set filtering > put the array data into the texture > generate mips
    zeng.gl.genTextures(1, &ret);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ret);
    defer zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, 0);
    zeng.gl.texParameteri(zeng.gl.TEXTURE_2D, zeng.gl.TEXTURE_MIN_FILTER, zeng.gl.NEAREST);
    zeng.gl.texParameteri(zeng.gl.TEXTURE_2D, zeng.gl.TEXTURE_MAG_FILTER, zeng.gl.NEAREST);
    //Engine.gl.pixelStorei(Engine.gl.UNPACK_ALIGNMENT, 1);
    zeng.gl.texImage2D(zeng.gl.TEXTURE_2D, 0, zeng.gl.RGB, width, height, 0, zeng.gl.RGB, zeng.gl.UNSIGNED_BYTE, image_data);
    zeng.gl.generateMipmap(zeng.gl.TEXTURE_2D);

    return ret;
}

pub fn serialize_to_bytes(payload: anytype, dest_bytes: []u8, dest_curr_byte: *u32) void {
    switch (@typeInfo(@TypeOf(payload))) {
        .Int, .Float, .Bool, .Pointer => {
            @memcpy(dest_bytes[dest_curr_byte.* .. dest_curr_byte.* + @sizeOf(@TypeOf(payload))], std.mem.toBytes(payload)[0..]);
            dest_curr_byte.* += @sizeOf(@TypeOf(payload));
        },
        .Struct => {
            inline for (std.meta.fields(@TypeOf(payload))) |f| {
                serialize_to_bytes(@field(payload, f.name), dest_bytes, dest_curr_byte);
            }
        },
        else => {},
    }
}

pub fn deserialize_from_bytes(T: type, dest_bytes: []u8, src_bytes: []u8, src_curr_byte: *u32, offset: u32) void {
    switch (@typeInfo(T)) {
        .Int, .Float, .Bool, .Pointer => {
            @memcpy(dest_bytes[offset .. offset + @sizeOf(T)], src_bytes[src_curr_byte.* .. src_curr_byte.* + @sizeOf(T)]);
            src_curr_byte.* += @sizeOf(T);
        },
        .Struct => {
            inline for (std.meta.fields(T)) |f| {
                deserialize_from_bytes(f.type, dest_bytes, src_bytes, src_curr_byte, offset + @offsetOf(T, f.name));
            }
        },
        else => {},
    }
}

test "Serialize" {
    const point_to_me: u64 = 10;
    const SineMover = struct {
        offset: f32 = 0.0,
    };
    const CircleCollider = struct {
        radius: f32 = 1.0,
    };
    const G = struct {
        s: SineMover = SineMover{ .offset = 1.1 },
        c: CircleCollider = CircleCollider{ .radius = 9.1 },
        f: f32 = 5.0,
    };
    const Gr = struct {
        ptr: *const u64 = &point_to_me,
        g: G = G{},
        s: SineMover = SineMover{ .offset = 1.2 },
        h: bool = true,
        c: CircleCollider = CircleCollider{ .radius = 9.2 },
        f: f32 = 5.01,
        b: bool = false,
        d: bool = true,
    };

    var b: [2048]u8 = undefined;
    var b_len: u32 = 0;
    zeng.serialize_to_bytes(Gr{}, b[0..], &b_len);

    var g: Gr = undefined;
    var curr_point: u32 = 0;
    zeng.deserialize_from_bytes(Gr, std.mem.asBytes(&g), b[0..b_len], &curr_point, 0);

    try std.testing.expectEqual(Gr{}, g);
}
