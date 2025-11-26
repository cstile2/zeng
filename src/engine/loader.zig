const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const phy = @import("physics.zig");

pub fn get_file_bytes(filepath: []const u8, allocator: std.mem.Allocator) []u8 {
    // open file from filepath > close after done
    const file = std.fs.cwd().openFile(filepath, .{}) catch unreachable;
    defer file.close();

    // get size of the file (in bytes)
    const stat = file.stat() catch unreachable;

    // read the file and store it into a dynamically allocated array of u8 > return as a slice
    const buf = allocator.alloc(u8, stat.size) catch unreachable;
    var reader = file.reader(buf);
    const n = reader.read(buf) catch unreachable;
    return buf[0..n];
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
pub fn load_shader(allocator: std.mem.Allocator, vertex_path: anytype, fragment_path: anytype) u32 {
    var ret: u32 = undefined;

    // get code from vertex shader file as a string
    const vert_shader_code = get_file_bytes(vertex_path, allocator);
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
    var frag_shader_code = zeng.loader.get_file_bytes(fragment_path, allocator);
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
pub fn load_texture(path: anytype, srgb: bool, flip_y: bool) u32 {
    zeng.c.stbi_set_flip_vertically_on_load(@intFromBool(flip_y));

    var ret: u32 = undefined;

    // load image texture via stb_image library
    var width: i32 = undefined;
    var height: i32 = undefined;
    var num_channels: i32 = undefined;
    const image_data: [*c]u8 = zeng.c.stbi_load(@ptrCast(path), &width, &height, &num_channels, 3);
    // std.debug.print("{s} {} {} {}\n", .{ path, width, height, num_channels });
    defer zeng.c.stbi_image_free(image_data);

    // create texture location > bind > set filtering > put the array data into the texture > generate mips
    zeng.gl.genTextures(1, &ret);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ret);

    zeng.gl.texParameteri(zeng.gl.TEXTURE_2D, zeng.gl.TEXTURE_MIN_FILTER, zeng.gl.NEAREST);
    zeng.gl.texParameteri(zeng.gl.TEXTURE_2D, zeng.gl.TEXTURE_MAG_FILTER, zeng.gl.NEAREST);
    //Engine.gl.pixelStorei(Engine.gl.UNPACK_ALIGNMENT, 1);
    zeng.gl.texImage2D(zeng.gl.TEXTURE_2D, 0, if (srgb) zeng.gl.SRGB else zeng.gl.RGB, width, height, 0, zeng.gl.RGB, zeng.gl.UNSIGNED_BYTE, image_data);
    zeng.gl.generateMipmap(zeng.gl.TEXTURE_2D);

    zeng.gl_log_errors();

    return ret;
}

pub fn serialized_size(T: type) usize {
    switch (@typeInfo(T)) {
        .int, .float, .bool, .pointer, .array => {
            return @sizeOf(T);
        },
        .@"struct" => {
            var sum: usize = 0;
            inline for (std.meta.fields(T)) |f| {
                sum += @sizeOf(f.type);
            }
            return sum;
        },
        else => {
            // @compileError("this type cannot be serialized");
            @compileLog(T);
        },
    }
}
pub fn serialize_to_bytes(payload: anytype, dest_bytes: []u8, dest_curr_byte: *u32) void {
    switch (@typeInfo(@TypeOf(payload))) {
        .int, .float, .bool, .pointer, .array => {
            @memcpy(dest_bytes[dest_curr_byte.* .. dest_curr_byte.* + @sizeOf(@TypeOf(payload))], std.mem.toBytes(payload)[0..@sizeOf(@TypeOf(payload))]);
            dest_curr_byte.* += @sizeOf(@TypeOf(payload));
        },
        .@"struct" => {
            inline for (std.meta.fields(@TypeOf(payload))) |f| {
                serialize_to_bytes(@field(payload, f.name), dest_bytes, dest_curr_byte);
            }
        },
        else => {
            // @compileError("this type cannot be serialized");
            @compileLog(@TypeOf(payload));
        },
    }
}
pub fn deserialize_from_bytes(T: type, dest_bytes: [*]u8, src_bytes: []const u8, src_curr_byte: *u32, offset: u32) void {
    switch (@typeInfo(T)) {
        .int, .float, .bool, .pointer, .array => {
            @memcpy(dest_bytes[offset .. offset + @sizeOf(T)], src_bytes[src_curr_byte.* .. src_curr_byte.* + @sizeOf(T)]);
            src_curr_byte.* += @sizeOf(T);
        },
        .@"struct" => {
            inline for (std.meta.fields(T)) |f| {
                deserialize_from_bytes(f.type, dest_bytes, src_bytes, src_curr_byte, offset + @offsetOf(T, f.name));
            }
        },
        else => {
            @compileError("this type cannot be deserialized");
        },
    }
}
pub fn serialize_to_byte_slice(payload: anytype, allocator: std.mem.Allocator) []u8 {
    const s = allocator.alloc(u8, serialized_size(@TypeOf(payload))) catch unreachable;
    var curr: u32 = 0;
    serialize_to_bytes(payload, s, &curr);
    return s;
}

pub fn create_square_mesh() struct { u32, c_int } {
    const vertices = [20]f32{
        1.0, 1.0, 0.0, 1.0, 1.0, // top right
        1.0, -1.0, 0.0, 1.0, 0.0, // bottom right
        -1.0, -1.0, 0.0, 0.0, 0.0, // bottom left
        -1.0, 1.0, 0.0, 0.0, 1.0, // top left
    };
    const indices = [6]c_uint{
        3, 1, 0, // first triangle
        3, 2, 1, // second triangle
    };

    var VBO: c_uint = undefined;
    var VAO: c_uint = undefined;
    var EBO: c_uint = undefined;

    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.genBuffers(1, &EBO);

    // bind the Vertex Array Object first, then bind and set vertex buffer(s), and then configure vertex attributes(s).
    zeng.gl.bindVertexArray(VAO);
    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    // Fill our buffer with the vertex data
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * vertices.len, &vertices, zeng.gl.STATIC_DRAW);
    // copy our index array in an element buffer for OpenGL to use
    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, 6 * @sizeOf(c_uint), &indices, zeng.gl.STATIC_DRAW);

    // Specify and link our vertext attribute description
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 5 * @sizeOf(f32), null);
    zeng.gl.vertexAttribPointer(1, 2, zeng.gl.FLOAT, zeng.gl.FALSE, 5 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32)));

    zeng.gl.enableVertexAttribArray(0);
    zeng.gl.enableVertexAttribArray(1);

    return .{ VAO, indices.len };
}
pub fn create_square_mesh2() struct { u32, c_int } {
    const vertices = [20]f32{
        1.0, -1.0, 0.0, 1.0, 1.0, // top right
        1.0, 0.0, 0.0, 1.0, 0.0, // bottom right
        0.0, 0.0, 0.0, 0.0, 0.0, // bottom left
        0.0, -1.0, 0.0, 0.0, 1.0, // top left
    };
    const indices = [6]c_uint{
        1, 3, 0, // first triangle
        2, 3, 1, // second triangle
    };

    var VBO: c_uint = undefined;
    var VAO: c_uint = undefined;
    var EBO: c_uint = undefined;

    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.genBuffers(1, &EBO);

    // bind the Vertex Array Object first, then bind and set vertex buffer(s), and then configure vertex attributes(s).
    zeng.gl.bindVertexArray(VAO);
    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    // Fill our buffer with the vertex data
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * vertices.len, &vertices, zeng.gl.STATIC_DRAW);
    // copy our index array in an element buffer for OpenGL to use
    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, 6 * @sizeOf(c_uint), &indices, zeng.gl.STATIC_DRAW);

    // Specify and link our vertext attribute description
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 5 * @sizeOf(f32), null);
    zeng.gl.vertexAttribPointer(1, 2, zeng.gl.FLOAT, zeng.gl.FALSE, 5 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32)));

    zeng.gl.enableVertexAttribArray(0);
    zeng.gl.enableVertexAttribArray(1);

    return .{ VAO, indices.len };
}
pub fn create_cube_mesh() struct { u32, c_int } {
    // 8 unique vertices, but for proper UVs and normals, we need 24 (4 per face)
    // Now: positions + uvs only (no normals)
    const vertices = [24]f32{
        // positions        // uvs
        // Front face
        -1.0, -1.0, 1.0,  0.0, 0.0,
        1.0,  -1.0, 1.0,  1.0, 0.0,
        1.0,  1.0,  1.0,  1.0, 1.0,
        -1.0, 1.0,  1.0,  0.0, 1.0,
        // Back face
        -1.0, -1.0, -1.0, 1.0, 0.0,
        1.0,  -1.0, -1.0, 0.0, 0.0,
        1.0,  1.0,  -1.0, 0.0, 1.0,
        -1.0, 1.0,  -1.0, 1.0, 1.0,
        // Left face
        -1.0, -1.0, -1.0, 0.0, 0.0,
        -1.0, -1.0, 1.0,  1.0, 0.0,
        -1.0, 1.0,  1.0,  1.0, 1.0,
        -1.0, 1.0,  -1.0, 0.0, 1.0,
        // Right face
        1.0,  -1.0, 1.0,  0.0, 0.0,
        1.0,  -1.0, -1.0, 1.0, 0.0,
        1.0,  1.0,  -1.0, 1.0, 1.0,
        1.0,  1.0,  1.0,  0.0, 1.0,
        // Top face
        -1.0, 1.0,  1.0,  0.0, 0.0,
        1.0,  1.0,  1.0,  1.0, 0.0,
        1.0,  1.0,  -1.0, 1.0, 1.0,
        -1.0, 1.0,  -1.0, 0.0, 1.0,
        // Bottom face
        -1.0, -1.0, -1.0, 0.0, 0.0,
        1.0,  -1.0, -1.0, 1.0, 0.0,
        1.0,  -1.0, 1.0,  1.0, 1.0,
        -1.0, -1.0, 1.0,  0.0, 1.0,
    };
    const indices = [36]c_uint{
        // Front face
        0,  1,  2,  2,  3,  0,
        // Back face
        4,  5,  6,  6,  7,  4,
        // Left face
        8,  9,  10, 10, 11, 8,
        // Right face
        12, 13, 14, 14, 15, 12,
        // Top face
        16, 17, 18, 18, 19, 16,
        // Bottom face
        20, 21, 22, 22, 23, 20,
    };

    var VBO: c_uint = undefined;
    var VAO: c_uint = undefined;
    var EBO: c_uint = undefined;

    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.genBuffers(1, &EBO);

    zeng.gl.bindVertexArray(VAO);

    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * vertices.len, &vertices, zeng.gl.STATIC_DRAW);

    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @sizeOf(c_uint) * indices.len, &indices, zeng.gl.STATIC_DRAW);

    // position (3), uv (2)
    const stride = 5 * @sizeOf(f32);
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(0));
    zeng.gl.vertexAttribPointer(1, 2, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * @sizeOf(f32)));

    zeng.gl.enableVertexAttribArray(0);
    zeng.gl.enableVertexAttribArray(1);

    return .{ VAO, indices.len };
}
pub fn create_cube_mesh_with_normals() struct { u32, c_int } {
    // Cube with positions, normals, and UVs (36 vertices, 8 floats per vertex: pos(3), normal(3), uv(2))
    const vertices = [288]f32{
        // Front face
        // pos            // normal         // uv
        -1.0, -1.0, 1.0,  0.0,  0.0,  1.0,  0.0, 0.0,
        1.0,  -1.0, 1.0,  0.0,  0.0,  1.0,  1.0, 0.0,
        1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0, 1.0,
        1.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0, 1.0,
        -1.0, 1.0,  1.0,  0.0,  0.0,  1.0,  0.0, 1.0,
        -1.0, -1.0, 1.0,  0.0,  0.0,  1.0,  0.0, 0.0,

        // Back face
        -1.0, -1.0, -1.0, 0.0,  0.0,  -1.0, 1.0, 0.0,
        -1.0, 1.0,  -1.0, 0.0,  0.0,  -1.0, 1.0, 1.0,
        1.0,  1.0,  -1.0, 0.0,  0.0,  -1.0, 0.0, 1.0,
        1.0,  1.0,  -1.0, 0.0,  0.0,  -1.0, 0.0, 1.0,
        1.0,  -1.0, -1.0, 0.0,  0.0,  -1.0, 0.0, 0.0,
        -1.0, -1.0, -1.0, 0.0,  0.0,  -1.0, 1.0, 0.0,

        // Left face
        -1.0, 1.0,  1.0,  -1.0, 0.0,  0.0,  1.0, 1.0,
        -1.0, 1.0,  -1.0, -1.0, 0.0,  0.0,  0.0, 1.0,
        -1.0, -1.0, -1.0, -1.0, 0.0,  0.0,  0.0, 0.0,
        -1.0, -1.0, -1.0, -1.0, 0.0,  0.0,  0.0, 0.0,
        -1.0, -1.0, 1.0,  -1.0, 0.0,  0.0,  1.0, 0.0,
        -1.0, 1.0,  1.0,  -1.0, 0.0,  0.0,  1.0, 1.0,

        // Right face
        1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0, 1.0,
        1.0,  -1.0, -1.0, 1.0,  0.0,  0.0,  1.0, 0.0,
        1.0,  1.0,  -1.0, 1.0,  0.0,  0.0,  1.0, 1.0,
        1.0,  -1.0, -1.0, 1.0,  0.0,  0.0,  1.0, 0.0,
        1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0, 1.0,
        1.0,  -1.0, 1.0,  1.0,  0.0,  0.0,  0.0, 0.0,

        // Top face
        -1.0, 1.0,  -1.0, 0.0,  1.0,  0.0,  0.0, 1.0,
        -1.0, 1.0,  1.0,  0.0,  1.0,  0.0,  0.0, 0.0,
        1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  1.0, 0.0,
        1.0,  1.0,  1.0,  0.0,  1.0,  0.0,  1.0, 0.0,
        1.0,  1.0,  -1.0, 0.0,  1.0,  0.0,  1.0, 1.0,
        -1.0, 1.0,  -1.0, 0.0,  1.0,  0.0,  0.0, 1.0,

        // Bottom face
        -1.0, -1.0, -1.0, 0.0,  -1.0, 0.0,  1.0, 1.0,
        1.0,  -1.0, -1.0, 0.0,  -1.0, 0.0,  0.0, 1.0,
        1.0,  -1.0, 1.0,  0.0,  -1.0, 0.0,  0.0, 0.0,
        1.0,  -1.0, 1.0,  0.0,  -1.0, 0.0,  0.0, 0.0,
        -1.0, -1.0, 1.0,  0.0,  -1.0, 0.0,  1.0, 0.0,
        -1.0, -1.0, -1.0, 0.0,  -1.0, 0.0,  1.0, 1.0,
    };

    // Indices for the cube (6 faces * 2 triangles * 3 vertices = 36 indices)
    const indices = [36]u32{
        0, 1, 2, 3, 4, 5, // Front face
        6, 7, 8, 9, 10, 11, // Back face
        12, 13, 14, 15, 16, 17, // Left face
        18, 19, 20, 21, 22, 23, // Right face
        24, 25, 26, 27, 28, 29, // Top face
        30, 31, 32, 33, 34, 35, // Bottom face
    };

    var VBO: c_uint = undefined;
    var VAO: c_uint = undefined;
    var EBO: c_uint = undefined;

    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.genBuffers(1, &EBO);

    zeng.gl.bindVertexArray(VAO);
    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * vertices.len, &vertices, zeng.gl.STATIC_DRAW);

    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @sizeOf(u32) * indices.len, &indices, zeng.gl.STATIC_DRAW);

    // position
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(0));
    // normal
    zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32)));
    // uv
    zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32)));

    zeng.gl.enableVertexAttribArray(0);
    zeng.gl.enableVertexAttribArray(1);
    zeng.gl.enableVertexAttribArray(2);

    return .{ VAO, indices.len };
}
pub fn create_cube_mesh_collider() struct { [108]f32, [36]u32 } {
    const vertices = [108]f32{
        // Front face
        // pos
        -1.0, -1.0, 1.0,
        1.0,  -1.0, 1.0,
        1.0,  1.0,  1.0,
        1.0,  1.0,  1.0,
        -1.0, 1.0,  1.0,
        -1.0, -1.0, 1.0,

        // Back face
        -1.0, -1.0, -1.0,
        -1.0, 1.0,  -1.0,
        1.0,  1.0,  -1.0,
        1.0,  1.0,  -1.0,
        1.0,  -1.0, -1.0,
        -1.0, -1.0, -1.0,

        // Left face
        -1.0, 1.0,  1.0,
        -1.0, 1.0,  -1.0,
        -1.0, -1.0, -1.0,
        -1.0, -1.0, -1.0,
        -1.0, -1.0, 1.0,
        -1.0, 1.0,  1.0,

        // Right face
        1.0,  1.0,  1.0,
        1.0,  -1.0, -1.0,
        1.0,  1.0,  -1.0,
        1.0,  -1.0, -1.0,
        1.0,  1.0,  1.0,
        1.0,  -1.0, 1.0,

        // Top face
        -1.0, 1.0,  -1.0,
        -1.0, 1.0,  1.0,
        1.0,  1.0,  1.0,
        1.0,  1.0,  1.0,
        1.0,  1.0,  -1.0,
        -1.0, 1.0,  -1.0,

        // Bottom face
        -1.0, -1.0, -1.0,
        1.0,  -1.0, -1.0,
        1.0,  -1.0, 1.0,
        1.0,  -1.0, 1.0,
        -1.0, -1.0, 1.0,
        -1.0, -1.0, -1.0,
    };

    // Indices for the cube (6 faces * 2 triangles * 3 vertices = 36 indices)
    const indices = [36]u32{
        0, 1, 2, 3, 4, 5, // Front face
        6, 7, 8, 9, 10, 11, // Back face
        12, 13, 14, 15, 16, 17, // Left face
        18, 19, 20, 21, 22, 23, // Right face
        24, 25, 26, 27, 28, 29, // Top face
        30, 31, 32, 33, 34, 35, // Bottom face
    };

    return .{ vertices, indices };
}
pub fn create_triangle_mesh() struct { u32, u32 } {
    const vertices = [9]f32{
        0.0,  1.0,  0.0,
        -1.0, -1.0, 0.0,
        1.0,  -1.0, 0.0,
    };
    const indices = [3]u8{ 1, 0, 2 };

    var VBO: c_uint = undefined;
    var VAO: c_uint = undefined;
    var EBO: c_uint = undefined;

    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.genBuffers(1, &EBO);

    // bind the Vertex Array Object first, then bind and set vertex buffer(s), and then configure vertex attributes(s).
    zeng.gl.bindVertexArray(VAO);

    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * 9, &vertices, zeng.gl.STATIC_DRAW);

    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, 3 * @sizeOf(u8), &indices, zeng.gl.STATIC_DRAW);

    // Specify and link our vertex attribute description
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, 3 * @sizeOf(f32), null);

    zeng.gl.enableVertexAttribArray(0);

    return .{ VAO, VBO };
}

pub const gltf = struct {
    pub const string_literal_tok = struct {
        string: []const u8,
    };
    pub const int_literal_tok = struct {
        value: i128,
    };
    pub const float_literal_tok = struct {
        value: f64,
    };
    pub const token_tag = enum {
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_square,
        r_square,

        string_literal,
        int_constant,
        float_constant,
        semicolon,

        colon,
        comma,

        _true,
    };
    pub const token = union(gltf.token_tag) {
        // misc
        l_paren: void,
        r_paren: void,
        l_brace: void,
        r_brace: void,
        l_square: void,
        r_square: void,

        string_literal: string_literal_tok,
        int_constant: int_literal_tok,
        float_constant: float_literal_tok,

        semicolon: void,
        colon: void,
        comma: void,

        _true,
    };
    pub const node_tag = enum {
        string,
        integer,
        float,
        boolean,
        object,
        array,
    };
    pub const node = union(node_tag) {
        string: []const u8,
        integer: i128,
        float: f64,
        boolean: bool,

        object: std.StringHashMap(*gltf.node),
        array: std.ArrayList(*gltf.node),
    };
};
pub fn concat(a: []const u8, b: []const u8, allocator: std.mem.Allocator) []u8 {
    var result = allocator.alloc(u8, a.len + b.len) catch unreachable;
    @memcpy(result[0..a.len], a);
    @memcpy(result[a.len..], b);
    return result;
}
pub fn concat_as_null_terminated(a: []const u8, b: []const u8, allocator: std.mem.Allocator) []u8 {
    var result = allocator.alloc(u8, a.len + b.len + 1) catch unreachable;
    @memcpy(result[0..a.len], a);
    @memcpy(result[a.len .. a.len + b.len], b);
    result[result.len - 1] = 0;
    return result;
}
fn contains(char: u8, string: []const u8) bool {
    for (string) |s_char| {
        if (s_char == char) {
            return true;
        }
    }
    return false;
}
pub fn lexer(bytes: []const u8, tokens: *std.ArrayList(gltf.token), allocator: std.mem.Allocator) !void {
    var char_number: u32 = 0;
    var curr: u64 = 0;
    while (curr < bytes.len) {
        defer curr += 1;
        defer char_number += 1;

        if (bytes[curr] == '\n') {
            char_number = 0;
        } else if (bytes[curr] == ';') {
            try tokens.append(allocator, gltf.token{ .semicolon = void{} });
        } else if (bytes[curr] == ',') {
            try tokens.append(allocator, gltf.token{ .comma = void{} });
        } else if (bytes[curr] == ':') {
            try tokens.append(allocator, gltf.token{ .colon = void{} });
        } else if (bytes[curr] == '{') {
            try tokens.append(allocator, gltf.token{ .l_brace = void{} });
        } else if (bytes[curr] == '}') {
            try tokens.append(allocator, gltf.token{ .r_brace = void{} });
        } else if (bytes[curr] == '(') {
            try tokens.append(allocator, gltf.token{ .l_paren = void{} });
        } else if (bytes[curr] == ')') {
            try tokens.append(allocator, gltf.token{ .r_paren = void{} });
        } else if (bytes[curr] == '[') {
            try tokens.append(allocator, gltf.token{ .l_square = void{} });
        } else if (bytes[curr] == ']') {
            try tokens.append(allocator, gltf.token{ .r_square = void{} });
        } else if (bytes[curr] == '"') {
            const start = curr;
            curr += 1;
            while (curr < bytes.len and bytes[curr] != '"') {
                curr += 1;
            }
            try tokens.append(allocator, gltf.token{ .string_literal = gltf.string_literal_tok{ .string = bytes[start + 1 .. curr] } });
        } else if (contains(bytes[curr], "-0123456789")) {
            if (bytes[curr] == '-') {}
            const start = curr;
            var is_float = false;
            var is_scientific = false;

            curr += 1;
            while (contains(bytes[curr], "0123456789")) {
                curr += 1;
            }
            if (bytes[curr] == '.') {
                is_float = true;
                curr += 1;
                while (contains(bytes[curr], "0123456789")) {
                    curr += 1;
                }
                if (bytes[curr] == 'e') {
                    is_scientific = true;
                    curr += 1;
                    if (bytes[curr] == '-') {
                        curr += 1;
                    }
                }
                while (contains(bytes[curr], "0123456789")) {
                    curr += 1;
                }
            }

            if (is_float) {
                if (!is_scientific) {
                    try tokens.append(allocator, gltf.token{ .float_constant = gltf.float_literal_tok{ .value = std.fmt.parseFloat(f64, bytes[start..curr]) catch unreachable } });
                } else {
                    try tokens.append(allocator, gltf.token{ .float_constant = gltf.float_literal_tok{ .value = 0.0 } });
                }
                // std.debug.print("float value: {}\n", .{tokens.getLast().float_constant.value});
            } else {
                try tokens.append(allocator, gltf.token{ .int_constant = gltf.int_literal_tok{ .value = std.fmt.parseInt(i128, bytes[start..curr], 10) catch unreachable } });
                // std.debug.print("int value: {}\n", .{tokens.getLast().int_constant.value});
            }
            curr -= 1;
        } else if (contains(bytes[curr], "abcdefghijklmnopqrstuvwxyz")) {
            const start = curr;
            curr += 1;
            while (curr < bytes.len and contains(bytes[curr], "abcdefghijklmnopqrstuvwxyz")) {
                curr += 1;
            }
            if (std.mem.eql(u8, bytes[start..curr], "true")) {
                try tokens.append(allocator, gltf.token{ ._true = void{} });
            }
            curr -= 1;
        }
    }
}

const context = struct {
    allocator: std.mem.Allocator,
    tokens: []gltf.token,
    curr: u64,
    temp: u64,

    tabs: u32,
};
fn match(rec: *context, tag: gltf.token_tag) bool {
    if (rec.curr >= rec.tokens.len) {
        return false;
    }
    defer rec.curr += 1;
    if (rec.tokens[rec.curr] == tag) {
        return true;
    }
    return false;
}
fn gltf_object(rec: *context) ?*gltf.node {
    const new = rec.allocator.create(gltf.node) catch unreachable;
    new.* = gltf.node{ .object = undefined };

    if (!match(rec, .l_brace))
        return null;

    const name_list = gltf_name_list(rec);
    if (name_list == null)
        return null;
    new.object = name_list.?;

    if (!match(rec, .r_brace))
        return null;

    return new;
}
fn gltf_array(rec: *context) ?*gltf.node {
    const new = rec.allocator.create(gltf.node) catch unreachable;
    new.* = gltf.node{ .array = undefined };

    if (!match(rec, .l_square))
        return null;

    const nameless_list = gltf_nameless_list(rec);
    if (nameless_list == null)
        return null;
    new.array = nameless_list.?;

    if (!match(rec, .r_square))
        return null;

    return new;
}
fn gltf_thing(rec: *context) ?*gltf.node {
    rec.temp = rec.curr;

    const object = gltf_object(rec);
    if (object != null)
        return object;

    rec.curr = rec.temp;

    const array = gltf_array(rec);
    if (array != null)
        return array;

    rec.curr = rec.temp;

    if (match(rec, .string_literal)) {
        // std.debug.print("string literal: \"{s}\"\n", .{rec.tokens[rec.curr - 1].string_literal.string});
        const new = rec.allocator.create(gltf.node) catch unreachable;
        new.* = gltf.node{ .string = rec.tokens[rec.curr - 1].string_literal.string };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, .int_constant)) {
        // std.debug.print("int constant: \"{}\"\n", .{rec.tokens[rec.curr - 1].int_constant.value});
        const new = rec.allocator.create(gltf.node) catch unreachable;
        new.* = gltf.node{ .integer = rec.tokens[rec.curr - 1].int_constant.value };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, .float_constant)) {
        // std.debug.print("int constant: \"{}\"\n", .{rec.tokens[rec.curr - 1].float_constant.value});
        const new = rec.allocator.create(gltf.node) catch unreachable;
        new.* = gltf.node{ .float = rec.tokens[rec.curr - 1].float_constant.value };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, ._true)) {
        // std.debug.print("boolean true\n", .{});
        const new = rec.allocator.create(gltf.node) catch unreachable;
        new.* = gltf.node{ .boolean = true };
        return new;
    }

    // std.debug.print("OOF!\n", .{});
    return null;
}
fn gltf_name_list(rec: *context) ?std.StringHashMap(*gltf.node) {
    var new: std.StringHashMap(*gltf.node) = std.StringHashMap(*gltf.node).init(rec.allocator);

    while (true) {
        if (!(match(rec, .string_literal) and match(rec, .colon)))
            return null;
        const str = rec.tokens[rec.curr - 2].string_literal.string;

        const thing = gltf_thing(rec);
        if (thing == null) {
            return null;
        } else {
            new.put(str, thing.?) catch unreachable;
        }

        if (!match(rec, .comma)) {
            rec.curr -= 1;
            return new;
        }
    }
}
fn gltf_nameless_list(rec: *context) ?std.ArrayList(*gltf.node) {
    var new: std.ArrayList(*gltf.node) = std.ArrayList(*gltf.node).initCapacity(rec.allocator, 0) catch unreachable;

    while (true) {
        const thing = gltf_thing(rec);
        if (thing == null) {
            return null;
        } else {
            new.append(rec.allocator, thing.?) catch unreachable;
            // new.put(void{}, thing.?) catch unreachable;
        }

        if (!match(rec, .comma)) {
            rec.curr -= 1;
            return new;
        }
    }
}
pub fn gltf_parse(bytes: []u8, allocator: std.mem.Allocator) ?*gltf.node {
    var tokens = std.ArrayList(gltf.token).initCapacity(allocator, 10) catch unreachable;
    defer tokens.deinit(allocator);
    lexer(bytes, &tokens, allocator) catch unreachable;

    var rec: context = .{ .allocator = allocator, .tokens = tokens.items, .curr = 0, .temp = undefined, .tabs = 0 };

    return gltf_object(&rec);
}

const scene_node = union(enum) {
    static_mesh: zeng.mesh,
    skinned_mesh: zeng.skinned_mesh,
    empty: void,
};
const scene_node_w_matrix = struct {
    node: scene_node,
    matrix: [16]f32,
    gltf_id: usize = 0,
};
pub const animation = struct {
    const channel_output_data_tag = enum {
        rotation,
        translation,
        scale,
    };
    pub const channel_output_data = union(channel_output_data_tag) {
        rotation: []zeng.quat,
        translation: []zeng.vec3,
        scale: []zeng.vec3,
    };
    const sampler_data = struct {
        inputs: []f32,
        output: channel_output_data,
    };
    pub const channel = struct {
        target: usize,
        inputs: []f32,
        outputs: channel_output_data,
    };

    channels: []channel,
    duration: f32,
};

fn get_component_type_size(_type: usize) usize {
    return switch (_type) {
        5120 => 1,
        5121 => 1,
        5122 => 2,
        5123 => 2,
        5125 => 4,
        5126 => 4,
        else => 1,
    };
}
fn get_component_type_enum(_type: usize) zeng.gl.GLenum {
    return switch (_type) {
        5120 => zeng.gl.BYTE,
        5121 => zeng.gl.UNSIGNED_BYTE,
        5122 => zeng.gl.SHORT,
        5123 => zeng.gl.UNSIGNED_SHORT,
        5125 => zeng.gl.UNSIGNED_INT,
        5126 => zeng.gl.FLOAT,
        else => unreachable,
    };
}
fn get_offsest_and_length(accessor_index: usize, accessors: gltf.node, bufferviews: gltf.node) struct { usize, usize, usize } {
    const bv_index: usize = @intCast(accessors.array.items[accessor_index].object.get("bufferView").?.integer);
    const offset: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteOffset").?.integer);
    const length: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteLength").?.integer);
    const component_type: usize = @intCast(accessors.array.items[accessor_index].object.get("componentType").?.integer);
    return .{ offset, length, component_type };
}
fn get_offsest_and_length2(accessor_index: usize, accessors: gltf.node, bufferviews: gltf.node) struct { usize, usize, usize, usize } {
    const bv_index: usize = @intCast(accessors.array.items[accessor_index].object.get("bufferView").?.integer);
    const offset: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteOffset").?.integer);
    const length: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteLength").?.integer);
    const buffer: usize = @intCast(bufferviews.array.items[bv_index].object.get("buffer").?.integer);
    const component_type: usize = @intCast(accessors.array.items[accessor_index].object.get("componentType").?.integer);
    return .{ buffer, offset, length, component_type };
}
fn get_float_from_numeric(n: *gltf.node, idx: comptime_int) f32 {
    if (n.array.items[idx].* == .float)
        return @floatCast(n.array.items[idx].float);

    return @floatFromInt(n.array.items[idx].integer);
}
fn get_float_from_numeric_value(n: *gltf.node) f32 {
    if (n.* == .float)
        return @floatCast(n.float);

    return @floatFromInt(n.integer);
}

pub var global_colliders: ?std.ArrayList(zeng.cpu_mesh) = null;
pub var global_matrices: ?std.ArrayList(zeng.world_matrix) = null;

pub fn add_to_group(key: usize, value: ecs.entity_id, group_map: *std.AutoArrayHashMap(usize, ?std.ArrayList(ecs.entity_id)), allocator: std.mem.Allocator) void {
    const ptr = group_map.getOrPut(key) catch unreachable;

    if (!ptr.found_existing) {
        ptr.value_ptr.* = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    }
    ptr.value_ptr.*.?.append(allocator, value) catch unreachable;
}
pub fn deep_copy_skeleton(s: zeng.skeleton, allocator: std.mem.Allocator) zeng.skeleton {
    var ret: zeng.skeleton = undefined;
    // ret.animations = s.animations.clone(allocator) catch unreachable;

    ret.bone_parent_indices = allocator.alloc(isize, s.bone_parent_indices.len) catch unreachable;
    @memcpy(ret.bone_parent_indices, s.bone_parent_indices);

    ret.inverse_bind_matrices = allocator.alloc([16]f32, s.inverse_bind_matrices.len) catch unreachable;
    @memcpy(ret.inverse_bind_matrices, s.inverse_bind_matrices);

    ret.local_bone_matrices = allocator.alloc([16]f32, s.local_bone_matrices.len) catch unreachable;
    @memcpy(ret.local_bone_matrices, s.local_bone_matrices);

    ret.model_bone_matrices = allocator.alloc([16]f32, s.model_bone_matrices.len) catch unreachable;
    @memcpy(ret.model_bone_matrices, s.model_bone_matrices);

    ret.default_bone_translations = allocator.alloc(zeng.vec3, s.model_bone_matrices.len) catch unreachable;
    @memcpy(ret.default_bone_translations, s.default_bone_translations);
    ret.default_bone_rotations = allocator.alloc(zeng.quat, s.model_bone_matrices.len) catch unreachable;
    @memcpy(ret.default_bone_rotations, s.default_bone_rotations);
    ret.default_bone_scales = allocator.alloc(zeng.vec3, s.model_bone_matrices.len) catch unreachable;
    @memcpy(ret.default_bone_scales, s.default_bone_scales);

    return ret;
}
pub fn instantiate_model_hierarchy(mesh_slice: []scene_node_w_matrix, names_map: std.AutoHashMap(usize, []const u8), parent_child_map: std.AutoArrayHashMap(usize, std.ArrayList(usize)), top_level_children: std.AutoHashMap(usize, void), skeleton_slice: []zeng.skeleton, skinmesh_to_skeleton: std.AutoHashMap(usize, usize), world: *ecs.world, allocator: std.mem.Allocator) ecs.entity_id {
    var skeleton_entity_list = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    for (skeleton_slice) |skel| {
        const skeleton_copy_entity = world.spawn(.{deep_copy_skeleton(skel, allocator)});
        skeleton_entity_list.append(allocator, skeleton_copy_entity) catch unreachable;
    }

    var gltf_id_to_entity_id = std.AutoArrayHashMap(usize, ?std.ArrayList(ecs.entity_id)).init(allocator);
    var root_child_list = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;
    for (mesh_slice) |mesh_like| {
        if (mesh_like.node == .skinned_mesh) {
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                zeng.local_matrix{ .transform = mesh_like.matrix },
                blk: {
                    var new = mesh_like.node.skinned_mesh;
                    new.skeleton = skeleton_entity_list.items[skinmesh_to_skeleton.get(mesh_like.gltf_id).?];
                    break :blk new;
                },
            });
            if (names_map.get(mesh_like.gltf_id)) |name| {
                world.add(name, entity_id);
            }
            if (top_level_children.contains(mesh_like.gltf_id)) root_child_list.append(allocator, entity_id) catch unreachable;
            // gltf_id_to_entity_id.put(mesh_like.gltf_id, entity_id) catch unreachable;
            add_to_group(mesh_like.gltf_id, entity_id, &gltf_id_to_entity_id, allocator);
        } else if (mesh_like.node == .static_mesh) {
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                zeng.local_matrix{ .transform = mesh_like.matrix },
                mesh_like.node.static_mesh,
            });
            if (names_map.get(mesh_like.gltf_id)) |name| {
                world.add(name, entity_id);
            }
            if (top_level_children.contains(mesh_like.gltf_id)) root_child_list.append(allocator, entity_id) catch unreachable;
            // gltf_id_to_entity_id.put(mesh_like.gltf_id, entity_id) catch unreachable;
            add_to_group(mesh_like.gltf_id, entity_id, &gltf_id_to_entity_id, allocator);
        } else if (mesh_like.node == .empty) {
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                zeng.local_matrix{ .transform = mesh_like.matrix },
            });
            if (names_map.get(mesh_like.gltf_id)) |name| {
                world.add(name, entity_id);
            }
            if (top_level_children.contains(mesh_like.gltf_id)) root_child_list.append(allocator, entity_id) catch unreachable;
            // gltf_id_to_entity_id.put(mesh_like.gltf_id, entity_id) catch unreachable;
            add_to_group(mesh_like.gltf_id, entity_id, &gltf_id_to_entity_id, allocator);
        }
    }

    for (parent_child_map.keys(), parent_child_map.values()) |parent, children| {
        if (gltf_id_to_entity_id.get(parent)) |_parent_e_ids| {
            if (_parent_e_ids) |parent_e_ids| {
                const parent_e_id = parent_e_ids.items[0];
                var children_slice_component = std.ArrayList(ecs.entity_id).initCapacity(allocator, 0) catch unreachable;

                for (children.items) |child_group| {
                    const child_e_ids = gltf_id_to_entity_id.get(child_group).? orelse unreachable;
                    for (child_e_ids.items) |child_e_id| {
                        children_slice_component.append(allocator, child_e_id) catch unreachable;
                    }
                }

                world.add(zeng.children{ .items = children_slice_component.items }, parent_e_id);
            }
        }
    }

    const model_root = world.spawn(.{
        zeng.mat_identity,
        zeng.children{ .items = root_child_list.items },
    });

    return model_root;
}
pub fn gltf_extract_resources(root_n: ?*gltf.node, buffers: []const []const u8, dependencies_path: []const u8, allocator: std.mem.Allocator, skin_shader_program: u32, static_shader_program: u32, default_texture: u32) struct { []scene_node_w_matrix, std.AutoHashMap(usize, []const u8), []animation, [][]const u8, []zeng.skeleton, std.AutoArrayHashMap(usize, std.ArrayList(usize)), std.AutoHashMap(usize, void), std.AutoHashMap(usize, usize) } {
    var result_top_level_objects = std.AutoHashMap(usize, void).init(allocator);
    for (root_n.?.object.get("scenes").?.array.items) |scene_n| {
        for (scene_n.object.get("nodes").?.array.items) |node_n| {
            result_top_level_objects.put(@intCast(node_n.integer), void{}) catch unreachable;
        }
    }
    var joint_to_skin = std.AutoHashMap(usize, usize).init(allocator);
    var skeleton_space_maps = std.ArrayList(std.AutoHashMap(usize, usize)).initCapacity(allocator, 0) catch unreachable;

    var result_nodes = std.ArrayList(scene_node_w_matrix).initCapacity(allocator, 0) catch unreachable;
    var result_names = std.AutoHashMap(usize, []const u8).init(allocator);
    var result_animations = std.ArrayList(animation).initCapacity(allocator, 0) catch unreachable;
    var result_animation_names = std.ArrayList([]const u8).initCapacity(allocator, 0) catch unreachable;
    var result_skeletons = std.ArrayList(zeng.skeleton).initCapacity(allocator, 0) catch unreachable;
    var result_children_map = std.AutoArrayHashMap(usize, std.ArrayList(usize)).init(allocator);
    var result_skinmesh_to_skeleton = std.AutoHashMap(usize, usize).init(allocator);

    const nodes_n = root_n.?.object.get("nodes").?;
    const accessors_n = root_n.?.object.get("accessors").?;
    const bufferviews_n = root_n.?.object.get("bufferViews").?;

    const _animations_n = root_n.?.object.get("animations");
    const _textures_n = root_n.?.object.get("textures");
    const _images_n = root_n.?.object.get("images");
    const _skins_n = root_n.?.object.get("skins");

    if (_skins_n) |skins_n| {
        var current_skin_num: usize = 0;
        for (skins_n.array.items) |current_skin_n| {
            defer current_skin_num += 1;

            var jointspace_to_nodespace = std.AutoHashMap(usize, usize).init(allocator);

            var bone_counter: usize = 0;
            for (current_skin_n.object.get("joints").?.array.items) |joint_n| {
                defer bone_counter += 1;
                jointspace_to_nodespace.put(@intCast(joint_n.integer), bone_counter) catch unreachable;
                joint_to_skin.put(@intCast(joint_n.integer), current_skin_num) catch unreachable;
            }

            const temp_bone_parent_indices = allocator.alloc(isize, bone_counter) catch unreachable;
            @memset(temp_bone_parent_indices, -1);

            const temp_inverse_bind_matrices = allocator.alloc(zeng.world_matrix, bone_counter) catch unreachable;
            const this_buffer_index, const offset: usize, const length: usize, _ = get_offsest_and_length2(@intCast(current_skin_n.object.get("inverseBindMatrices").?.integer), accessors_n.*, bufferviews_n.*);
            @memcpy(@as([*]u8, @ptrCast(temp_inverse_bind_matrices)), buffers[this_buffer_index][offset .. offset + length]);

            const _default_bone_translations = allocator.alloc(zeng.vec3, temp_bone_parent_indices.len) catch unreachable;
            const _default_bone_rotations = allocator.alloc(zeng.quat, temp_bone_parent_indices.len) catch unreachable;
            const _default_bone_scales = allocator.alloc(zeng.vec3, temp_bone_parent_indices.len) catch unreachable;
            for (_default_bone_scales) |*s| {
                s.* = zeng.vec3.ONE;
            }
            for (_default_bone_rotations) |*r| {
                r.* = zeng.quat.IDENTITY;
            }
            for (_default_bone_translations) |*t| {
                t.* = zeng.vec3.ZERO;
            }
            skeleton_space_maps.append(allocator, jointspace_to_nodespace) catch unreachable;
            result_skeletons.append(allocator, zeng.skeleton{
                .inverse_bind_matrices = temp_inverse_bind_matrices,
                .bone_parent_indices = temp_bone_parent_indices,
                .local_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable,
                .model_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable,
                .default_bone_translations = _default_bone_translations,
                .default_bone_rotations = _default_bone_rotations,
                .default_bone_scales = _default_bone_scales,
            }) catch unreachable;
        }
    }
    if (_animations_n != null) {
        for (_animations_n.?.array.items) |current_animation| {
            var temp_channels = std.ArrayList(animation.channel).initCapacity(allocator, 0) catch unreachable;

            const channels_n = current_animation.object.get("channels");
            const samplers_n = current_animation.object.get("samplers");
            if (channels_n == null or samplers_n == null) unreachable;

            var max_timestamp: f32 = 0.0;
            var owner_skin: usize = 0;
            for (channels_n.?.array.items) |channel| {
                const sampler = samplers_n.?.array.items[@intCast(channel.object.get("sampler").?.integer)];

                const input_accessor_index: usize = @intCast(sampler.object.get("input").?.integer);
                const output_accessor_index: usize = @intCast(sampler.object.get("output").?.integer);
                const input_buffer, const input_offset, const input_length, const input_component_type = get_offsest_and_length2(input_accessor_index, accessors_n.*, bufferviews_n.*);
                const output_buffer, const output_offset, const output_length, const output_component_type = get_offsest_and_length2(output_accessor_index, accessors_n.*, bufferviews_n.*);

                std.debug.assert(get_component_type_enum(input_component_type) == zeng.gl.FLOAT);
                std.debug.assert(get_component_type_enum(output_component_type) == zeng.gl.FLOAT);
                std.debug.assert(input_length > 0);
                std.debug.assert(output_length > 0);

                var target_index: usize = @intCast(channel.object.get("target").?.object.get("node").?.integer);
                owner_skin = joint_to_skin.get(target_index).?;
                target_index = skeleton_space_maps.items[owner_skin].get(target_index).?; // REMAP from gltf.node space to a skin bone

                const temp_inputs: []f32 = allocator.alloc(f32, input_length / 4) catch unreachable;
                @memcpy(@as([*]u8, @ptrCast(temp_inputs)), buffers[input_buffer][input_offset .. input_offset + input_length]);

                const target_path = channel.object.get("target").?.object.get("path").?.string;
                var output_type: animation.channel_output_data_tag = undefined;
                if (std.mem.eql(u8, target_path, "rotation")) {
                    output_type = .rotation;
                } else if (std.mem.eql(u8, target_path, "translation")) {
                    output_type = .translation;
                } else if (std.mem.eql(u8, target_path, "scale")) {
                    output_type = .scale;
                } else unreachable;

                var temp_outputs: animation.channel_output_data = undefined;
                if (output_type == .rotation) {
                    temp_outputs = animation.channel_output_data{ .rotation = allocator.alloc(zeng.quat, output_length / 16) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.rotation)), buffers[output_buffer][output_offset .. output_offset + output_length]);
                } else if (output_type == .translation) {
                    temp_outputs = animation.channel_output_data{ .translation = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.translation)), buffers[output_buffer][output_offset .. output_offset + output_length]);
                } else if (output_type == .scale) {
                    temp_outputs = animation.channel_output_data{ .scale = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.scale)), buffers[output_buffer][output_offset .. output_offset + output_length]);
                } else unreachable;

                for (temp_inputs) |f| {
                    max_timestamp = @max(max_timestamp, f);
                }

                temp_channels.append(allocator, animation.channel{
                    .target = target_index,
                    .inputs = temp_inputs,
                    .outputs = temp_outputs,
                }) catch unreachable;
            }

            result_animations.append(allocator, animation{ .channels = temp_channels.items, .duration = max_timestamp }) catch unreachable;
            result_animation_names.append(allocator, current_animation.object.get("name").?.string) catch unreachable;
            // result_skeletons.items[owner_skin].animations.append(allocator, result_animations.items.len - 1) catch unreachable;
        }
    }

    var current_node_index: usize = 0;
    for (nodes_n.array.items) |current_node_n| {
        defer current_node_index += 1;

        const children = current_node_n.object.get("children");
        if (children != null) {
            var entry = std.ArrayList(usize).initCapacity(allocator, 0) catch unreachable;
            children_blk: for (children.?.array.items) |child| {
                // test if any skeleton contains BOTH the child and the parent bone - otherwise add children to the hierarchy
                for (skeleton_space_maps.items, 0..) |skin, s| {
                    if (skin.contains(current_node_index) and skin.contains(@intCast(child.integer))) { // this is an armature connection - don't add it to the global children hierarchy - add it inside this skeleton ONLY
                        result_skeletons.items[s].bone_parent_indices[skin.get(@intCast(child.integer)).?] = @intCast(skin.get(current_node_index).?);
                        continue :children_blk;
                    }
                }
                entry.append(allocator, @intCast(child.integer)) catch unreachable;
            }

            if (entry.items.len > 0) {
                result_children_map.put(current_node_index, entry) catch unreachable; // assign a set of children to this node in output
            } else entry.deinit(allocator);
        }

        const mesh_index_n = current_node_n.object.get("mesh");
        const skin_index_n = current_node_n.object.get("skin");
        if (mesh_index_n != null) { // skinned mesh

            var translation: zeng.vec3 = zeng.vec3.ZERO;
            var scale: zeng.vec3 = zeng.vec3.ONE;
            var rotation: zeng.quat = zeng.quat.IDENTITY;

            if (current_node_n.object.get("translation")) |_translation| {
                translation.x = get_float_from_numeric(_translation, 0);
                translation.y = get_float_from_numeric(_translation, 1);
                translation.z = get_float_from_numeric(_translation, 2);
            }
            if (current_node_n.object.get("rotation")) |_rotation| {
                rotation.x = get_float_from_numeric(_rotation, 0);
                rotation.y = get_float_from_numeric(_rotation, 1);
                rotation.z = get_float_from_numeric(_rotation, 2);
                rotation.w = get_float_from_numeric(_rotation, 3);
            }
            if (current_node_n.object.get("scale")) |_scale| {
                scale.x = get_float_from_numeric(_scale, 0);
                scale.y = get_float_from_numeric(_scale, 1);
                scale.z = get_float_from_numeric(_scale, 2);
            }
            const mat = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotation), zeng.mat_scal(zeng.mat_identity, scale)), translation);

            const mesh_n = root_n.?.object.get("meshes").?.array.items[@intCast(mesh_index_n.?.integer)];
            for (mesh_n.object.get("primitives").?.array.items) |primitive_n| {
                var base_color_of_material: ?zeng.vec3 = null;
                var metallic_of_material: f32 = 0.0;
                var roughness_of_material: f32 = 0.5;
                var base_color_texture_gpu: u32 = default_texture;
                const attributes_n = primitive_n.object.get("attributes").?;
                if (primitive_n.object.get("material")) |material_index_n| {
                    const material_index: usize = @intCast(material_index_n.integer);
                    const material_n = root_n.?.object.get("materials").?.array.items[material_index];

                    if (primitive_n.object.get("material") != null and _textures_n != null and _images_n != null) {
                        if (material_n.object.get("pbrMetallicRoughness") != null and material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture") != null) {
                            const base_color_texture_index: usize = @intCast(material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture").?.object.get("index").?.integer);
                            const base_color_texture_image_index: usize = @intCast(_textures_n.?.array.items[base_color_texture_index].object.get("source").?.integer);
                            const base_color_texture_image_str = _images_n.?.array.items[base_color_texture_image_index].object.get("uri").?.string;
                            base_color_texture_gpu = zeng.loader.load_texture(std.fmt.allocPrint(allocator, "{s}/{s}\x00", .{ dependencies_path, base_color_texture_image_str }) catch unreachable, true, false);
                        }
                    }
                    if (material_n.object.get("pbrMetallicRoughness")) |pbr| {
                        if (pbr.object.get("baseColorFactor")) |bcf| {
                            base_color_of_material = zeng.vec3{ .x = get_float_from_numeric(bcf, 0), .y = get_float_from_numeric(bcf, 1), .z = get_float_from_numeric(bcf, 2) };
                        }
                        if (pbr.object.get("metallicFactor")) |mf| {
                            metallic_of_material = get_float_from_numeric_value(mf);
                        }
                        if (pbr.object.get("roughnessFactor")) |rf| {
                            roughness_of_material = get_float_from_numeric_value(rf);
                        }
                    }
                }

                var position_buffer: usize, var position_data_offset: usize, var position_data_len: usize, var position_component_type: usize = .{ 0, 0, 0, 5126 };
                if (attributes_n.object.get("POSITION")) |position_accessor_n| {
                    position_buffer, position_data_offset, position_data_len, position_component_type = get_offsest_and_length2(@intCast(position_accessor_n.integer), accessors_n.*, bufferviews_n.*);
                }
                const position_component_size = get_component_type_size(position_component_type);

                var normal_buffer: usize, var normal_data_offset: usize, var normal_data_len: usize, var normal_component_type: usize = .{ 0, 0, 0, 5126 };
                if (attributes_n.object.get("NORMAL")) |normal_accessor_n| {
                    normal_buffer, normal_data_offset, normal_data_len, normal_component_type = get_offsest_and_length2(@intCast(normal_accessor_n.integer), accessors_n.*, bufferviews_n.*);
                }
                const normal_component_size = get_component_type_size(normal_component_type);

                var texcoord_data_buffer: usize, var texcoord_data_offset: usize, var texcoord_data_len: usize, var texcoord_component_type: usize = .{ 0, 0, 0, 5126 };
                if (attributes_n.object.get("TEXCOORD_0")) |texcoord_accessor_n| {
                    texcoord_data_buffer, texcoord_data_offset, texcoord_data_len, texcoord_component_type = get_offsest_and_length2(@intCast(texcoord_accessor_n.integer), accessors_n.*, bufferviews_n.*);
                }
                const texcoord_component_size = get_component_type_size(texcoord_component_type);

                var joints_buffer: usize, var joints_data_offset: usize, var joints_data_len: usize, var joints_component_type: usize = .{ 0, 0, 0, 5121 };
                if (attributes_n.object.get("JOINTS_0")) |joints_accessor_n| {
                    joints_buffer, joints_data_offset, joints_data_len, joints_component_type = get_offsest_and_length2(@intCast(joints_accessor_n.integer), accessors_n.*, bufferviews_n.*);
                }
                const joints_component_size = get_component_type_size(joints_component_type);

                var weights_buffer: usize, var weights_data_offset: usize, var weights_data_len: usize, var weights_component_type: usize = .{ 0, 0, 0, 5126 };
                if (attributes_n.object.get("WEIGHTS_0")) |weights_accessor_n| {
                    weights_buffer, weights_data_offset, weights_data_len, weights_component_type = get_offsest_and_length2(@intCast(weights_accessor_n.integer), accessors_n.*, bufferviews_n.*);
                }
                const weights_component_size = get_component_type_size(weights_component_type);

                const indices_data_buffer, const indices_data_offset: usize, const indices_data_len: usize, const indices_component_type: usize = get_offsest_and_length2(@intCast(primitive_n.object.get("indices").?.integer), accessors_n.*, bufferviews_n.*);
                const indices_component_size = get_component_type_size(indices_component_type);

                const mesh_data_size: usize = (position_data_len / position_component_size) * (3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size + 4 * joints_component_size + 4 * weights_component_size);
                var mesh_data = allocator.alloc(u8, mesh_data_size) catch unreachable;

                std.debug.assert(get_component_type_enum(position_component_type) == zeng.gl.FLOAT);
                std.debug.assert(get_component_type_enum(normal_component_type) == zeng.gl.FLOAT);
                std.debug.assert(get_component_type_enum(texcoord_component_type) == zeng.gl.FLOAT);
                std.debug.assert(get_component_type_enum(joints_component_type) == zeng.gl.UNSIGNED_BYTE);
                std.debug.assert(get_component_type_enum(weights_component_type) == zeng.gl.FLOAT);
                std.debug.assert(get_component_type_enum(indices_component_type) == zeng.gl.UNSIGNED_SHORT);
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[(@intCast(attributes_n.object.get("POSITION").?.integer))].object.get("type").?.string, "VEC3"));
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[(@intCast(attributes_n.object.get("NORMAL").?.integer))].object.get("type").?.string, "VEC3"));
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[(@intCast(attributes_n.object.get("TEXCOORD_0").?.integer))].object.get("type").?.string, "VEC2"));
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[(@intCast(attributes_n.object.get("JOINTS_0").?.integer))].object.get("type").?.string, "VEC4"));
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[(@intCast(attributes_n.object.get("WEIGHTS_0").?.integer))].object.get("type").?.string, "VEC4"));
                // std.debug.assert(std.mem.eql(u8, root_n.?.object.get("accessors").?.array.items[@intCast(primitive_n.object.get("indices").?.integer)].object.get("type").?.string, "SCALAR"));
                // std.debug.assert(position_data_len > 0);
                // std.debug.assert(normal_data_len > 0);
                // std.debug.assert(position_data_len > 0);
                // std.debug.assert(joints_data_len > 0);
                // std.debug.assert(weights_data_len > 0);
                // std.debug.assert(indices_data_len > 0);

                if (skin_index_n == null) { // create a collider based on this STATIC mesh and add it to the global list of colliders
                    var collider_positions: []zeng.vec3 = undefined;
                    var collider_indices: []u32 = undefined;

                    collider_positions = allocator.alloc(zeng.vec3, position_data_len / 12) catch unreachable;
                    @memcpy(@as([*]u8, @ptrCast(collider_positions)), buffers[position_buffer][position_data_offset .. position_data_offset + position_data_len]);

                    collider_indices = allocator.alloc(u32, indices_data_len / 2) catch unreachable;
                    var curr_ind: usize = 0;
                    while (curr_ind < indices_data_len) {
                        defer curr_ind += 2;

                        var i: u16 = undefined;
                        @memcpy(@as([*]u8, @ptrCast(&i)), buffers[indices_data_buffer][indices_data_offset + curr_ind .. indices_data_offset + curr_ind + 2]);
                        const _i: u32 = @intCast(i);
                        collider_indices[curr_ind / 2] = _i;
                    }

                    const collider_mesh = zeng.cpu_mesh{ .indices = collider_indices, .positions = collider_positions };

                    if (global_colliders == null) {
                        global_matrices = std.ArrayList(zeng.world_matrix).initCapacity(allocator, 0) catch unreachable;
                        global_colliders = std.ArrayList(zeng.cpu_mesh).initCapacity(allocator, 0) catch unreachable;
                    }
                    global_colliders.?.append(allocator, collider_mesh) catch unreachable;
                    global_matrices.?.append(allocator, mat) catch unreachable;

                    std.debug.assert(joints_data_len == 0);
                    std.debug.assert(weights_data_len == 0);
                }
                const index_data = allocator.alloc(u8, indices_data_len) catch unreachable;
                @memcpy(@as([*]u8, @ptrCast(index_data)), buffers[indices_data_buffer][indices_data_offset .. indices_data_offset + indices_data_len]);

                var _curr: usize = 0;
                var _i: usize = 0;
                var _j: usize = 0;
                var _k: usize = 0;
                var _l: usize = 0;
                var _m: usize = 0;
                while (_i < position_data_len) {
                    if (position_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 3 * position_component_size],
                            buffers[position_buffer][position_data_offset + _i .. position_data_offset + _i + 3 * position_component_size],
                        );
                    } else {
                        unreachable;
                    }
                    _i += 3 * position_component_size;
                    _curr += 3 * position_component_size;

                    if (normal_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 3 * normal_component_size],
                            buffers[normal_buffer][normal_data_offset + _j .. normal_data_offset + _j + 3 * normal_component_size],
                        );
                    } else {
                        @memset(mesh_data[_curr .. _curr + 3 * normal_component_size], 0);
                    }
                    _j += 3 * normal_component_size;
                    _curr += 3 * normal_component_size;

                    if (texcoord_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 2 * texcoord_component_size],
                            buffers[texcoord_data_buffer][texcoord_data_offset + _k .. texcoord_data_offset + _k + 2 * texcoord_component_size],
                        );
                    } else {
                        @memset(mesh_data[_curr .. _curr + 2 * texcoord_component_size], 0);
                    }
                    _k += 2 * texcoord_component_size;
                    _curr += 2 * texcoord_component_size;

                    if (skin_index_n != null) {
                        if (joints_data_len > 0) {
                            @memcpy(
                                mesh_data[_curr .. _curr + 4 * joints_component_size],
                                buffers[joints_buffer][joints_data_offset + _l .. joints_data_offset + _l + 4 * joints_component_size],
                            );
                        } else {
                            @memset(mesh_data[_curr .. _curr + 4 * joints_component_size], 0);
                        }
                        _l += 4 * joints_component_size;
                        _curr += 4 * joints_component_size;

                        if (weights_data_len > 0) {
                            @memcpy(
                                mesh_data[_curr .. _curr + 4 * weights_component_size],
                                buffers[weights_buffer][weights_data_offset + _m .. weights_data_offset + _m + 4 * weights_component_size],
                            );
                        } else {
                            @memset(mesh_data[_curr .. _curr + 4 * weights_component_size], 0);
                        }
                        _m += 4 * weights_component_size;
                        _curr += 4 * weights_component_size;
                    }
                }

                if (skin_index_n != null) {
                    var VAO: u32 = undefined;
                    zeng.gl.genVertexArrays(1, &VAO);
                    zeng.gl.bindVertexArray(VAO);

                    var VBO: u32 = undefined;
                    zeng.gl.genBuffers(1, &VBO);
                    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
                    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @intCast(mesh_data.len), mesh_data.ptr, zeng.gl.STATIC_DRAW);

                    var EBO: u32 = undefined;
                    zeng.gl.genBuffers(1, &EBO);
                    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
                    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @intCast(index_data.len), index_data.ptr, zeng.gl.STATIC_DRAW);

                    const stride: c_int = @intCast(3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size + 4 * joints_component_size + 4 * weights_component_size);
                    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(0)); // position
                    zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size)); // normal
                    zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size + 3 * normal_component_size)); // uv
                    zeng.gl.vertexAttribIPointer(3, 4, get_component_type_enum(joints_component_type), stride, @ptrFromInt(3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size)); // joint index
                    zeng.gl.vertexAttribPointer(4, 4, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size + 4 * joints_component_size)); // joint weights

                    zeng.gl.enableVertexAttribArray(0);
                    zeng.gl.enableVertexAttribArray(1);
                    zeng.gl.enableVertexAttribArray(2);
                    zeng.gl.enableVertexAttribArray(3);
                    zeng.gl.enableVertexAttribArray(4);

                    result_skinmesh_to_skeleton.put(current_node_index, @intCast(skin_index_n.?.integer)) catch unreachable;
                    result_nodes.append(allocator, scene_node_w_matrix{
                        .node = scene_node{
                            .skinned_mesh = zeng.skinned_mesh{
                                .indices_length = @intCast(indices_data_len / indices_component_size),
                                .indices_type = get_component_type_enum(indices_component_type),
                                .material = .{
                                    .shader_program = skin_shader_program,
                                    .parameter_map = blk: {
                                        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
                                        _value.put("albedo_texture", .{ .texture = base_color_texture_gpu }) catch unreachable;
                                        _value.put("albedo", .{ .float_3 = base_color_of_material orelse zeng.vec3.ONE }) catch unreachable;
                                        _value.put("metallic", .{ .float_1 = metallic_of_material }) catch unreachable;
                                        _value.put("roughness", .{ .float_1 = roughness_of_material }) catch unreachable;

                                        break :blk _value;
                                    },
                                },
                                .vao_gpu = VAO,
                                .skeleton = undefined,
                            },
                        },
                        .matrix = mat,
                        .gltf_id = current_node_index,
                    }) catch unreachable;
                } else {
                    var VAO: u32 = undefined;
                    zeng.gl.genVertexArrays(1, &VAO);
                    zeng.gl.bindVertexArray(VAO);

                    var VBO: u32 = undefined;
                    zeng.gl.genBuffers(1, &VBO);
                    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
                    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @intCast(mesh_data.len), mesh_data.ptr, zeng.gl.STATIC_DRAW);

                    var EBO: u32 = undefined;
                    zeng.gl.genBuffers(1, &EBO);
                    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
                    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @intCast(indices_data_len), index_data.ptr, zeng.gl.STATIC_DRAW);

                    const stride: c_int = @intCast(3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size);
                    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(0)); // position
                    zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size)); // normal
                    zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size + 3 * normal_component_size)); // uv

                    zeng.gl.enableVertexAttribArray(0);
                    zeng.gl.enableVertexAttribArray(1);
                    zeng.gl.enableVertexAttribArray(2);

                    result_nodes.append(allocator, scene_node_w_matrix{
                        .node = scene_node{
                            .static_mesh = zeng.mesh{
                                .indices_length = @intCast(indices_data_len / indices_component_size),
                                .indices_type = get_component_type_enum(indices_component_type),
                                .material = .{
                                    .shader_program = static_shader_program,
                                    .parameter_map = blk: {
                                        var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
                                        _value.put("albedo_texture", .{ .texture = base_color_texture_gpu }) catch unreachable;
                                        _value.put("albedo", .{ .float_3 = base_color_of_material orelse zeng.vec3.ONE }) catch unreachable;
                                        _value.put("metallic", .{ .float_1 = metallic_of_material }) catch unreachable;
                                        _value.put("roughness", .{ .float_1 = roughness_of_material }) catch unreachable;

                                        break :blk _value;
                                    },
                                },
                                .vao_gpu = VAO,
                            },
                        },
                        .matrix = mat,
                        .gltf_id = current_node_index,
                    }) catch unreachable;
                }
            }
        }
        // else if (mesh_index_n != null) { // regular mesh
        //     var translation: zeng.vec3 = zeng.vec3.ZERO;
        //     var scale: zeng.vec3 = zeng.vec3.ONE;
        //     var rotation: zeng.quat = zeng.quat.IDENTITY;

        //     if (current_node_n.object.get("translation")) |_translation| {
        //         translation.x = get_float_from_numeric(_translation, 0);
        //         translation.y = get_float_from_numeric(_translation, 1);
        //         translation.z = get_float_from_numeric(_translation, 2);
        //     }
        //     if (current_node_n.object.get("rotation")) |_rotation| {
        //         rotation.x = get_float_from_numeric(_rotation, 0);
        //         rotation.y = get_float_from_numeric(_rotation, 1);
        //         rotation.z = get_float_from_numeric(_rotation, 2);
        //         rotation.w = get_float_from_numeric(_rotation, 3);
        //     }
        //     if (current_node_n.object.get("scale")) |_scale| {
        //         scale.x = get_float_from_numeric(_scale, 0);
        //         scale.y = get_float_from_numeric(_scale, 1);
        //         scale.z = get_float_from_numeric(_scale, 2);
        //     }
        //     const mat = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotation), zeng.mat_scal(zeng.mat_identity, scale)), translation);

        //     const mesh_n = root_n.?.object.get("meshes").?.array.items[@intCast(mesh_index_n.?.integer)];
        //     for (mesh_n.object.get("primitives").?.array.items) |primitive_n| {
        //         var base_color_of_material: ?zeng.vec3 = null;
        //         var metallic_of_material: f32 = 0.0;
        //         var roughness_of_material: f32 = 0.5;
        //         var base_color_texture_gpu: u32 = default_texture;
        //         const attributes_n = primitive_n.object.get("attributes").?;
        //         // const material_index: usize = @intCast(primitive_n.object.get("material").?.integer);
        //         if (primitive_n.object.get("material")) |material_index_n| {
        //             const material_index: usize = @intCast(material_index_n.integer);
        //             const material_n = root_n.?.object.get("materials").?.array.items[material_index];

        //             if (primitive_n.object.get("material") != null and _textures_n != null and _images_n != null) {
        //                 if (material_n.object.get("pbrMetallicRoughness") != null and material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture") != null) {
        //                     const base_color_texture_index: usize = @intCast(material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture").?.object.get("index").?.integer);
        //                     const base_color_texture_image_index: usize = @intCast(_textures_n.?.array.items[base_color_texture_index].object.get("source").?.integer);
        //                     const base_color_texture_image_str = _images_n.?.array.items[base_color_texture_image_index].object.get("uri").?.string;
        //                     base_color_texture_gpu = zeng.loader.load_texture(std.fmt.allocPrint(allocator, "{s}/{s}\x00", .{ dependencies_path, base_color_texture_image_str }) catch unreachable, true, false);
        //                 }
        //             }
        //             if (material_n.object.get("pbrMetallicRoughness")) |pbr| {
        //                 if (pbr.object.get("baseColorFactor")) |bcf| {
        //                     base_color_of_material = zeng.vec3{ .x = get_float_from_numeric(bcf, 0), .y = get_float_from_numeric(bcf, 1), .z = get_float_from_numeric(bcf, 2) };
        //                 }
        //                 if (pbr.object.get("metallicFactor")) |mf| {
        //                     metallic_of_material = get_float_from_numeric_value(mf);
        //                 }
        //                 if (pbr.object.get("roughnessFactor")) |rf| {
        //                     roughness_of_material = get_float_from_numeric_value(rf);
        //                 }
        //             }
        //         }

        //         const position_buffer, const position_data_offset: usize, const position_data_len: usize, const position_component_type: usize = get_offsest_and_length2(@intCast(attributes_n.object.get("POSITION").?.integer), accessors_n.*, bufferviews_n.*);
        //         const position_component_size = get_component_type_size(position_component_type);

        //         const normal_data_buffer, const normal_data_offset: usize, const normal_data_len: usize, const normal_component_type: usize = get_offsest_and_length2(@intCast(attributes_n.object.get("NORMAL").?.integer), accessors_n.*, bufferviews_n.*);
        //         const normal_component_size = get_component_type_size(normal_component_type);

        //         var texcoord_data_buffer: usize, var texcoord_data_offset: usize, var texcoord_data_len: usize, var texcoord_component_type: usize = .{ 0, 0, 0, 5126 };
        //         if (attributes_n.object.get("TEXCOORD_0") != null)
        //             texcoord_data_buffer, texcoord_data_offset, texcoord_data_len, texcoord_component_type = get_offsest_and_length2(@intCast(attributes_n.object.get("TEXCOORD_0").?.integer), accessors_n.*, bufferviews_n.*);
        //         const texcoord_component_size = get_component_type_size(texcoord_component_type);

        //         const indices_data_buffer, const indices_data_offset: usize, const indices_data_len: usize, const indices_component_type: usize = get_offsest_and_length2(@intCast(primitive_n.object.get("indices").?.integer), accessors_n.*, bufferviews_n.*);
        //         const indices_component_size = get_component_type_size(indices_component_type);

        //         const mesh_data_size: usize = (position_data_len / position_component_size) * (3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size);
        //         var mesh_data = allocator.alloc(u8, mesh_data_size) catch unreachable;

        //         std.debug.assert(get_component_type_enum(position_component_type) == zeng.gl.FLOAT);
        //         std.debug.assert(get_component_type_enum(normal_component_type) == zeng.gl.FLOAT);
        //         std.debug.assert(get_component_type_enum(texcoord_component_type) == zeng.gl.FLOAT);
        //         std.debug.assert(get_component_type_enum(indices_component_type) == zeng.gl.UNSIGNED_SHORT);

        //         { // create a collider based on this mesh and add it to the global list of colliders
        //             var collider_positions: []zeng.vec3 = undefined;
        //             var collider_indices: []u32 = undefined;

        //             collider_positions = allocator.alloc(zeng.vec3, position_data_len / 12) catch unreachable;
        //             @memcpy(@as([*]u8, @ptrCast(collider_positions)), buffers[position_buffer][position_data_offset .. position_data_offset + position_data_len]);

        //             collider_indices = allocator.alloc(u32, indices_data_len / 2) catch unreachable;
        //             var curr_ind: usize = 0;
        //             while (curr_ind < indices_data_len) {
        //                 defer curr_ind += 2;

        //                 var i: u16 = undefined;
        //                 @memcpy(@as([*]u8, @ptrCast(&i)), buffers[indices_data_buffer][indices_data_offset + curr_ind .. indices_data_offset + curr_ind + 2]);
        //                 const _i: u32 = @intCast(i);
        //                 collider_indices[curr_ind / 2] = _i;
        //             }

        //             const collider_mesh = zeng.cpu_mesh{ .indices = collider_indices, .positions = collider_positions };

        //             if (global_colliders == null) {
        //                 global_matrices = std.ArrayList(zeng.world_matrix).initCapacity(allocator, 0) catch unreachable;
        //                 global_colliders = std.ArrayList(zeng.cpu_mesh).initCapacity(allocator, 0) catch unreachable;
        //             }
        //             global_colliders.?.append(allocator, collider_mesh) catch unreachable;
        //             global_matrices.?.append(allocator, mat) catch unreachable;
        //         }

        //         var _curr: usize = 0;
        //         var _i: usize = 0;
        //         var _j: usize = 0;
        //         var _k: usize = 0;
        //         while (_i < position_data_len) {
        //             if (position_data_len > 0) {
        //                 @memcpy(
        //                     mesh_data[_curr .. _curr + 3 * position_component_size],
        //                     buffers[position_buffer][position_data_offset + _i .. position_data_offset + _i + 3 * position_component_size],
        //                 );
        //             } else {
        //                 unreachable;
        //             }
        //             _i += 3 * position_component_size;
        //             _curr += 3 * position_component_size;

        //             if (normal_data_len > 0) {
        //                 @memcpy(
        //                     mesh_data[_curr .. _curr + 3 * normal_component_size],
        //                     buffers[normal_data_buffer][normal_data_offset + _j .. normal_data_offset + _j + 3 * normal_component_size],
        //                 );
        //             } else {
        //                 @memset(mesh_data[_curr .. _curr + 3 * normal_component_size], 0);
        //             }
        //             _j += 3 * normal_component_size;
        //             _curr += 3 * normal_component_size;

        //             if (texcoord_data_len > 0) {
        //                 @memcpy(
        //                     mesh_data[_curr .. _curr + 2 * texcoord_component_size],
        //                     buffers[texcoord_data_buffer][texcoord_data_offset + _k .. texcoord_data_offset + _k + 2 * texcoord_component_size],
        //                 );
        //             } else {
        //                 @memset(mesh_data[_curr .. _curr + 2 * texcoord_component_size], 0);
        //             }
        //             _k += 2 * texcoord_component_size;
        //             _curr += 2 * texcoord_component_size;
        //         }
        //         std.debug.assert(_i == position_data_len);

        //         const index_data = allocator.alloc(u8, indices_data_len) catch unreachable;

        //         @memcpy(@as([*]u8, @ptrCast(index_data)), buffers[indices_data_buffer][indices_data_offset .. indices_data_offset + indices_data_len]);

        //         var VAO: u32 = undefined;
        //         zeng.gl.genVertexArrays(1, &VAO);
        //         zeng.gl.bindVertexArray(VAO);

        //         var VBO: u32 = undefined;
        //         zeng.gl.genBuffers(1, &VBO);
        //         zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
        //         zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @intCast(mesh_data.len), mesh_data.ptr, zeng.gl.STATIC_DRAW);

        //         var EBO: u32 = undefined;
        //         zeng.gl.genBuffers(1, &EBO);
        //         zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
        //         zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @intCast(indices_data_len), index_data.ptr, zeng.gl.STATIC_DRAW);

        //         const stride: c_int = @intCast(3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size);
        //         zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(0)); // position
        //         zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size)); // normal
        //         zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, stride, @ptrFromInt(3 * position_component_size + 3 * normal_component_size)); // uv

        //         zeng.gl.enableVertexAttribArray(0);
        //         zeng.gl.enableVertexAttribArray(1);
        //         zeng.gl.enableVertexAttribArray(2);

        //         result_nodes.append(allocator, scene_node_w_matrix{
        //             .node = scene_node{
        //                 .static_mesh = zeng.mesh{
        //                     .indices_length = @intCast(indices_data_len / indices_component_size),
        //                     .indices_type = get_component_type_enum(indices_component_type),
        //                     .material = .{
        //                         .shader_program = static_shader_program,
        //                         .parameter_map = blk: {
        //                             var _value = std.StringHashMap(zeng.material_parameter).init(allocator);
        //                             _value.put("albedo_texture", .{ .texture = base_color_texture_gpu }) catch unreachable;
        //                             _value.put("albedo", .{ .float_3 = base_color_of_material orelse zeng.vec3.ONE }) catch unreachable;
        //                             _value.put("metallic", .{ .float_1 = metallic_of_material }) catch unreachable;
        //                             _value.put("roughness", .{ .float_1 = roughness_of_material }) catch unreachable;

        //                             break :blk _value;
        //                         },
        //                     },
        //                     .vao_gpu = VAO,
        //                 },
        //             },
        //             .matrix = mat,
        //             .gltf_id = current_node_index,
        //         }) catch unreachable;
        //     }
        // }
        else if (current_node_n.object.get("children") != null) {
            var translation: zeng.vec3 = zeng.vec3.ZERO;
            var scale: zeng.vec3 = zeng.vec3.ONE;
            var rotation: zeng.quat = zeng.quat.IDENTITY;

            if (current_node_n.object.get("translation")) |_translation| {
                translation.x = get_float_from_numeric(_translation, 0);
                translation.y = get_float_from_numeric(_translation, 1);
                translation.z = get_float_from_numeric(_translation, 2);
            }
            if (current_node_n.object.get("rotation")) |_rotation| {
                rotation.x = get_float_from_numeric(_rotation, 0);
                rotation.y = get_float_from_numeric(_rotation, 1);
                rotation.z = get_float_from_numeric(_rotation, 2);
                rotation.w = get_float_from_numeric(_rotation, 3);
            }
            if (current_node_n.object.get("scale")) |_scale| {
                scale.x = get_float_from_numeric(_scale, 0);
                scale.y = get_float_from_numeric(_scale, 1);
                scale.z = get_float_from_numeric(_scale, 2);
            }
            const mat = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotation), zeng.mat_scal(zeng.mat_identity, scale)), translation);

            result_nodes.append(allocator, scene_node_w_matrix{
                .node = scene_node{
                    .empty = void{},
                },
                .matrix = mat,
                .gltf_id = current_node_index,
            }) catch unreachable;
        }
    }

    current_node_index = 0;
    for (nodes_n.array.items) |current_node_n| {
        defer current_node_index += 1;

        if (current_node_n.object.get("name")) |name| {
            result_names.put(current_node_index, name.string) catch unreachable;
        }

        var translation: zeng.vec3 = zeng.vec3.ZERO;
        var scale: zeng.vec3 = zeng.vec3.ONE;
        var rotation: zeng.quat = zeng.quat.IDENTITY;

        if (current_node_n.object.get("translation")) |_translation| {
            translation.x = get_float_from_numeric(_translation, 0);
            translation.y = get_float_from_numeric(_translation, 1);
            translation.z = get_float_from_numeric(_translation, 2);
        }
        if (current_node_n.object.get("rotation")) |_rotation| {
            rotation.x = get_float_from_numeric(_rotation, 0);
            rotation.y = get_float_from_numeric(_rotation, 1);
            rotation.z = get_float_from_numeric(_rotation, 2);
            rotation.w = get_float_from_numeric(_rotation, 3);
        }
        if (current_node_n.object.get("scale")) |_scale| {
            scale.x = get_float_from_numeric(_scale, 0);
            scale.y = get_float_from_numeric(_scale, 1);
            scale.z = get_float_from_numeric(_scale, 2);
        }

        if (joint_to_skin.get(current_node_index)) |my_skeleton| {
            const index_within_skeleton = skeleton_space_maps.items[my_skeleton].get(current_node_index).?;
            result_skeletons.items[my_skeleton].default_bone_translations[index_within_skeleton] = translation;
            result_skeletons.items[my_skeleton].default_bone_rotations[index_within_skeleton] = rotation;
            result_skeletons.items[my_skeleton].default_bone_scales[index_within_skeleton] = scale;
        }
    }

    return .{ result_nodes.items, result_names, result_animations.items, result_animation_names.items, result_skeletons.items, result_children_map, result_top_level_objects, result_skinmesh_to_skeleton };
}
pub fn auto_import(datablob: *zeng.Datablob, world: *ecs.world, folder_name: anytype, file_name: anytype, skin_shader: u32, static_shader: u32, default_texture: u32, allocator: std.mem.Allocator) ecs.entity_id {
    const gltf_extraction_type = @typeInfo(@TypeOf(gltf_extract_resources)).@"fn".return_type.?;
    const full_file_path = std.fmt.allocPrint(allocator, "{s}/{s}.gltf", .{ folder_name, file_name }) catch unreachable;

    if (datablob.get_maybe(full_file_path, gltf_extraction_type)) |_cached_gltf_extraction| {
        const mesh_slice, const names_map, _, _, const skeleton_slice, const parent_child_map, const top_level_children, const skinned_mesh_to_skeleton = _cached_gltf_extraction.*;
        return zeng.loader.instantiate_model_hierarchy(mesh_slice, names_map, parent_child_map, top_level_children, skeleton_slice, skinned_mesh_to_skeleton, world, allocator);
    }

    const gltf_bytes = get_file_bytes(full_file_path, allocator);

    const parsed_gltf = gltf_parse(gltf_bytes, allocator);
    var buffers = std.ArrayList([]u8).initCapacity(allocator, parsed_gltf.?.object.get("buffers").?.array.items.len) catch unreachable;

    const decoder = std.base64.Base64Decoder.init(std.base64.standard_alphabet_chars, '=');
    for (parsed_gltf.?.object.get("buffers").?.array.items) |buffer_n| {
        const byte_length: usize = @intCast(buffer_n.object.get("byteLength").?.integer);
        const uri = buffer_n.object.get("uri");
        const PREFIX = "data:application/octet-stream;base64,";
        if (uri.?.string.len > PREFIX.len and std.mem.eql(u8, uri.?.string[0..PREFIX.len], PREFIX)) {
            std.debug.assert(decoder.calcSizeForSlice(uri.?.string[PREFIX.len..]) catch unreachable == byte_length);
            const data = allocator.alloc(u8, byte_length) catch unreachable;

            decoder.decode(data, uri.?.string[PREFIX.len..]) catch unreachable;

            buffers.append(allocator, data) catch unreachable;
        } else {
            const full_data_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ folder_name, uri.?.string }) catch unreachable;
            const bin_bytes = get_file_bytes(full_data_path, allocator);

            buffers.append(allocator, bin_bytes) catch unreachable;
        }
    }

    const gltf_extraction: gltf_extraction_type = gltf_extract_resources(parsed_gltf, buffers.items, folder_name, allocator, skin_shader, static_shader, default_texture);
    const cached_gltf_extraction = allocator.create(gltf_extraction_type) catch unreachable;
    cached_gltf_extraction.* = gltf_extraction;
    datablob.put(full_file_path, cached_gltf_extraction);

    const mesh_slice, const names_map, const animation_slice, const animation_name_slice, const skeleton_slice, const parent_child_map, const top_level_children, const skinned_mesh_to_skeleton = gltf_extraction;
    for (animation_slice, animation_name_slice) |*_animation, animation_name| {
        const full_animation_path = std.fmt.allocPrint(allocator, "{s}/animations/{s}", .{ full_file_path, animation_name }) catch unreachable;
        datablob.put(full_animation_path, _animation);
    }

    return zeng.loader.instantiate_model_hierarchy(mesh_slice, names_map, parent_child_map, top_level_children, skeleton_slice, skinned_mesh_to_skeleton, world, allocator);
}
