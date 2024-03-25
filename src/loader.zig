const std = @import("std");
const gl = @import("gl");
const Engine = struct {
    usingnamespace @import("data_types.zig");
};

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

pub fn SeparateText(text: []const u8, comptime delimiter: u8) [][]u8 {
    var ret = std.heap.c_allocator.alloc([]u8, 50) catch unreachable;
    var ret_count: u64 = 0;
    var buffer = [_]u8{'x'} ** 1024;
    var offset: u64 = 0;
    var length: u32 = 0;
    for (text, 0..) |char, i| {
        if (char == delimiter) {
            if (length > 0) {
                ret[ret_count] = std.heap.c_allocator.dupe(u8, buffer[0..length]) catch unreachable;
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
        ret[ret_count] = std.heap.c_allocator.dupe(u8, buffer[0..length]) catch unreachable;
        ret_count += 1;
    }

    return ret[0..ret_count];
}

pub fn ImportModelAsset(filepath: anytype, allocator: std.mem.Allocator, shader_program_GPU: u32, texture: u32, entity_array: *[]Engine.Entity) []*Engine.Entity {
    var res = allocator.alloc(*Engine.Entity, 64) catch unreachable;
    var res_count: u32 = 0;
    // open file from filepath > close after done
    const file = std.fs.cwd().openFile(filepath, .{}) catch unreachable;
    defer file.close();
    // get size of the file (in bytes)
    const stat = file.stat() catch unreachable;
    // read the file and store it into a dynamically allocated array of u8
    var data_bytes = file.reader().readAllAlloc(allocator, stat.size) catch unreachable;
    std.debug.print("{}, size: {}\n", .{ @TypeOf(data_bytes), data_bytes.len });
    var curr_byte: u32 = 0;
    var index: u8 = 0;
    while (curr_byte < data_bytes.len) {
        defer index += 1;

        // create vao mesh
        var VAO: u32 = undefined;
        gl.genVertexArrays(1, &VAO);
        gl.bindVertexArray(VAO);
        defer {
            gl.bindVertexArray(0);
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            gl.bindBuffer(gl.ARRAY_BUFFER, 0);
        }

        // read current place as a u32 > increment current byte by size of this u32 (4 bytes)
        const sizea: u32 = @as(*u32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4]))).*;
        curr_byte = curr_byte + 4;
        std.debug.print("sizea of curr: {}\n", .{sizea});

        // read a bunch of floats (amount is 'sizea') which represents the model's vertex data
        const mesh_vertex_data = data_bytes[curr_byte .. curr_byte + sizea];
        curr_byte = curr_byte + sizea;

        // read current place as a u32 > increment current byte by size of this u32 (4 bytes)
        const sizeb: u32 = @as(*u32, @alignCast(@ptrCast(data_bytes[curr_byte .. curr_byte + 4]))).*;
        curr_byte = curr_byte + 4;
        std.debug.print("sizeb of curr: {}\n", .{sizeb});

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
        gl.genBuffers(1, &VBO);
        gl.bindBuffer(gl.ARRAY_BUFFER, VBO);
        gl.bufferData(gl.ARRAY_BUFFER, sizea, @ptrCast(mesh_vertex_data), gl.STATIC_DRAW);

        // create element buffer object (indices array) > bind it > store the data from the indices array
        var EBO: u32 = undefined;
        gl.genBuffers(1, &EBO);
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, EBO);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, sizeb, @ptrCast(mesh_indices_data), gl.STATIC_DRAW);

        // data layout is : position.xyz, normal.xyz, uv.xy, repeat
        gl.vertexAttribPointer(0, 3, gl.FLOAT, gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(0));
        gl.enableVertexAttribArray(0);
        gl.vertexAttribPointer(1, 3, gl.FLOAT, gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32)));
        gl.enableVertexAttribArray(1);
        gl.vertexAttribPointer(2, 2, gl.FLOAT, gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32)));
        gl.enableVertexAttribArray(2);

        // create a transformation matrix using the position, rotation read from the file
        var transform: [16]f32 = Engine.identity();
        transform = Engine.multiply_matrices(Engine.QuatToMatrix(transform_rotation), transform);
        transform[12..15].* = transform_position.*;

        Engine.CreateEntity(entity_array, Engine.Entity{ .vao_gpu = VAO, .indices_length = @divTrunc(@as(i32, @intCast(sizeb)), 4), .world_matrix = transform, .material = Engine.Material{ .shader_program_GPU = shader_program_GPU, .texture_GPU = texture }, .component_flags = Engine.ComponentFlags{ .mesh = true }, .camera = undefined });
        res[res_count] = &entity_array.*[entity_array.len - 1];
        res_count += 1;
    }

    return res[0..res_count];
}
