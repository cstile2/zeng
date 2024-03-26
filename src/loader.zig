const std = @import("std");
const Engine = @import("engine.zig");
pub const c = @cImport({
    @cInclude("stb_image.h");
});

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
        std.debug.print("size of name: {}\n", .{size_name});

        // read in the name (there may be padding so that an alignment of 4 is maintained)
        const name_data = data_bytes[curr_byte .. curr_byte + padded_size];
        curr_byte = curr_byte + padded_size;
        std.debug.print("'{s}' :\n", .{name_data});

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
        Engine.gl.genBuffers(1, &VBO);
        Engine.gl.bindBuffer(Engine.gl.ARRAY_BUFFER, VBO);
        Engine.gl.bufferData(Engine.gl.ARRAY_BUFFER, sizea, @ptrCast(mesh_vertex_data), Engine.gl.STATIC_DRAW);

        // create element buffer object (indices array) > bind it > store the data from the indices array
        var EBO: u32 = undefined;
        Engine.gl.genBuffers(1, &EBO);
        Engine.gl.bindBuffer(Engine.gl.ELEMENT_ARRAY_BUFFER, EBO);
        Engine.gl.bufferData(Engine.gl.ELEMENT_ARRAY_BUFFER, sizeb, @ptrCast(mesh_indices_data), Engine.gl.STATIC_DRAW);

        // data layout is : position.xyz, normal.xyz, uv.xy, repeat
        Engine.gl.vertexAttribPointer(0, 3, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(0));
        Engine.gl.enableVertexAttribArray(0);
        Engine.gl.vertexAttribPointer(1, 3, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32)));
        Engine.gl.enableVertexAttribArray(1);
        Engine.gl.vertexAttribPointer(2, 2, Engine.gl.FLOAT, Engine.gl.FALSE, 8 * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32)));
        Engine.gl.enableVertexAttribArray(2);

        // create a transformation matrix using the position, rotation read from the file
        var transform: [16]f32 = Engine.identity();
        transform = Engine.multiply_matrices(Engine.QuatToMatrix(transform_rotation), transform);
        transform[12..15].* = transform_position.*;

        Engine.CreateEntity(entity_array, Engine.Entity{ .mesh = .{ .vao_gpu = VAO, .indices_length = @divTrunc(@as(i32, @intCast(sizeb)), 4), .material = Engine.Material{ .shader_program_GPU = shader_program_GPU, .texture_GPU = texture } }, .world_matrix = transform, .component_flags = Engine.ComponentFlags{ .mesh = true }, .camera = undefined });
        entity_array.*[entity_array.len - 1].name = allocator.alloc(u8, size_name) catch unreachable;
        std.mem.copyForwards(u8, entity_array.*[entity_array.len - 1].name, name_data[0..size_name]);

        res[res_count] = &entity_array.*[entity_array.len - 1];
        res_count += 1;
    }

    return res[0..res_count];
}
