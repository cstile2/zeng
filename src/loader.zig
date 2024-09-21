const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const ECS = @import("main.zig").ECS;

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

pub fn instantiate_scene(world: *ECS.World, filepath: anytype, allocator: std.mem.Allocator, shader_program_GPU: u32, texture_GPU: u32) ecs.EntityDataLocation {
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
        else => {
            @compileError("this type cannot be serialized");
        },
    }
}

pub fn deserialize_from_bytes(T: type, dest_bytes: [*]u8, src_bytes: []u8, src_curr_byte: *u32, offset: u32) void {
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
        else => {
            @compileError("this type cannot be deserialized");
        },
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

const StringLiteral = struct {
    string: []const u8,
};
const IntConstant = struct {
    value: i128,
};
const FloatConstant = struct {
    value: f64,
};

const TokenTag = enum {
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
const Token = union(TokenTag) {
    // misc
    l_paren: void,
    r_paren: void,
    l_brace: void,
    r_brace: void,
    l_square: void,
    r_square: void,

    string_literal: StringLiteral,
    int_constant: IntConstant,
    float_constant: FloatConstant,

    semicolon: void,
    colon: void,
    comma: void,

    _true,
};

pub fn contains(char: u8, string: []const u8) bool {
    for (string) |s_char| {
        if (s_char == char) {
            return true;
        }
    }
    return false;
}

pub fn lexer(bytes: []const u8, tokens: *std.ArrayList(Token)) !void {
    var char_number: u32 = 0;
    var curr: u64 = 0;
    while (curr < bytes.len) {
        defer curr += 1;
        defer char_number += 1;

        if (bytes[curr] == '\n') {
            char_number = 0;
        } else if (bytes[curr] == ';') {
            try tokens.append(Token{ .semicolon = void{} });
        } else if (bytes[curr] == ',') {
            try tokens.append(Token{ .comma = void{} });
        } else if (bytes[curr] == ':') {
            try tokens.append(Token{ .colon = void{} });
        } else if (bytes[curr] == '{') {
            try tokens.append(Token{ .l_brace = void{} });
        } else if (bytes[curr] == '}') {
            try tokens.append(Token{ .r_brace = void{} });
        } else if (bytes[curr] == '(') {
            try tokens.append(Token{ .l_paren = void{} });
        } else if (bytes[curr] == ')') {
            try tokens.append(Token{ .r_paren = void{} });
        } else if (bytes[curr] == '[') {
            try tokens.append(Token{ .l_square = void{} });
        } else if (bytes[curr] == ']') {
            try tokens.append(Token{ .r_square = void{} });
        } else if (bytes[curr] == '"') {
            const start = curr;
            curr += 1;
            while (curr < bytes.len and bytes[curr] != '"') {
                curr += 1;
            }
            try tokens.append(Token{ .string_literal = StringLiteral{ .string = bytes[start + 1 .. curr] } });
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
                    try tokens.append(Token{ .float_constant = FloatConstant{ .value = std.fmt.parseFloat(f64, bytes[start..curr]) catch unreachable } });
                } else {
                    try tokens.append(Token{ .float_constant = FloatConstant{ .value = 0.0 } });
                }
                // std.debug.print("float value: {}\n", .{tokens.getLast().float_constant.value});
            } else {
                try tokens.append(Token{ .int_constant = IntConstant{ .value = std.fmt.parseInt(i128, bytes[start..curr], 10) catch unreachable } });
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
                try tokens.append(Token{ ._true = void{} });
            }
            curr -= 1;
        }
    }
}

const ParseError = error{
    parse_error,
};

var tab_amount: u8 = 0;
fn print_tabbed(comptime str: anytype, tup: anytype, enter: bool) void {
    if (enter) {
        tab_amount += 1;
    }
    defer {
        if (!enter) {
            tab_amount -= 1;
        }
    }
    var count: u8 = tab_amount;
    while (count != 0) {
        defer count -= 1;
        std.debug.print(". ", .{});
    }
    std.debug.print(str, tup);
}

const Primitive = struct {
    attributes: std.ArrayList(Attribute),
};
const BufferView = struct {
    length: u32,
    offset: u32,
};

pub var nodes: [1024]u32 = undefined;
pub var nodes_len: u32 = 0;
pub var meshes: [1024]Primitive = undefined;
pub var meshes_len: u32 = 0;
pub var accessors: [1024]u32 = undefined;
pub var accessors_len: u32 = 0;
pub var buffer_views: [1024]BufferView = undefined;
pub var buffer_views_len: u32 = 0;

const Sampler = struct {
    input: u32,
    output: u32,
};

const Channel = struct {
    sampler: u32,
    target_node: u32,
};

const Animation = struct {
    samplers: std.ArrayList(Sampler),
    channels: std.ArrayList(Channel),
};

pub var animations: [1024]Animation = undefined;
pub var animations_len: usize = 0;

var hierarchy: std.ArrayList([]const u8) = undefined;
fn print_hierarchy() void {
    std.debug.print("<", .{});
    for (hierarchy.items) |item| {
        std.debug.print("{s}/", .{item});
    }
    std.debug.print(">\n", .{});
}

var path_buffer: [1028]u8 = undefined;
fn get_gltf_path() []u8 {
    var curr: usize = 0;
    for (hierarchy.items) |item| {
        path_buffer[curr] = '.';
        curr += 1;
        @memcpy(path_buffer[curr .. curr + item.len], item);
        curr += item.len;
    }
    return path_buffer[0..curr];
}
fn backup(slice: []u8, count: usize) []u8 {
    return slice[slice.len - count ..];
}
fn test_hierachy(str: anytype) bool {
    const p = get_gltf_path();
    if (str.len > p.len) return false;
    return std.mem.eql(u8, backup(p, str.len), str);
}

const AttributeType = enum {
    POSITION,
    NORMAL,
    TEXCOORD_0,
    COLOR_0,
    JOINTS_0,
    WEIGHTS_0,
    indices,
};

const Attribute = struct {
    _type: AttributeType,
    index: u32,
};

var global_allocator: std.mem.Allocator = undefined;
pub fn parse_gltf(bytes: []u8, allocator: std.mem.Allocator) !void {
    global_allocator = allocator;
    nodes_len = 0;
    meshes_len = 0;
    accessors_len = 0;
    buffer_views_len = 0;

    animations_len = 0;

    hierarchy = std.ArrayList([]const u8).init(allocator);
    defer hierarchy.deinit();

    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();
    try lexer(bytes, &tokens);

    if (tokens.items[0] != .l_brace) unreachable;

    var curr: u64 = 1;
    const a = gltf_leaf_list(tokens.items, &curr);
    if (a == null) unreachable;

    if (tokens.items[curr] != .r_brace) unreachable;
}
var current_primitive: *Primitive = undefined;
var current_buffer_view: *BufferView = undefined;
var current_animation: *Animation = undefined;
var current_channel: *Channel = undefined;
var current_sampler: *Sampler = undefined;
fn match(tokens: []Token, tag: TokenTag, curr: *u64) bool {
    if (curr.* >= tokens.len)
        return false;
    defer curr.* += 1;
    if (tokens[curr.*] == tag) {
        return true;
    }
    return false;
}
pub fn gltf_leaf_list(tokens: []Token, curr: *u64) ?u64 {
    var temp = curr.*;
    while (true) {
        if (gltf_leaf(tokens, curr) == null) {
            break;
        }
        temp = curr.*;
        if (!match(tokens, .comma, curr)) {
            break;
        }
    }
    curr.* = temp;
    return curr.*;
}
pub fn gltf_leaf(tokens: []Token, curr: *u64) ?u64 {
    const temp = curr.*;
    if (match(tokens, .string_literal, curr)) {
        const str = tokens[curr.* - 1].string_literal.string;

        if (tokens[curr.*] == .colon) {
            curr.* += 1;

            hierarchy.append(str) catch unreachable;
            defer _ = hierarchy.pop();
            if (test_hierachy("attributes")) {
                meshes[meshes_len] = .{ .attributes = std.ArrayList(Attribute).init(global_allocator) };
                current_primitive = &meshes[meshes_len];
                meshes_len += 1;
            }

            const result = gltf_leaf(tokens, curr);
            return result;
        } else {
            return curr.*;
        }
    }
    curr.* = temp;

    if (match(tokens, .int_constant, curr)) {
        if (test_hierachy("meshes.[.primitives.[.attributes.POSITION")) {
            current_primitive.attributes.append(.{ ._type = .POSITION, .index = @intCast(tokens[curr.* - 1].int_constant.value) }) catch unreachable;
        } else if (test_hierachy("meshes.[.primitives.[.attributes.NORMAL")) {
            current_primitive.attributes.append(.{ ._type = .NORMAL, .index = @intCast(tokens[curr.* - 1].int_constant.value) }) catch unreachable;
        } else if (test_hierachy("meshes.[.primitives.[.attributes.TEXCOORD_0")) {
            current_primitive.attributes.append(.{ ._type = .TEXCOORD_0, .index = @intCast(tokens[curr.* - 1].int_constant.value) }) catch unreachable;
        } else if (test_hierachy("meshes.[.primitives.[.indices")) {
            current_primitive.attributes.append(.{ ._type = .indices, .index = @intCast(tokens[curr.* - 1].int_constant.value) }) catch unreachable;
        } else if (test_hierachy("nodes.[.mesh")) {
            nodes[nodes_len] = @intCast(tokens[curr.* - 1].int_constant.value);
            nodes_len += 1;
        } else if (test_hierachy("accessors.[.bufferView")) {
            accessors[accessors_len] = @intCast(tokens[curr.* - 1].int_constant.value);
            accessors_len += 1;
        } else if (test_hierachy("bufferViews.[.byteLength")) {
            current_buffer_view.length = @intCast(tokens[curr.* - 1].int_constant.value);
        } else if (test_hierachy("bufferViews.[.byteOffset")) {
            current_buffer_view.offset = @intCast(tokens[curr.* - 1].int_constant.value);
        } else if (test_hierachy("sampler")) {
            current_channel.sampler = @intCast(tokens[curr.* - 1].int_constant.value);
        } else if (test_hierachy("target.node")) {
            current_channel.target_node = @intCast(tokens[curr.* - 1].int_constant.value);
        } else if (test_hierachy("samplers.[.input")) {
            current_sampler.input = @intCast(tokens[curr.* - 1].int_constant.value);
        } else if (test_hierachy("samplers.[.output")) {
            current_sampler.output = @intCast(tokens[curr.* - 1].int_constant.value);
        }

        return curr.*;
    }
    curr.* = temp;

    if (match(tokens, .float_constant, curr)) return curr.*;
    curr.* = temp;
    if (match(tokens, ._true, curr)) return curr.*;
    curr.* = temp;

    if (match(tokens, .l_brace, curr)) {
        if (test_hierachy("bufferViews.[")) {
            current_buffer_view = &buffer_views[buffer_views_len];
            buffer_views_len += 1;
        } else if (test_hierachy("animations.[")) {
            // std.debug.print("animation prep\n", .{});
            animations[animations_len].channels = std.ArrayList(Channel).init(global_allocator);
            animations[animations_len].samplers = std.ArrayList(Sampler).init(global_allocator);
            current_animation = &animations[animations_len];
            animations_len += 1;
        } else if (test_hierachy("animations.[.channels.[")) {
            // std.debug.print("channel prep\n", .{});
            current_animation.channels.append(undefined) catch unreachable;
            current_channel = &current_animation.channels.items[current_animation.channels.items.len - 1];
        } else if (test_hierachy("animations.[.samplers.[")) {
            current_animation.samplers.append(undefined) catch unreachable;
            current_sampler = &current_animation.samplers.items[current_animation.samplers.items.len - 1];
        }
        if (gltf_leaf_list(tokens, curr) == null) return null;
        if (!match(tokens, .r_brace, curr)) return null;

        return curr.*;
    }
    curr.* = temp;

    if (match(tokens, .l_square, curr)) {
        hierarchy.append("[") catch unreachable;
        defer _ = hierarchy.pop();
        if (gltf_leaf_list(tokens, curr) == null) return null;
        if (!match(tokens, .r_square, curr)) return null;
        return curr.*;
    }

    return null;
}

const THING = [4]i32{ 0, 0, 0, 0 };

// only supports a skinned mesh currently - normal meshes are still using old format
pub fn use_parsed_gltf(path: anytype, index: usize, shader_program_GPU: u32, texture_GPU: u32, allocator: std.mem.Allocator) !zeng.SkinnedMesh {
    const buffer = get_file_bytes(path, allocator);
    defer allocator.free(buffer);

    const accessor = meshes[nodes[index]];
    var position_data_len: usize = 0;
    var position_data_offset: usize = undefined;
    var normal_data_len: usize = 0;
    var normal_data_offset: usize = undefined;
    var texcoord_data_len: usize = 0;
    var texcoord_data_offset: usize = undefined;

    var indices_len: usize = 0;
    var indices_data_offset: usize = undefined;

    for (accessor.attributes.items) |attrib| {
        if (attrib._type == .POSITION) {
            position_data_len = buffer_views[accessors[attrib.index]].length;
            position_data_offset = buffer_views[accessors[attrib.index]].offset;
        } else if (attrib._type == .NORMAL) {
            normal_data_len = buffer_views[accessors[attrib.index]].length;
            normal_data_offset = buffer_views[accessors[attrib.index]].offset;
        } else if (attrib._type == .TEXCOORD_0) {
            texcoord_data_len = buffer_views[accessors[attrib.index]].length;
            texcoord_data_offset = buffer_views[accessors[attrib.index]].offset;
        } else if (attrib._type == .indices) {
            indices_len = buffer_views[accessors[attrib.index]].length;
            indices_data_offset = buffer_views[accessors[attrib.index]].offset;
        }
    }

    const mesh_data_size: usize = @divTrunc(position_data_len, 12) * 64; // was 32
    var mesh_data = allocator.alloc(u8, mesh_data_size) catch unreachable;
    defer allocator.free(mesh_data);

    var _curr: usize = 0;
    var _i: usize = 0;
    var _j: usize = 0;
    var _k: usize = 0;
    while (_i < position_data_len) {
        if (position_data_len > 0) {
            @memcpy(
                mesh_data[_curr .. _curr + 12],
                buffer[position_data_offset + _i .. position_data_offset + _i + 12],
            );
        } else {
            unreachable;
        }
        _i += 12;
        _curr += 12;

        if (normal_data_len > 0) {
            @memcpy(
                mesh_data[_curr .. _curr + 12],
                buffer[normal_data_offset + _j .. normal_data_offset + _j + 12],
            );
        } else {
            @memset(mesh_data[_curr .. _curr + 12], 0);
        }
        _j += 12;
        _curr += 12;

        if (texcoord_data_len > 0) {
            @memcpy(
                mesh_data[_curr .. _curr + 8],
                buffer[texcoord_data_offset + _k .. texcoord_data_offset + _k + 8],
            );
        } else {
            @memset(mesh_data[_curr .. _curr + 8], 0);
        }
        _k += 8;
        _curr += 8;

        @memcpy(mesh_data[_curr .. _curr + 16], @as([*]const u8, @ptrCast(&THING))); //@skinned
        _curr += 16;
        @memcpy(mesh_data[_curr .. _curr + 16], @as([*]const u8, @ptrCast(&[4]f32{ 1.0, 0.0, 0.0, 0.0 }))); //@skinned
        _curr += 16;
    }
    if (_i != position_data_len) unreachable;

    var index_data = allocator.alloc(u32, @divTrunc(indices_len, 2)) catch unreachable;
    defer allocator.free(index_data);

    var curr_: usize = 0;
    while (curr_ * 2 < indices_len) {
        var a: u16 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&a)), buffer[indices_data_offset + curr_ * 2 .. indices_data_offset + (curr_ + 1) * 2]);
        index_data[curr_] = @intCast(a);
        curr_ += 1;
    }

    // create VAO
    var VAO: u32 = undefined;
    zeng.gl.genVertexArrays(1, &VAO);
    zeng.gl.bindVertexArray(VAO);
    defer {
        zeng.gl.bindVertexArray(0);
        zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, 0);
        zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, 0);
    }

    // create vertex buffer object (holds vertex data) > bind it > store the data from vertices array
    var VBO: u32 = undefined;
    zeng.gl.genBuffers(1, &VBO);
    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
    defer zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, 0);
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @intCast(mesh_data.len), mesh_data.ptr, zeng.gl.STATIC_DRAW);

    // create element buffer object (indices array) > bind it > store the data from the indices array
    var EBO: u32 = undefined;
    zeng.gl.genBuffers(1, &EBO);
    zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
    zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @intCast(index_data.len * 4), index_data.ptr, zeng.gl.STATIC_DRAW);

    // data layout
    const float_count = 8 + 4 + 4;
    zeng.gl.vertexAttribPointer(0, 3, zeng.gl.FLOAT, zeng.gl.FALSE, float_count * @sizeOf(f32), @ptrFromInt(0)); // position
    zeng.gl.vertexAttribPointer(1, 3, zeng.gl.FLOAT, zeng.gl.FALSE, float_count * @sizeOf(f32), @ptrFromInt(3 * @sizeOf(f32))); // normal
    zeng.gl.vertexAttribPointer(2, 2, zeng.gl.FLOAT, zeng.gl.FALSE, float_count * @sizeOf(f32), @ptrFromInt(6 * @sizeOf(f32))); // uv
    zeng.gl.vertexAttribIPointer(3, 4, zeng.gl.INT, float_count * @sizeOf(i32), @ptrFromInt(8 * @sizeOf(f32))); // index
    zeng.gl.vertexAttribPointer(4, 4, zeng.gl.FLOAT, zeng.gl.FALSE, float_count * @sizeOf(f32), @ptrFromInt(12 * @sizeOf(f32))); // weights

    zeng.gl.enableVertexAttribArray(0);
    zeng.gl.enableVertexAttribArray(1);
    zeng.gl.enableVertexAttribArray(2);
    zeng.gl.enableVertexAttribArray(3);
    zeng.gl.enableVertexAttribArray(4);

    return zeng.SkinnedMesh{
        .indices_length = @intCast(@divTrunc(indices_len, 2)),
        .material = .{
            .shader_program_GPU = shader_program_GPU,
            .texture_GPU = texture_GPU,
        },
        .vao_gpu = VAO,
        .num_bones = 65,
    };
}
