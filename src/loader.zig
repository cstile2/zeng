const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const main = @import("main.zig");

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
    zeng.opengl_log_error() catch unreachable;

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
            // @compileError("this type cannot be serialized");
            @compileLog(@TypeOf(payload));
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
const NodeTag = enum {
    string,
    integer,
    float,
    boolean,
    object,
    array,
};
const Node = union(NodeTag) {
    string: []const u8,
    integer: i128,
    float: f64,
    boolean: bool,

    object: std.StringHashMap(*Node),
    array: std.ArrayList(*Node),
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

const context = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    curr: u64,
    temp: u64,

    tabs: u32,
};
fn match(rec: *context, tag: TokenTag) bool {
    if (rec.curr >= rec.tokens.len) {
        return false;
    }
    defer rec.curr += 1;
    if (rec.tokens[rec.curr] == tag) {
        return true;
    }
    return false;
}
fn gltf_object(rec: *context) ?*Node {
    const new = rec.allocator.create(Node) catch unreachable;
    new.* = Node{ .object = undefined };

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
fn gltf_array(rec: *context) ?*Node {
    const new = rec.allocator.create(Node) catch unreachable;
    new.* = Node{ .array = undefined };

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
fn gltf_thing(rec: *context) ?*Node {
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
        const new = rec.allocator.create(Node) catch unreachable;
        new.* = Node{ .string = rec.tokens[rec.curr - 1].string_literal.string };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, .int_constant)) {
        // std.debug.print("int constant: \"{}\"\n", .{rec.tokens[rec.curr - 1].int_constant.value});
        const new = rec.allocator.create(Node) catch unreachable;
        new.* = Node{ .integer = rec.tokens[rec.curr - 1].int_constant.value };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, .float_constant)) {
        // std.debug.print("int constant: \"{}\"\n", .{rec.tokens[rec.curr - 1].float_constant.value});
        const new = rec.allocator.create(Node) catch unreachable;
        new.* = Node{ .float = rec.tokens[rec.curr - 1].float_constant.value };
        return new;
    }

    rec.curr = rec.temp;

    if (match(rec, ._true)) {
        // std.debug.print("boolean true\n", .{});
        const new = rec.allocator.create(Node) catch unreachable;
        new.* = Node{ .boolean = true };
        return new;
    }

    // std.debug.print("OOF!\n", .{});
    return null;
}
fn gltf_name_list(rec: *context) ?std.StringHashMap(*Node) {
    var new: std.StringHashMap(*Node) = std.StringHashMap(*Node).init(rec.allocator);

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
fn gltf_nameless_list(rec: *context) ?std.ArrayList(*Node) {
    var new: std.ArrayList(*Node) = std.ArrayList(*Node).init(rec.allocator);

    while (true) {
        const thing = gltf_thing(rec);
        if (thing == null) {
            return null;
        } else {
            new.append(thing.?) catch unreachable;
            // new.put(void{}, thing.?) catch unreachable;
        }

        if (!match(rec, .comma)) {
            rec.curr -= 1;
            return new;
        }
    }
}
pub fn gltf_parse(bytes: []u8, allocator: std.mem.Allocator) ?*Node {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();
    lexer(bytes, &tokens) catch unreachable;

    var rec: context = .{ .allocator = allocator, .tokens = tokens.items, .curr = 0, .temp = undefined, .tabs = 0 };

    return gltf_object(&rec);
}

const SceneNodeType = enum {
    static_mesh,
    skinned_mesh,
    empty,
};
const SceneNode = union(SceneNodeType) {
    static_mesh: zeng.mesh,
    skinned_mesh: zeng.skinned_mesh,
    empty: void,
};
const SceneNodeWithMatrix = struct {
    node: SceneNode,
    matrix: [16]f32,
    gltf_id: usize = 0,
};
const ChannelOutputDataTag = enum {
    rotation,
    translation,
    scale,
};
pub const ChannelOutputData = union(ChannelOutputDataTag) {
    rotation: []zeng.quat,
    translation: []zeng.vec3,
    scale: []zeng.vec3,
};
const SamplerData = struct {
    inputs: []f32,
    output: ChannelOutputData,
};
pub const AnimationChannel = struct {
    target: usize,
    inputs: []f32,
    outputs: ChannelOutputData,
};
pub const Animation = struct {
    channels: []AnimationChannel,
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
        else => 1,
    };
}
fn get_offsest_and_length(accessor_index: usize, accessors: Node, bufferviews: Node) struct { usize, usize, usize } {
    const bv_index: usize = @intCast(accessors.array.items[accessor_index].object.get("bufferView").?.integer);
    const offset: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteOffset").?.integer);
    const length: usize = @intCast(bufferviews.array.items[bv_index].object.get("byteLength").?.integer);
    const component_type: usize = @intCast(accessors.array.items[accessor_index].object.get("componentType").?.integer);
    return .{ offset, length, component_type };
}
fn get_float_from_numeric(n: *Node, idx: comptime_int) f32 {
    if (n.array.items[idx].* == .float)
        return @floatCast(n.array.items[idx].float);

    return @floatFromInt(n.array.items[idx].integer);
}

pub var global_mesh_verts: []zeng.vec3 = undefined;
pub var global_mesh_indices: []u16 = undefined;

pub fn gltf_extract_resources(root_n: ?*Node, bin_path: []const u8, dependencies_path: []const u8, allocator: std.mem.Allocator, skin_shader_program: u32, static_shader_program: u32, default_texture: u32) struct { []SceneNodeWithMatrix, []Animation, []zeng.skeleton, std.AutoArrayHashMap(usize, std.ArrayList(usize)) } {
    const bin_data = zeng.get_file_bytes(bin_path, allocator);

    // var top_level_objects = std.AutoHashMap(usize, void).init(allocator);
    // for (root_n.?.object.get("scenes").?.array.items) |scene_n| {
    //     for (scene_n.object.get("nodes").?.array.items) |node_n| {
    //         top_level_objects.put(@intCast(node_n.integer), void{}) catch unreachable;
    //     }
    // }
    var node_to_skin = std.AutoHashMap(usize, usize).init(allocator);

    var skeleton_space_maps = std.ArrayList(std.AutoHashMap(usize, usize)).init(allocator);
    var result_nodes = std.ArrayList(SceneNodeWithMatrix).init(allocator);
    var result_animations = std.ArrayList(Animation).init(allocator);
    var result_skeletons = std.ArrayList(zeng.skeleton).init(allocator);
    var result_children_list = std.AutoArrayHashMap(usize, std.ArrayList(usize)).init(allocator);

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
                node_to_skin.put(@intCast(joint_n.integer), current_skin_num) catch unreachable;
            }

            const temp_bone_parent_indices = allocator.alloc(isize, bone_counter) catch unreachable;
            @memset(temp_bone_parent_indices, -1);

            const temp_inverse_bind_matrices = allocator.alloc(zeng.world_matrix, bone_counter) catch unreachable;
            const offset: usize, const length: usize, _ = get_offsest_and_length(@intCast(current_skin_n.object.get("inverseBindMatrices").?.integer), accessors_n.*, bufferviews_n.*);
            @memcpy(@as([*]u8, @ptrCast(temp_inverse_bind_matrices)), bin_data[offset .. offset + length]);

            skeleton_space_maps.append(jointspace_to_nodespace) catch unreachable;
            result_skeletons.append(.{ .inverse_bind_matrices = temp_inverse_bind_matrices, .bone_parent_indices = temp_bone_parent_indices, .local_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable, .model_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable, .animations = std.ArrayList(usize).init(allocator) }) catch unreachable;
        }
    }
    if (_animations_n != null) {
        for (_animations_n.?.array.items) |current_animation| {
            var temp_channels = std.ArrayList(AnimationChannel).init(allocator);

            const channels_n = current_animation.object.get("channels");
            const samplers_n = current_animation.object.get("samplers");
            if (channels_n == null or samplers_n == null) break;

            var max_timestamp: f32 = 0.0;
            var owner_skin: usize = 0;
            for (channels_n.?.array.items) |channel| {
                const sampler = samplers_n.?.array.items[@intCast(channel.object.get("sampler").?.integer)];

                const input_accessor_index: usize = @intCast(sampler.object.get("input").?.integer);
                const output_accessor_index: usize = @intCast(sampler.object.get("output").?.integer);
                const input_offset, const input_length, _ = get_offsest_and_length(input_accessor_index, accessors_n.*, bufferviews_n.*);
                const output_offset, const output_length, _ = get_offsest_and_length(output_accessor_index, accessors_n.*, bufferviews_n.*);

                var target_index: usize = @intCast(channel.object.get("target").?.object.get("node").?.integer);
                owner_skin = node_to_skin.get(target_index).?;
                target_index = skeleton_space_maps.items[owner_skin].get(target_index).?; // REMAP from node space to a  skin[0] bone TODO: make this use more than just the first skin in the file

                const temp_inputs: []f32 = allocator.alloc(f32, input_length / 4) catch unreachable;
                @memcpy(@as([*]u8, @ptrCast(temp_inputs)), bin_data[input_offset .. input_offset + input_length]);

                const target_path = channel.object.get("target").?.object.get("path").?.string;
                var output_type: ChannelOutputDataTag = undefined;
                if (std.mem.eql(u8, target_path, "rotation")) {
                    output_type = .rotation;
                } else if (std.mem.eql(u8, target_path, "translation")) {
                    output_type = .translation;
                } else if (std.mem.eql(u8, target_path, "scale")) {
                    output_type = .scale;
                } else unreachable;

                var temp_outputs: ChannelOutputData = undefined;
                if (output_type == .rotation) {
                    temp_outputs = ChannelOutputData{ .rotation = allocator.alloc(zeng.quat, output_length / 16) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.rotation)), bin_data[output_offset .. output_offset + output_length]);
                } else if (output_type == .translation) {
                    temp_outputs = ChannelOutputData{ .translation = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.translation)), bin_data[output_offset .. output_offset + output_length]);
                } else if (output_type == .scale) {
                    temp_outputs = ChannelOutputData{ .scale = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };
                    @memcpy(@as([*]u8, @ptrCast(temp_outputs.scale)), bin_data[output_offset .. output_offset + output_length]);
                } else unreachable;

                for (temp_inputs) |f| {
                    max_timestamp = @max(max_timestamp, f);
                }

                temp_channels.append(AnimationChannel{
                    .target = target_index,
                    .inputs = temp_inputs,
                    .outputs = temp_outputs,
                }) catch unreachable;
            }

            result_animations.append(Animation{ .channels = temp_channels.items, .duration = max_timestamp }) catch unreachable;
            result_skeletons.items[owner_skin].animations.append(result_animations.items.len - 1) catch unreachable;
        }
    }

    var current_node_index: usize = 0;
    for (nodes_n.array.items) |current_node_n| {
        defer current_node_index += 1;

        const children = current_node_n.object.get("children");
        if (children != null) {
            var entry = std.ArrayList(usize).init(allocator);
            children_blk: for (children.?.array.items) |child| {
                // test is any skin contains BOTH the child and the parent - otherwise add children to the hierarchy
                for (skeleton_space_maps.items, 0..) |skin, s| {
                    if (skin.contains(current_node_index) and skin.contains(@intCast(child.integer))) { // this is an armature connection - don't add it to the global children hierarchy - add it inside the skeleton
                        result_skeletons.items[s].bone_parent_indices[skin.get(@intCast(child.integer)).?] = @intCast(skin.get(current_node_index).?);
                        continue :children_blk;
                    }
                }
                entry.append(@intCast(child.integer)) catch unreachable;
            }

            if (entry.items.len > 0) {
                result_children_list.put(current_node_index, entry) catch unreachable;
            } else entry.deinit();
        }

        const mesh_index_n = current_node_n.object.get("mesh");
        const skin_index_n = current_node_n.object.get("skin");
        if (mesh_index_n != null and skin_index_n != null) {
            var base_color_texture_gpu: u32 = default_texture;

            const mesh_n = root_n.?.object.get("meshes").?.array.items[@intCast(mesh_index_n.?.integer)];
            const primitive_n = mesh_n.object.get("primitives").?.array.items[0];
            const attributes_n = primitive_n.object.get("attributes").?;
            if (_textures_n != null and _images_n != null) {
                const material_index: usize = @intCast(primitive_n.object.get("material").?.integer);
                const material_n = root_n.?.object.get("materials").?.array.items[material_index];
                const base_color_texture_index: usize = @intCast(material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture").?.object.get("index").?.integer);
                const base_color_texture_image_index: usize = @intCast(_textures_n.?.array.items[base_color_texture_index].object.get("source").?.integer);
                const base_color_texture_image_str = _images_n.?.array.items[base_color_texture_image_index].object.get("uri").?.string;
                base_color_texture_gpu = zeng.load_texture(zeng.concat_as_null_terminated(dependencies_path, base_color_texture_image_str, allocator), true, false);
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
            const mat = zeng.mat_tran(zeng.mat_mult(zeng.quat_to_mat(rotation), zeng.mat_scal(zeng.mat_identity, scale)), translation);

            const position_data_offset: usize, const position_data_len: usize, const position_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("POSITION").?.integer), accessors_n.*, bufferviews_n.*);
            const position_component_size = get_component_type_size(position_component_type);

            const normal_data_offset: usize, const normal_data_len: usize, const normal_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("NORMAL").?.integer), accessors_n.*, bufferviews_n.*);
            const normal_component_size = get_component_type_size(normal_component_type);

            const texcoord_data_offset: usize, const texcoord_data_len: usize, const texcoord_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("TEXCOORD_0").?.integer), accessors_n.*, bufferviews_n.*);
            const texcoord_component_size = get_component_type_size(texcoord_component_type);

            const joints_data_offset: usize, const joints_data_len: usize, const joints_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("JOINTS_0").?.integer), accessors_n.*, bufferviews_n.*);
            const joints_component_size = get_component_type_size(joints_component_type);

            const weights_data_offset: usize, const weights_data_len: usize, const weights_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("WEIGHTS_0").?.integer), accessors_n.*, bufferviews_n.*);
            const weights_component_size = get_component_type_size(weights_component_type);

            const indices_data_offset: usize, const indices_data_len: usize, const indices_component_type: usize = get_offsest_and_length(@intCast(primitive_n.object.get("indices").?.integer), accessors_n.*, bufferviews_n.*);
            const indices_component_size = get_component_type_size(indices_component_type);

            const mesh_data_size: usize = (position_data_len / position_component_size) * (3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size + 4 * joints_component_size + 4 * weights_component_size);
            var mesh_data = allocator.alloc(u8, mesh_data_size) catch unreachable;

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
                        bin_data[position_data_offset + _i .. position_data_offset + _i + 3 * position_component_size],
                    );
                } else {
                    unreachable;
                }
                _i += 3 * position_component_size;
                _curr += 3 * position_component_size;

                if (normal_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 3 * normal_component_size],
                        bin_data[normal_data_offset + _j .. normal_data_offset + _j + 3 * normal_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 3 * normal_component_size], 0);
                }
                _j += 3 * normal_component_size;
                _curr += 3 * normal_component_size;

                if (texcoord_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 2 * texcoord_component_size],
                        bin_data[texcoord_data_offset + _k .. texcoord_data_offset + _k + 2 * texcoord_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 2 * texcoord_component_size], 0);
                }
                _k += 2 * texcoord_component_size;
                _curr += 2 * texcoord_component_size;

                if (joints_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 4 * joints_component_size],
                        bin_data[joints_data_offset + _l .. joints_data_offset + _l + 4 * joints_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 4 * joints_component_size], 0);
                }
                _l += 4 * joints_component_size;
                _curr += 4 * joints_component_size;

                if (weights_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 4 * weights_component_size],
                        bin_data[weights_data_offset + _m .. weights_data_offset + _m + 4 * weights_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 4 * weights_component_size], 0);
                }
                _m += 4 * weights_component_size;
                _curr += 4 * weights_component_size;
            }
            if (_i != position_data_len) unreachable;

            const index_data = allocator.alloc(u8, indices_data_len) catch unreachable;

            @memcpy(@as([*]u8, @ptrCast(index_data)), bin_data[indices_data_offset .. indices_data_offset + indices_data_len]);

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

            const skin_index: usize = @intCast(skin_index_n.?.integer);
            result_nodes.append(SceneNodeWithMatrix{
                .node = SceneNode{
                    .skinned_mesh = zeng.skinned_mesh{
                        .indices_length = @intCast(indices_data_len / indices_component_size),
                        .indices_type = get_component_type_enum(indices_component_type),
                        .material = .{
                            .shader_program = skin_shader_program,
                            .texture = base_color_texture_gpu,
                        },
                        .vao_gpu = VAO,
                        .skeleton = &result_skeletons.items[skin_index],
                    },
                },
                .matrix = mat,
                .gltf_id = current_node_index,
            }) catch unreachable;
        } else if (mesh_index_n != null) {
            const name = current_node_n.object.get("name");

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
            if (name != null and std.mem.eql(u8, name.?.string, "C\\u00edrculo.017")) {
                // std.debug.print("{} {} {}\n", .{ translation, scale, rotation });
                std.debug.print("{any}\n", .{mat});
            }

            const mesh_n = root_n.?.object.get("meshes").?.array.items[@intCast(mesh_index_n.?.integer)];
            for (mesh_n.object.get("primitives").?.array.items) |primitive_n| {
                var base_color_texture_gpu: u32 = default_texture;
                const attributes_n = primitive_n.object.get("attributes").?;
                if (primitive_n.object.get("material") != null and _textures_n != null and _images_n != null) {
                    const material_index: usize = @intCast(primitive_n.object.get("material").?.integer);
                    const material_n = root_n.?.object.get("materials").?.array.items[material_index];
                    const base_color_texture_index: usize = @intCast(material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture").?.object.get("index").?.integer);
                    const base_color_texture_image_index: usize = @intCast(_textures_n.?.array.items[base_color_texture_index].object.get("source").?.integer);
                    const base_color_texture_image_str = _images_n.?.array.items[base_color_texture_image_index].object.get("uri").?.string;
                    // std.debug.print("image: {s}\n", .{base_color_texture_image_str});
                    base_color_texture_gpu = zeng.load_texture(zeng.concat_as_null_terminated(dependencies_path, base_color_texture_image_str, allocator), true, false);
                }

                const position_data_offset: usize, const position_data_len: usize, const position_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("POSITION").?.integer), accessors_n.*, bufferviews_n.*);
                const position_component_size = get_component_type_size(position_component_type);

                const normal_data_offset: usize, const normal_data_len: usize, const normal_component_type: usize = get_offsest_and_length(@intCast(attributes_n.object.get("NORMAL").?.integer), accessors_n.*, bufferviews_n.*);
                const normal_component_size = get_component_type_size(normal_component_type);

                var texcoord_data_offset: usize, var texcoord_data_len: usize, var texcoord_component_type: usize = .{ 0, 0, 5126 };
                if (attributes_n.object.get("TEXCOORD_0") != null)
                    texcoord_data_offset, texcoord_data_len, texcoord_component_type = get_offsest_and_length(@intCast(attributes_n.object.get("TEXCOORD_0").?.integer), accessors_n.*, bufferviews_n.*);
                const texcoord_component_size = get_component_type_size(texcoord_component_type);

                const indices_data_offset: usize, const indices_data_len: usize, const indices_component_type: usize = get_offsest_and_length(@intCast(primitive_n.object.get("indices").?.integer), accessors_n.*, bufferviews_n.*);
                const indices_component_size = get_component_type_size(indices_component_type);

                const mesh_data_size: usize = (position_data_len / position_component_size) * (3 * position_component_size + 3 * normal_component_size + 2 * texcoord_component_size);
                var mesh_data = allocator.alloc(u8, mesh_data_size) catch unreachable;

                if (name != null and std.mem.eql(u8, name.?.string, "groundly")) {
                    if (indices_component_size != 2) unreachable;
                    global_mesh_verts = allocator.alloc(zeng.vec3, position_data_len / 12) catch unreachable;
                    @memcpy(@as([*]u8, @ptrCast(global_mesh_verts)), bin_data[position_data_offset .. position_data_offset + position_data_len]);
                    global_mesh_indices = allocator.alloc(u16, indices_data_len / 2) catch unreachable;
                    @memcpy(@as([*]u8, @ptrCast(global_mesh_indices)), bin_data[indices_data_offset .. indices_data_offset + indices_data_len]);
                    std.debug.print("TRIS: {}\n", .{global_mesh_indices.len / 3});
                }

                var _curr: usize = 0;
                var _i: usize = 0;
                var _j: usize = 0;
                var _k: usize = 0;
                while (_i < position_data_len) {
                    if (position_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 3 * position_component_size],
                            bin_data[position_data_offset + _i .. position_data_offset + _i + 3 * position_component_size],
                        );
                    } else {
                        unreachable;
                    }
                    _i += 3 * position_component_size;
                    _curr += 3 * position_component_size;

                    if (normal_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 3 * normal_component_size],
                            bin_data[normal_data_offset + _j .. normal_data_offset + _j + 3 * normal_component_size],
                        );
                    } else {
                        @memset(mesh_data[_curr .. _curr + 3 * normal_component_size], 0);
                    }
                    _j += 3 * normal_component_size;
                    _curr += 3 * normal_component_size;

                    if (texcoord_data_len > 0) {
                        @memcpy(
                            mesh_data[_curr .. _curr + 2 * texcoord_component_size],
                            bin_data[texcoord_data_offset + _k .. texcoord_data_offset + _k + 2 * texcoord_component_size],
                        );
                    } else {
                        @memset(mesh_data[_curr .. _curr + 2 * texcoord_component_size], 0);
                    }
                    _k += 2 * texcoord_component_size;
                    _curr += 2 * texcoord_component_size;
                }
                if (_i != position_data_len) unreachable;

                const index_data = allocator.alloc(u8, indices_data_len) catch unreachable;

                @memcpy(@as([*]u8, @ptrCast(index_data)), bin_data[indices_data_offset .. indices_data_offset + indices_data_len]);

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

                result_nodes.append(SceneNodeWithMatrix{
                    .node = SceneNode{
                        .static_mesh = zeng.mesh{
                            .indices_length = @intCast(indices_data_len / indices_component_size),
                            .indices_type = get_component_type_enum(indices_component_type),
                            .material = .{
                                .shader_program = static_shader_program,
                                .texture = base_color_texture_gpu,
                            },
                            .vao_gpu = VAO,
                        },
                    },
                    .matrix = mat,
                    .gltf_id = current_node_index,
                }) catch unreachable;
            }
        } else if (current_node_n.object.get("children") != null) {
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

            result_nodes.append(SceneNodeWithMatrix{
                .node = SceneNode{
                    .empty = void{},
                },
                .matrix = mat,
                .gltf_id = current_node_index,
            }) catch unreachable;
        }
    }

    return .{ result_nodes.items, result_animations.items, result_skeletons.items, result_children_list };
}
pub fn instantiate(mesh_slice: []SceneNodeWithMatrix, parent_child_map: std.AutoArrayHashMap(usize, std.ArrayList(usize)), world: *ecs.world, ctx: *zeng.engine_context) ecs.entity_id {
    var node_mapping = std.AutoArrayHashMap(usize, ecs.entity_id).init(ctx.arena_allocator);
    var top_level_children = std.ArrayList(ecs.entity_id).init(ctx.arena_allocator);
    for (mesh_slice) |mesh_like| {
        if (mesh_like.node == .skinned_mesh) {
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                main.local_matrix{ .transform = mesh_like.matrix },
                mesh_like.node.skinned_mesh,
            });
            top_level_children.append(entity_id) catch unreachable;
            node_mapping.put(mesh_like.gltf_id, entity_id) catch unreachable;
        } else if (mesh_like.node == .static_mesh) {
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                main.local_matrix{ .transform = mesh_like.matrix },
                mesh_like.node.static_mesh,
            });
            top_level_children.append(entity_id) catch unreachable;
            node_mapping.put(mesh_like.gltf_id, entity_id) catch unreachable;
        } else if (mesh_like.node == .empty) {
            std.debug.print("empty node spawned!\n", .{});
            const entity_id = world.spawn(.{
                zeng.mat_identity,
                main.local_matrix{ .transform = mesh_like.matrix },
            });
            top_level_children.append(entity_id) catch unreachable;
            node_mapping.put(mesh_like.gltf_id, entity_id) catch unreachable;
        }
    }

    var non_top_level_entities = std.AutoHashMap(ecs.entity_id, void).init(ctx.arena_allocator);
    for (parent_child_map.keys(), parent_child_map.values()) |parent, children| {
        if (node_mapping.get(parent)) |parent_e_id| {
            const children_slice_component = ctx.arena_allocator.alloc(ecs.entity_id, children.items.len) catch unreachable;

            for (0.., children.items) |idx, child| {
                const child_e_id = node_mapping.get(child).?;
                children_slice_component[idx] = child_e_id;
                non_top_level_entities.put(child_e_id, void{}) catch unreachable;
            }

            world.add(main.children{ .items = children_slice_component }, parent_e_id);
        }
    }
    var revised_top_level_children = std.ArrayList(ecs.entity_id).init(ctx.arena_allocator);
    for (top_level_children.items) |top_level_child| {
        if (!non_top_level_entities.contains(top_level_child)) {
            revised_top_level_children.append(top_level_child) catch unreachable;
        }
    }

    const model_root = world.spawn(.{
        zeng.mat_identity,
        main.children{ .items = revised_top_level_children.items },
    });

    return model_root;
}
pub fn auto_import(ctx: *zeng.engine_context, world: *ecs.world, folder_name: anytype, file_name: anytype, skin_shader: u32, static_shader: u32, uv_checker_tex: u32) ecs.entity_id {
    const buffer_0 = ctx.arena_allocator.alloc(u8, folder_name.len + file_name.len + 5) catch unreachable;
    const full_file_path = std.fmt.bufPrint(buffer_0, "{s}{s}.gltf", .{ folder_name, file_name }) catch unreachable;
    const buffer_1 = ctx.arena_allocator.alloc(u8, folder_name.len + file_name.len + 4) catch unreachable;
    const full_bin_path = std.fmt.bufPrint(buffer_1, "{s}{s}.bin", .{ folder_name, file_name }) catch unreachable;

    const mesh_slice, const animation_slice, const skeleton_slice, const parent_child_map = zeng.gltf_extract_resources(zeng.gltf_parse(zeng.get_file_bytes(full_file_path, ctx.arena_allocator), ctx.arena_allocator), full_bin_path, folder_name, ctx.arena_allocator, skin_shader, static_shader, uv_checker_tex);
    _ = skeleton_slice; // autofix
    _ = animation_slice; // autofix
    const model_root = zeng.instantiate(mesh_slice, parent_child_map, world, ctx);
    return model_root;
}
