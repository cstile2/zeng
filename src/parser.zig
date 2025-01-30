const std = @import("std");
const zeng = @import("zeng.zig");
const ecs = @import("ecs.zig");
const ECS = @import("main.zig").ECS;

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

const RecursiveInformation = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    curr: u64,
    temp: u64,

    tabs: u32,
};

fn match(rec: *RecursiveInformation, tag: TokenTag) bool {
    if (rec.curr >= rec.tokens.len) {
        return false;
    }
    defer rec.curr += 1;
    if (rec.tokens[rec.curr] == tag) {
        return true;
    }
    return false;
}
fn gltf_object(rec: *RecursiveInformation) ?*Node {
    rec_print(rec, "start object\n", true);
    defer rec_print(rec, "end object\n", false);
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
fn gltf_array(rec: *RecursiveInformation) ?*Node {
    rec_print(rec, "start array\n", true);
    defer rec_print(rec, "end array\n", false);
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
fn gltf_thing(rec: *RecursiveInformation) ?*Node {
    // rec_print(rec, "start thing\n", true);
    // defer rec_print(rec, "end thing\n", false);

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
fn gltf_name_list(rec: *RecursiveInformation) ?std.StringHashMap(*Node) {
    rec_print(rec, "start name_list\n", true);
    defer rec_print(rec, "end name_list\n", false);
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
fn gltf_nameless_list(rec: *RecursiveInformation) ?std.ArrayList(*Node) {
    rec_print(rec, "start nameless_list\n", true);
    defer rec_print(rec, "end nameless_list\n", false);
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
pub fn parse_gltf(bytes: []u8, allocator: std.mem.Allocator) ?*Node {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();
    lexer(bytes, &tokens) catch unreachable;

    var rec: RecursiveInformation = .{ .allocator = allocator, .tokens = tokens.items, .curr = 0, .temp = undefined, .tabs = 0 };

    return gltf_object(&rec);
}

pub fn rec_print(rec: *RecursiveInformation, comptime string: anytype, inc: bool) void {
    _ = rec; // autofix
    _ = string; // autofix
    _ = inc; // autofix
    // if (inc)
    //     rec.tabs += 1;
    // defer {
    //     if (!inc)
    //         rec.tabs -= 1;
    // }
    // var i: u32 = 0;
    // while (i < rec.tabs) {
    //     std.debug.print("   ", .{});
    //     i += 1;
    // }
    // std.debug.print(string, .{});
}

const MeshLikeType = enum {
    static,
    skinned,
};
const MeshLike = union(MeshLikeType) {
    static: zeng.mesh,
    skinned: zeng.skinned_mesh,
};

const Skin = struct {
    inverse_bind_matrices: []zeng.world_matrix,
    mapping: std.AutoHashMap(usize, usize),
    bone_parent_indices: []isize,
};
// skeleton will contain - parentIndices, inverseBindMatrices, localBoneMatrices, globalBoneMatrices(used for sockets)

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

pub fn extract(root_n: ?*Node, file_path: []const u8, dependencies_path: []const u8, allocator: std.mem.Allocator, shader_program_GPU: u32) struct { []MeshLike, []Animation, []zeng.skeleton } {
    const file_data = zeng.get_file_bytes(file_path, allocator);

    var result_meshes = std.AutoArrayHashMap(usize, MeshLike).init(allocator);
    var result_animations = std.ArrayList(Animation).init(allocator);
    var result_skins = std.ArrayList(Skin).init(allocator);
    var result_skeletons = std.ArrayList(zeng.skeleton).init(allocator);

    const nodes_n = root_n.?.object.get("nodes").?;
    const accessors_n = root_n.?.object.get("accessors").?;
    const bufferviews_n = root_n.?.object.get("bufferViews").?;
    const animations_n = root_n.?.object.get("animations");
    const skins_n = root_n.?.object.get("skins").?;
    const textures_n = root_n.?.object.get("textures").?;
    const images_n = root_n.?.object.get("images").?;

    for (skins_n.array.items) |current_skin_n| {
        var temp_mapping = std.AutoHashMap(usize, usize).init(allocator);

        var bone_counter: usize = 0;
        for (current_skin_n.object.get("joints").?.array.items) |joint_n| {
            defer bone_counter += 1;
            temp_mapping.put(@intCast(joint_n.integer), bone_counter) catch unreachable;
        }

        const temp_bone_parent_indices = allocator.alloc(isize, bone_counter) catch unreachable;
        @memset(temp_bone_parent_indices, -1);

        const temp_inverse_bind_matrices = allocator.alloc(zeng.world_matrix, bone_counter) catch unreachable;
        const offset: usize, const length: usize, _ = get_offsest_and_length(@intCast(current_skin_n.object.get("inverseBindMatrices").?.integer), accessors_n.*, bufferviews_n.*);
        @memcpy(@as([*]u8, @ptrCast(temp_inverse_bind_matrices)), file_data[offset .. offset + length]);

        result_skins.append(Skin{ .mapping = temp_mapping, .inverse_bind_matrices = temp_inverse_bind_matrices, .bone_parent_indices = temp_bone_parent_indices }) catch unreachable;
        result_skeletons.append(.{ .inverse_bind_matrices = temp_inverse_bind_matrices, .bone_parent_indices = temp_bone_parent_indices, .local_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable, .model_bone_matrices = allocator.alloc(zeng.world_matrix, temp_bone_parent_indices.len) catch unreachable }) catch unreachable;
    }

    var node_num: usize = 0;
    for (nodes_n.array.items) |current_node_n| {
        defer node_num += 1;
        const children = current_node_n.object.get("children");
        if (children != null) {
            for (children.?.array.items) |child| {
                for (result_skins.items) |skin| {
                    if (skin.mapping.contains(node_num) and skin.mapping.contains(@intCast(child.integer))) {
                        skin.bone_parent_indices[skin.mapping.get(@intCast(child.integer)).?] = @intCast(skin.mapping.get(node_num).?);
                    }
                }
            }
        }

        const mesh_index_n = current_node_n.object.get("mesh");
        const skin_index_n = current_node_n.object.get("skin");
        if (mesh_index_n != null and skin_index_n != null) {
            const mesh_n = root_n.?.object.get("meshes").?.array.items[@intCast(mesh_index_n.?.integer)];
            const primitive_n = mesh_n.object.get("primitives").?.array.items[0];
            const attributes_n = primitive_n.object.get("attributes").?;
            const material_index: usize = @intCast(primitive_n.object.get("material").?.integer);
            const material_n = root_n.?.object.get("materials").?.array.items[material_index];
            const base_color_texture_index: usize = @intCast(material_n.object.get("pbrMetallicRoughness").?.object.get("baseColorTexture").?.object.get("index").?.integer);
            const base_color_texture_image_index: usize = @intCast(textures_n.array.items[base_color_texture_index].object.get("source").?.integer);
            const base_color_texture_image_str = images_n.array.items[base_color_texture_image_index].object.get("uri").?.string;
            const base_color_texture_gpu = zeng.load_texture(zeng.concat_null(dependencies_path, base_color_texture_image_str, allocator), true, false);

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
                        file_data[position_data_offset + _i .. position_data_offset + _i + 3 * position_component_size],
                    );
                } else {
                    unreachable;
                }
                _i += 3 * position_component_size;
                _curr += 3 * position_component_size;

                if (normal_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 3 * normal_component_size],
                        file_data[normal_data_offset + _j .. normal_data_offset + _j + 3 * normal_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 3 * normal_component_size], 0);
                }
                _j += 3 * normal_component_size;
                _curr += 3 * normal_component_size;

                if (texcoord_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 2 * texcoord_component_size],
                        file_data[texcoord_data_offset + _k .. texcoord_data_offset + _k + 2 * texcoord_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 2 * texcoord_component_size], 0);
                }
                _k += 2 * texcoord_component_size;
                _curr += 2 * texcoord_component_size;

                if (joints_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 4 * joints_component_size],
                        file_data[joints_data_offset + _l .. joints_data_offset + _l + 4 * joints_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 4 * joints_component_size], 0);
                }
                _l += 4 * joints_component_size;
                _curr += 4 * joints_component_size;

                if (weights_data_len > 0) {
                    @memcpy(
                        mesh_data[_curr .. _curr + 4 * weights_component_size],
                        file_data[weights_data_offset + _m .. weights_data_offset + _m + 4 * weights_component_size],
                    );
                } else {
                    @memset(mesh_data[_curr .. _curr + 4 * weights_component_size], 0);
                }
                _m += 4 * weights_component_size;
                _curr += 4 * weights_component_size;
            }
            if (_i != position_data_len) unreachable;

            var index_data = allocator.alloc(u32, indices_data_len / indices_component_size) catch unreachable;

            var curr_: usize = 0;
            while (curr_ * indices_component_size < indices_data_len) {
                var a: u16 = undefined;
                @memcpy(@as([*]u8, @ptrCast(&a)), file_data[indices_data_offset + curr_ * indices_component_size .. indices_data_offset + (curr_ + 1) * indices_component_size]);
                index_data[curr_] = @intCast(a);
                curr_ += 1;
            }

            // create VAO
            var VAO: u32 = undefined;
            zeng.gl.genVertexArrays(1, &VAO);
            zeng.gl.bindVertexArray(VAO);

            // create vertex buffer object (holds vertex data) > bind it > store the data from vertices array
            var VBO: u32 = undefined;
            zeng.gl.genBuffers(1, &VBO);
            zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, VBO);
            zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @intCast(mesh_data.len), mesh_data.ptr, zeng.gl.STATIC_DRAW);

            // create element buffer object (indices array) > bind it > store the data from the indices array
            var EBO: u32 = undefined;
            zeng.gl.genBuffers(1, &EBO);
            zeng.gl.bindBuffer(zeng.gl.ELEMENT_ARRAY_BUFFER, EBO);
            zeng.gl.bufferData(zeng.gl.ELEMENT_ARRAY_BUFFER, @intCast(index_data.len * 4), index_data.ptr, zeng.gl.STATIC_DRAW);

            // data layout
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
            result_meshes.put(node_num, MeshLike{
                .skinned = zeng.skinned_mesh{
                    .indices_length = @intCast(indices_data_len / indices_component_size),
                    .material = .{
                        .shader_program_GPU = shader_program_GPU,
                        .texture_GPU = base_color_texture_gpu,
                    },
                    .vao_gpu = VAO,
                    // .bone_parent_indices = result_skins.items[skin_index].bone_parent_indices,
                    // .inverse_bind_matrices = result_skins.items[skin_index].inverse_bind_matrices,
                    .skeleton = &result_skeletons.items[skin_index],
                },
            }) catch unreachable;
        }
    }

    if (animations_n != null) {
        for (animations_n.?.array.items) |current_animation| {
            var temp_channels = std.ArrayList(AnimationChannel).init(allocator);

            const channels_n = current_animation.object.get("channels");
            const samplers_n = current_animation.object.get("samplers");
            if (channels_n == null or samplers_n == null) break;

            var max_timestamp: f32 = 0.0;

            for (channels_n.?.array.items) |channel| {
                const sampler = samplers_n.?.array.items[@intCast(channel.object.get("sampler").?.integer)];

                const input_accessor_index: usize = @intCast(sampler.object.get("input").?.integer);
                const output_accessor_index: usize = @intCast(sampler.object.get("output").?.integer);
                const input_offset, const input_length, _ = get_offsest_and_length(input_accessor_index, accessors_n.*, bufferviews_n.*);
                const output_offset, const output_length, _ = get_offsest_and_length(output_accessor_index, accessors_n.*, bufferviews_n.*);

                var target_n: usize = @intCast(channel.object.get("target").?.object.get("node").?.integer);
                target_n = result_skins.items[0].mapping.get(target_n).?; // REMAP to work with the (first) skin

                const temp_inputs: []f32 = allocator.alloc(f32, input_length / 4) catch unreachable;
                @memcpy(@as([*]u8, @ptrCast(temp_inputs)), file_data[input_offset .. input_offset + input_length]);

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

                    var c: usize = 0;
                    for (temp_outputs.rotation) |*output_quat| {
                        @memcpy(@as(*[16]u8, @ptrCast(output_quat)), file_data[output_offset + c .. output_offset + c + 16]);
                        c += 16;
                    }
                    // @memcpy(@as([*]u8, @ptrCast(temp_outputs.rotation)), file_data[output_offset .. output_offset + output_length]);
                } else if (output_type == .translation) {
                    temp_outputs = ChannelOutputData{ .translation = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };

                    var c: usize = 0;
                    for (temp_outputs.translation) |*output_vec3| {
                        @memcpy(@as(*[12]u8, @ptrCast(output_vec3)), file_data[output_offset + c .. output_offset + c + 12]);
                        c += 12;
                    }
                    // @memcpy(@as([*]u8, @ptrCast(temp_outputs.translation)), file_data[output_offset .. output_offset + output_length]);
                } else if (output_type == .scale) {
                    temp_outputs = ChannelOutputData{ .scale = allocator.alloc(zeng.vec3, output_length / 12) catch unreachable };

                    var c: usize = 0;
                    for (temp_outputs.scale) |*output_vec3| {
                        @memcpy(@as(*[12]u8, @ptrCast(output_vec3)), file_data[output_offset + c .. output_offset + c + 12]);
                        c += 12;
                    }
                    // @memcpy(@as([*]u8, @ptrCast(temp_outputs.scale)), file_data[output_offset .. output_offset + output_length]);
                } else unreachable;

                for (temp_inputs) |f| {
                    max_timestamp = @max(max_timestamp, f);
                }

                temp_channels.append(AnimationChannel{
                    .target = target_n,
                    .inputs = temp_inputs,
                    .outputs = temp_outputs,
                }) catch unreachable;
            }

            result_animations.append(Animation{ .channels = temp_channels.items, .duration = max_timestamp }) catch unreachable;
        }
    }

    return .{ result_meshes.values(), result_animations.items, result_skeletons.items };
}
