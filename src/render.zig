const zeng = @import("zeng.zig");
const std = @import("std");
const ecs = @import("ecs.zig");

pub fn draw_text(string: []const u8, ui_ren: *@import("main.zig").text_render_res, x: f32, y: f32) void {
    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    defer zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ui_ren.texture);

    zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "dims"), 0.02, 0.05);

    var horizontal: usize = 0;
    for (string) |char| {
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos"), @as(f32, @floatFromInt(horizontal)) * 0.038 + x, y);
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "image_point"), @as(f32, @floatFromInt((char - 32) % 16)), @as(f32, @floatFromInt((char - 32) / 16)));
        zeng.gl.drawElements(zeng.gl.TRIANGLES, ui_ren.indices_len, zeng.gl.UNSIGNED_INT, null);
        horizontal += 1;
    }
}
pub fn draw_mesh(entity_mesh: zeng.mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    // use shader program > bind VAO > bind texture
    zeng.gl.useProgram(entity_mesh.material.shader_program);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, entity_mesh.material.texture);

    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));

    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, entity_mesh.indices_type, null);
}
pub fn draw_animated_skinned_mesh(world: *ecs.world, entity_mesh: zeng.skinned_mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    zeng.gl.useProgram(entity_mesh.material.shader_program);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, entity_mesh.material.texture);

    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));

    const bone_matrices_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "bone_matrices");
    zeng.gl.uniformMatrix4fv(bone_matrices_location, 100, zeng.gl.FALSE, @ptrCast(world.id_get(entity_mesh.skeleton, .skeleton).?.model_bone_matrices));

    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, entity_mesh.indices_type, null);
}

pub const color = struct {
    r: f32,
    g: f32,
    b: f32,

    pub const WHITE = color{ .r = 1.0, .g = 1.0, .b = 1.0 };
    pub const BLACK = color{ .r = 0.0, .g = 0.0, .b = 0.0 };
    pub const RED = color{ .r = 1.0, .g = 0.0, .b = 0.0 };
    pub const GREEN = color{ .r = 0.0, .g = 1.0, .b = 0.0 };
    pub const BLUE = color{ .r = 0.0, .g = 0.0, .b = 1.0 };
    pub const YELLOW = color{ .r = 1.0, .g = 1.0, .b = 0.0 };
    pub const CYAN = color{ .r = 0.0, .g = 1.0, .b = 1.0 };
    pub const MAGENTA = color{ .r = 1.0, .g = 0.0, .b = 1.0 };
    pub const GRAY = color{ .r = 0.5, .g = 0.5, .b = 0.5 };
    pub const ORANGE = color{ .r = 1.0, .g = 0.5, .b = 0.0 };
    pub const PURPLE = color{ .r = 0.5, .g = 0.0, .b = 0.5 };
    pub const LIME = color{ .r = 0.0, .g = 1.0, .b = 0.5 };
};
pub fn draw_rect(ctx: zeng.engine_context, ui_ren: *@import("main.zig").rect_render_res, x: f32, y: f32, w: f32, h: f32, _color: color) void {
    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);

    // Set position and size uniforms if needed
    const screen_res_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_res");
    const pos_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos");
    const size_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "dims");
    const color_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "_color");
    zeng.gl.uniform2f(screen_res_location, @floatFromInt(ctx.width), @floatFromInt(ctx.height));
    zeng.gl.uniform2f(pos_location, x, y);
    zeng.gl.uniform2f(size_location, w, h);
    zeng.gl.uniform3f(color_location, _color.r, _color.g, _color.b);

    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, 6, zeng.gl.UNSIGNED_INT, null);
    zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl_log_errors() catch void{};
}

pub const triangle_debug_info = struct {
    vao: u32,
    vbo: u32,
    debug_shader: u32,
    projection_matrix: [16]f32,
    inv_camera_matrix: [16]f32,
};
pub fn debug_draw_triangle(tri: [3]zeng.vec3, info: triangle_debug_info) void {
    zeng.gl.useProgram(info.debug_shader);
    zeng.gl.bindVertexArray(info.vao);
    zeng.gl.bindBuffer(zeng.gl.ARRAY_BUFFER, info.vbo);
    zeng.gl.bufferData(zeng.gl.ARRAY_BUFFER, @sizeOf(f32) * 9, &tri, zeng.gl.STATIC_DRAW);

    var clip_matrix = zeng.mat_mult(info.projection_matrix, zeng.mat_mult(info.inv_camera_matrix, zeng.mat_identity));

    const world_location = zeng.gl.getUniformLocation(info.debug_shader, "world");
    const clip_location = zeng.gl.getUniformLocation(info.debug_shader, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &zeng.mat_identity);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    zeng.gl.disable(zeng.gl.CULL_FACE);
    zeng.gl.polygonMode(zeng.gl.FRONT_AND_BACK, zeng.gl.LINE);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, 3, zeng.gl.UNSIGNED_BYTE, null);
    zeng.gl.polygonMode(zeng.gl.FRONT_AND_BACK, zeng.gl.FILL);
    zeng.gl.enable(zeng.gl.CULL_FACE);
    zeng.gl.enable(zeng.gl.DEPTH_TEST);
}
