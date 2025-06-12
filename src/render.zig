const zeng = @import("zeng.zig");
const std = @import("std");
const ecs = @import("ecs.zig");

pub fn draw_text(string: []const u8, ui_ren: *@import("main.zig").text_render_res) void {
    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    defer zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ui_ren.texture);

    zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "dims"), 0.02, 0.05);

    var horizontal: usize = 0;
    for (string) |char| {
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos"), @as(f32, @floatFromInt(horizontal)) * 0.038, 0.0);
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
    zeng.gl.uniformMatrix4fv(bone_matrices_location, 100, zeng.gl.FALSE, @ptrCast(world.get(entity_mesh.skeleton, zeng.skeleton).?.model_bone_matrices));

    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, entity_mesh.indices_type, null);
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

    zeng.gl.disable(zeng.gl.CULL_FACE);
    zeng.gl.polygonMode(zeng.gl.FRONT_AND_BACK, zeng.gl.LINE);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, 3, zeng.gl.UNSIGNED_BYTE, null);
    zeng.gl.polygonMode(zeng.gl.FRONT_AND_BACK, zeng.gl.FILL);
    zeng.gl.enable(zeng.gl.CULL_FACE);
}
