const zeng = @import("zeng.zig");
const std = @import("std");

pub fn draw_mesh(entity_mesh: zeng.mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    // use shader program > bind VAO > bind texture
    zeng.gl.useProgram(entity_mesh.material.shader_program_GPU);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, entity_mesh.material.texture_GPU);
    defer {
        zeng.gl.bindVertexArray(0);
        zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, 0);
    }

    // set uniforms
    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    // draw object
    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, zeng.gl.UNSIGNED_INT, null);
}

/// Draws the text
pub fn draw_text(string: []const u8, ui_ren: *@import("main.zig").text_render_res) void {
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

    // zeng.gl.bindVertexArray(0);
}

// pub fn draw_skinned_mesh(entity_mesh: zeng.SkinnedMesh, entity_transform: zeng.Transform, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
//     bone_transforms[9] = zeng.multiply_matrices(zeng.axis_angle_to_matrix(zeng.Vec3{ .x = 1.0, .y = 0.0, .z = 0.0 }, 0.01), bone_transforms[9]);

//     var c: usize = 0;
//     while (c < entity_mesh.inverse_bind_matrices.len) {
//         real_transforms[c] = zeng.multiply_matrices(zeng.invert_matrix(entity_mesh.inverse_bind_matrices[c]), bone_transforms[c]);
//         c += 1;
//     }

//     zeng.gl.useProgram(entity_mesh.material.shader_program_GPU);
//     zeng.gl.bindVertexArray(entity_mesh.vao_gpu);
//     zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, entity_mesh.material.texture_GPU);
//     defer {
//         zeng.gl.bindVertexArray(0);
//         zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, 0);
//     }

//     var curr: usize = 0;
//     while (curr < entity_mesh.parent_indices.len) {
//         defer curr += 1;
//         temp_transforms[curr] = real_transforms[curr];
//         const parent_index = entity_mesh.parent_indices[curr];
//         if (parent_index != -1 and parent_index < entity_mesh.parent_indices.len) {
//             temp_transforms[curr] = zeng.multiply_matrices(temp_transforms[@intCast(parent_index)], temp_transforms[curr]);
//         }

//         temp_transforms[curr] = zeng.multiply_matrices(temp_transforms[curr], entity_mesh.inverse_bind_matrices[curr]);
//     }

//     // set uniforms
//     const bone_matrices_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "bone_matrices");
//     const final_matrix_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "final_matrix");
//     zeng.gl.uniformMatrix4fv(bone_matrices_location, 100, zeng.gl.FALSE, @ptrCast(&temp_transforms));
//     var clip_matrix = zeng.multiply_matrices(projection_matrix, zeng.multiply_matrices(inv_camera_matrix, entity_transform));
//     zeng.gl.uniformMatrix4fv(final_matrix_location, 1, zeng.gl.FALSE, &clip_matrix);

//     try zeng.opengl_log_error();

//     // draw object
//     zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, zeng.gl.UNSIGNED_INT, null);
// }

pub fn draw_animated_skinned_mesh(entity_mesh: zeng.skinned_mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    zeng.gl.useProgram(entity_mesh.material.shader_program_GPU);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, entity_mesh.material.texture_GPU);

    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));

    const bone_matrices_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "bone_matrices");
    const final_matrix_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program_GPU, "final_matrix");
    zeng.gl.uniformMatrix4fv(bone_matrices_location, 100, zeng.gl.FALSE, @ptrCast(entity_mesh.skeleton.model_bone_matrices));
    zeng.gl.uniformMatrix4fv(final_matrix_location, 1, zeng.gl.FALSE, &clip_matrix);

    // try zeng.opengl_log_error();

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, zeng.gl.UNSIGNED_INT, null);
}
