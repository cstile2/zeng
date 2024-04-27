const zeng = @import("zeng.zig");

pub fn draw_mesh(entity_mesh: zeng.Mesh, entity_transform: zeng.Transform, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
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
    var clip_matrix = zeng.multiply_matrices(projection_matrix, zeng.multiply_matrices(inv_camera_matrix, entity_transform));
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    // draw object
    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, zeng.gl.UNSIGNED_INT, @ptrFromInt(0));
}
