const Engine = @import("engine.zig");

pub fn DrawMesh(entity: Engine.Entity, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    // use shader program > bind VAO > bind texture
    Engine.gl.useProgram(entity.mesh.material.shader_program_GPU);
    Engine.gl.bindVertexArray(entity.mesh.vao_gpu);
    Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, entity.mesh.material.texture_GPU);
    defer {
        Engine.gl.bindVertexArray(0);
        Engine.gl.bindTexture(Engine.gl.TEXTURE_2D, 0);
    }

    // set uniforms
    const world_location = Engine.gl.getUniformLocation(entity.mesh.material.shader_program_GPU, "world");
    const clip_location = Engine.gl.getUniformLocation(entity.mesh.material.shader_program_GPU, "clip");
    Engine.gl.uniformMatrix4fv(world_location, 1, Engine.gl.FALSE, &entity.transform);
    var clip_matrix = Engine.multiply_matrices(projection_matrix, Engine.multiply_matrices(inv_camera_matrix, entity.transform));
    Engine.gl.uniformMatrix4fv(clip_location, 1, Engine.gl.FALSE, &clip_matrix);

    // draw object
    Engine.gl.drawElements(Engine.gl.TRIANGLES, entity.mesh.indices_length, Engine.gl.UNSIGNED_INT, @ptrFromInt(0));
}
