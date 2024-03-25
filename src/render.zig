const gl = @import("gl");
const Engine = @import("engine.zig");

pub fn DrawMesh(entity: Engine.Entity, projection_matrix: [16]f32, inv_camera_matrix: [16]f32) void {
    // use shader program > bind VAO > bind texture
    gl.useProgram(entity.material.shader_program_GPU);
    gl.bindVertexArray(entity.vao_gpu);
    gl.bindTexture(gl.TEXTURE_2D, entity.material.texture_GPU);
    defer {
        gl.bindVertexArray(0);
        gl.bindTexture(gl.TEXTURE_2D, 0);
    }

    // set uniforms
    const world_location = gl.getUniformLocation(entity.material.shader_program_GPU, "world");
    const clip_location = gl.getUniformLocation(entity.material.shader_program_GPU, "clip");
    gl.uniformMatrix4fv(world_location, 1, gl.FALSE, &entity.world_matrix);
    var clip_matrix = Engine.multiply_matrices(projection_matrix, Engine.multiply_matrices(inv_camera_matrix, entity.world_matrix));
    gl.uniformMatrix4fv(clip_location, 1, gl.FALSE, &clip_matrix);

    // draw object
    gl.drawElements(gl.TRIANGLES, entity.indices_length, gl.UNSIGNED_INT, @ptrFromInt(0));
}
