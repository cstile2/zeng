const zeng = @import("zeng.zig");
const std = @import("std");
const ecs = @import("ecs.zig");
const ui = @import("ui.zig");

pub fn draw_text(string: []const u8, ui_ren: *zeng.text_render_res, x: f32, y: f32, ctx: zeng.graphics_t) void {
    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    defer zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, ui_ren.texture);

    zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "dims"), 12, 18);
    const screen_res_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_res");
    zeng.gl.uniform2f(screen_res_location, @floatFromInt(ctx.width), @floatFromInt(ctx.height));

    var horizontal: usize = 0;
    for (string) |char| {
        const _char = char - 32;
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos"), @as(f32, @floatFromInt(horizontal)) * 12 + x, y);
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(ui_ren.shader_program, "image_point"), @as(f32, @floatFromInt(_char % 16)), @as(f32, @floatFromInt(_char / 16)));
        zeng.gl.drawElements(zeng.gl.TRIANGLES, ui_ren.indices_len, zeng.gl.UNSIGNED_INT, null);
        horizontal += 1;
    }
}
pub fn draw_sdf_font_text(string: []const u8, font_info: ui.font_info, mesh: zeng.mesh, x: f32, y: f32, scale: f32, ctx: zeng.graphics_t) void {
    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    defer zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl.disable(zeng.gl.CULL_FACE);

    zeng.gl.useProgram(font_info.shader_program);
    zeng.gl.bindVertexArray(mesh.vao_gpu);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, font_info.tex);

    zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "screen_res"), @floatFromInt(ctx.width), @floatFromInt(ctx.height));
    zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "image_dimensions"), @floatFromInt(font_info.tex_width), @floatFromInt(font_info.tex_height));
    zeng.gl.uniform1f(zeng.gl.getUniformLocation(font_info.shader_program, "scale"), scale);

    var horizontal: f32 = 0;
    for (string) |char| {
        if (char < 32) continue;
        const _char = char - 32;
        const curr_info = font_info.character_infos[_char];

        zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "slice_position"), @floatFromInt(curr_info.x), @floatFromInt(curr_info.y));
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "slice_dimensions"), @floatFromInt(curr_info.width), @floatFromInt(curr_info.height));
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "screenspace_dims"), @floatFromInt(curr_info.width), @floatFromInt(curr_info.height));
        zeng.gl.uniform2f(zeng.gl.getUniformLocation(font_info.shader_program, "screenspace_pos"), x + (horizontal + curr_info.xoffset) * scale, y + curr_info.yoffset * scale);
        zeng.gl.drawElements(zeng.gl.TRIANGLES, mesh.indices_length, mesh.indices_type, null);
        horizontal += curr_info.xadvance;
    }
}
pub fn draw_mesh(entity_mesh: zeng.mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32, camera_position: zeng.vec3, light_space_matrix: [16]f32, shadow_map: *zeng.shadow_map_res) void {

    // use shader program > bind VAO > bind texture
    zeng.gl.useProgram(entity_mesh.material.shader_program);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);

    const albedo_texture_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "albedo_texture");
    zeng.gl.uniform1i(albedo_texture_location, 0); // texture unit 0
    const shadow_map_texture_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "shadow_map");
    zeng.gl.uniform1i(shadow_map_texture_location, 1); // texture unit 0

    const base_color_texture = entity_mesh.material.parameter_map.get("albedo_texture").?.texture;
    zeng.gl.activeTexture(zeng.gl.TEXTURE0);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, base_color_texture);

    zeng.gl.activeTexture(zeng.gl.TEXTURE1);
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, shadow_map.depth_map_texture);

    zeng.gl.activeTexture(zeng.gl.TEXTURE0);

    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));
    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);

    const base_color_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "albedo");
    const base_color = entity_mesh.material.parameter_map.get("albedo").?.float_3;
    zeng.gl.uniform3fv(base_color_location, 1, @ptrCast(&base_color));

    const lights_locations_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "light_positions");
    zeng.gl.uniform3fv(lights_locations_location, 4, @ptrCast(&[4]zeng.vec3{ zeng.vec3{ .y = 15 }, zeng.vec3{}, zeng.vec3{}, zeng.vec3{} }));
    const lights_colors_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "light_colors");
    zeng.gl.uniform3fv(lights_colors_location, 4, @ptrCast(&[4]zeng.vec3{ zeng.vec3.ONE.mult(12), zeng.vec3{}, zeng.vec3{}, zeng.vec3{} }));

    const metallic = entity_mesh.material.parameter_map.get("metallic").?.float_1;
    const metallic_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "metallic");
    zeng.gl.uniform1f(metallic_location, metallic);
    const roughness = entity_mesh.material.parameter_map.get("roughness").?.float_1;
    const roughness_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "roughness");
    zeng.gl.uniform1f(roughness_location, roughness);
    const ao_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "ao");
    zeng.gl.uniform1f(ao_location, 1.0);
    const cam_pos_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "cam_pos");
    zeng.gl.uniform3fv(cam_pos_location, 1, @ptrCast(&camera_position));

    const light_space_matrix_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "lightSpaceMatrix");
    zeng.gl.uniformMatrix4fv(light_space_matrix_location, 1, zeng.gl.FALSE, &light_space_matrix);

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, entity_mesh.indices_type, null);
}
var noise_tex: ?u32 = null;
pub fn draw_animated_skinned_mesh(world: *ecs.world_t, entity_mesh: zeng.skinned_mesh, entity_transform: zeng.world_matrix, projection_matrix: [16]f32, inv_camera_matrix: [16]f32, camera_position: zeng.vec3) void {
    zeng.gl.useProgram(entity_mesh.material.shader_program);
    zeng.gl.bindVertexArray(entity_mesh.vao_gpu);

    const base_color_texture = entity_mesh.material.parameter_map.get("albedo_texture").?.texture;
    zeng.gl.bindTexture(zeng.gl.TEXTURE_2D, base_color_texture);

    var clip_matrix = zeng.mat_mult(projection_matrix, zeng.mat_mult(inv_camera_matrix, entity_transform));
    const bone_matrices_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "bone_matrices");
    const world_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "world");
    const clip_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "clip");
    zeng.gl.uniformMatrix4fv(world_location, 1, zeng.gl.FALSE, &entity_transform);
    zeng.gl.uniformMatrix4fv(clip_location, 1, zeng.gl.FALSE, &clip_matrix);
    zeng.gl.uniformMatrix4fv(bone_matrices_location, 100, zeng.gl.FALSE, @ptrCast(world.get(entity_mesh.skeleton, zeng.skeleton).?.model_bone_matrices));

    const base_color_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "albedo");
    const base_color = entity_mesh.material.parameter_map.get("albedo").?.float_3;
    zeng.gl.uniform3fv(base_color_location, 1, @ptrCast(&base_color));

    const lights_locations_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "light_positions");
    zeng.gl.uniform3fv(lights_locations_location, 4, @ptrCast(&[4]zeng.vec3{ zeng.vec3{ .y = 1 }, zeng.vec3{}, zeng.vec3{}, zeng.vec3{} }));
    const lights_colors_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "light_colors");
    zeng.gl.uniform3fv(lights_colors_location, 4, @ptrCast(&[4]zeng.vec3{ zeng.vec3.ONE.mult(5), zeng.vec3{}, zeng.vec3{}, zeng.vec3{} }));

    const metallic = entity_mesh.material.parameter_map.get("metallic").?.float_1;
    const metallic_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "metallic");
    zeng.gl.uniform1f(metallic_location, metallic);
    const roughness = entity_mesh.material.parameter_map.get("roughness").?.float_1;
    const roughness_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "roughness");
    zeng.gl.uniform1f(roughness_location, roughness);
    const ao_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "ao");
    zeng.gl.uniform1f(ao_location, 1.0);
    const cam_pos_location = zeng.gl.getUniformLocation(entity_mesh.material.shader_program, "cam_pos");
    zeng.gl.uniform3fv(cam_pos_location, 1, @ptrCast(&camera_position));

    zeng.gl.drawElements(zeng.gl.TRIANGLES, entity_mesh.indices_length, entity_mesh.indices_type, null);
}
pub fn draw_sky(sky_shader: u32, square_vao: u32, square_indices_length: c_int, camera_matrix: zeng.world_matrix, camera: *zeng.camera) void {
    zeng.gl.useProgram(sky_shader);
    zeng.gl.bindVertexArray(square_vao);
    zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_world_space"), 1, zeng.gl.FALSE, camera_matrix[0..].ptr);
    zeng.gl.uniformMatrix4fv(zeng.gl.getUniformLocation(sky_shader, "camera_perspective"), 1, zeng.gl.FALSE, &camera.projection_matrix);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, square_indices_length, zeng.gl.UNSIGNED_INT, null);
    zeng.gl.clear(zeng.gl.DEPTH_BUFFER_BIT);
}

pub const color = struct {
    r: f32,
    g: f32,
    b: f32,
    a: f32 = 1,

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
    pub const CLEAR = color{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 };
};
pub fn draw_rect(__graphics: zeng.graphics_t, ui_ren: *zeng.rect_render_res, x: f32, y: f32, w: f32, h: f32, _color: color) void {
    zeng.gl.useProgram(ui_ren.shader_program);
    zeng.gl.bindVertexArray(ui_ren.vao);

    // Set position and size uniforms if needed
    const screen_res_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_res");
    const pos_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "screen_pos");
    const size_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "dims");
    const color_location = zeng.gl.getUniformLocation(ui_ren.shader_program, "_color");
    zeng.gl.uniform2f(screen_res_location, @floatFromInt(__graphics.width), @floatFromInt(__graphics.height));
    zeng.gl.uniform2f(pos_location, x, y);
    zeng.gl.uniform2f(size_location, w, h);
    zeng.gl.uniform3f(color_location, _color.r, _color.g, _color.b);

    zeng.gl.disable(zeng.gl.DEPTH_TEST);
    zeng.gl.drawElements(zeng.gl.TRIANGLES, 6, zeng.gl.UNSIGNED_INT, null);
    zeng.gl.enable(zeng.gl.DEPTH_TEST);

    zeng.gl_log_errors();
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
