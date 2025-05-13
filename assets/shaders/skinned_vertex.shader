#version 410 core

layout (location = 0) in vec3 v_position;
layout (location = 1) in vec3 v_normal;
layout (location = 2) in vec2 v_tex_coord;

layout (location = 3) in ivec4 my_bone_ids;
layout (location = 4) in vec4 my_bone_weights;

uniform mat4 bone_matrices[100];
uniform mat4 world;
uniform mat4 clip;

out vec3 f_normal;
out vec2 f_tex_coord;

void main() {
    vec4 blend_pos = vec4(0.0);
    for(int i = 0; i < 4; i += 1) {
        blend_pos += bone_matrices[my_bone_ids[i]] * vec4(v_position, 1.0) * my_bone_weights[i];
    }
    vec4 blend_normal = vec4(0.0);
    for(int i = 0; i < 4; i += 1) {
        blend_normal += bone_matrices[my_bone_ids[i]] * vec4(v_normal, 0.0) * my_bone_weights[i];
    }
    blend_normal = world * blend_normal;
    
    gl_Position = clip * blend_pos;

    f_normal = normalize(blend_normal.xyz);
    f_tex_coord = v_tex_coord;
}
