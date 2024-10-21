#version 410 core

layout (location = 0) in vec3 v_position;
layout (location = 1) in vec3 v_normal;
layout (location = 2) in vec2 v_tex_coord;

layout (location = 3) in ivec4 my_bone_ids;
layout (location = 4) in vec4 my_bone_weights;

uniform mat4 bone_matrices[32];
uniform mat4 final_matrix;

out vec3 f_normal;
out vec2 f_tex_coord;

vec4 snap_to_position(vec4 base_position)
{
    float jitter = 0.5;
    ivec2 resolution = ivec2(320, 240);
	vec4 snapped_position = base_position;
	snapped_position.xyz = base_position.xyz / base_position.w;
	
	vec2 snap_resulotion = floor(vec2(resolution) * (1.0 - jitter));
	snapped_position.x = floor(snap_resulotion.x * snapped_position.x) / snap_resulotion.x;
	snapped_position.y = floor(snap_resulotion.y * snapped_position.y) / snap_resulotion.y;
	
	snapped_position.xyz *= base_position.w;
	return snapped_position;
}

void main() {
    vec4 blend_pos = vec4(0.0);
    for(int i = 0; i < 4; i += 1) {
        blend_pos += bone_matrices[my_bone_ids[i]] * vec4(v_position, 1.0) * my_bone_weights[i];
    }
    vec3 blend_normal = vec3(0.0);
    for(int i = 0; i < 4; i += 1) {
        blend_normal += (bone_matrices[my_bone_ids[i]] * vec4(v_normal, 0.0) * my_bone_weights[i]).xyz;
    }
    blend_normal = normalize(blend_normal);
    
    gl_Position = final_matrix * blend_pos;
    // gl_Position = snap_to_position(gl_Position);

    f_normal = blend_normal;
    f_tex_coord = v_tex_coord;

}
