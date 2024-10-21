#version 410 core

layout (location = 0) in vec3 v_position;
layout (location = 1) in vec3 v_normal;
layout (location = 2) in vec2 v_tex_coord;

uniform mat4 world;
uniform mat4 clip;

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

void main()
{
    f_normal = (world * vec4(v_normal, 0.0)).xyz;
    gl_Position = clip * vec4(v_position, 1.0);
    // gl_Position = snap_to_position(gl_Position);
    f_tex_coord = v_tex_coord;
}