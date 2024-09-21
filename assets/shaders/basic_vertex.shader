#version 410 core

layout (location = 0) in vec3 v_position;
layout (location = 1) in vec3 v_normal;
layout (location = 2) in vec2 v_tex_coord;

uniform mat4 world;
uniform mat4 clip;

out vec3 f_normal;
out vec2 f_tex_coord;

void main()
{
    f_normal = (world * vec4(v_normal, 0.0)).xyz;
    gl_Position = clip * vec4(v_position, 1.0);
    f_tex_coord = v_tex_coord;
}