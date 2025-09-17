#version 410 core
layout (location = 0) in vec3 v_pos;

uniform vec2 screen_res;
uniform vec2 dims;
uniform vec2 screen_pos;

void main()
{
    gl_Position = vec4((v_pos.x * dims.x + screen_pos.x)/screen_res.x, (v_pos.y * dims.y + screen_pos.y)/screen_res.y, v_pos.z, 1.0);
}