#version 410 core
layout (location = 0) in vec3 v_pos;
layout (location = 1) in vec2 v_uv_pos;

layout (location = 0) out vec2 f_uv_pos;

uniform vec2 dims;
uniform vec2 screen_pos;

void main()
{
    f_uv_pos = v_uv_pos;
    gl_Position = vec4(v_pos.x * dims.x + screen_pos.x, v_pos.y * dims.y + screen_pos.y, v_pos.z, 1.0);
}