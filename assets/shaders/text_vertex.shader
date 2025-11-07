#version 410 core
layout (location = 0) in vec3 v_pos;
layout (location = 1) in vec2 v_uv_pos;
layout (location = 0) out vec2 f_uv_pos;

uniform vec2 dims;
uniform vec2 screen_pos;
uniform vec2 screen_res;

void main()
{
    f_uv_pos = v_uv_pos;
    gl_Position = vec4(2.0*(v_pos.x * dims.x + screen_pos.x)/screen_res.x - 1.0, 2.0*(v_pos.y * dims.y - screen_pos.y)/screen_res.y + 1.0, v_pos.z, 1.0);
}