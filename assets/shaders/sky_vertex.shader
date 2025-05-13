#version 410 core
layout (location = 0) in vec3 v_pos;
layout (location = 1) in vec2 v_uv_pos;

out vec2 f_uv_pos;

void main()
{
    f_uv_pos = v_uv_pos;
    gl_Position = vec4(v_pos, 1.0);
}