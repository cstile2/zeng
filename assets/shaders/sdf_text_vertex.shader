#version 410 core
layout (location = 0) in vec3 v_pos;
layout (location = 1) in vec2 v_uv_pos;
layout (location = 0) out vec2 f_uv_pos;

uniform vec2 screenspace_dims;
uniform vec2 screenspace_pos;
uniform vec2 screen_res;
uniform float scale;

float remap(float low1, float high1, float low2, float high2, float value) {
    return low2 + (value - low1) * (high2 - low2) / (high1 - low1);
}

void main()
{
    f_uv_pos = v_uv_pos;

    float x = remap(-1.0, 1.0, screenspace_pos.x, screenspace_pos.x + screenspace_dims.x * scale, v_pos.x);
    float y = remap(-1.0, 1.0, screenspace_pos.y, screenspace_pos.y + screenspace_dims.y * scale, v_pos.y);

    float x2 = remap(0, screen_res.x, -1.0, 1.0, x);
    float y2 = remap(0, screen_res.y, 1.0, -1.0, y);

    gl_Position = vec4(x2, y2, v_pos.z, 1.0);
}