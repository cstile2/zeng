#version 410 core
out vec4 FragColor;

uniform sampler2D image_texture;

uniform vec2 image_point;

layout (location = 0) in vec2 f_uv_pos;

float remap(float low1, float high1, float low2, float high2, float value) {
    return low2 + (value - low1) * (high2 - low2) / (high1 - low1);
}

void main() {
    float d = 0.01;

    vec2 uv = f_uv_pos / vec2(30.0, 8.0);
    
    float _h = 1.0 / 16.0;
    float _v = 1.0 / 6.0;

    uv.x += image_point.x * _h + 0.005;
    uv.y += (-1-image_point.y) * _v + 0.02;

    FragColor = vec4(vec3(1), remap(0.5 - d, 0.5 + d, 0.0, 1.0, texture(image_texture, uv).x));
}