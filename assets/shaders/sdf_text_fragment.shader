#version 410 core
out vec4 FragColor;

uniform sampler2D image_texture;
uniform vec2 image_dimensions;
uniform vec2 slice_position;
uniform vec2 slice_dimensions;

layout (location = 0) in vec2 f_uv_pos;

float remap(float low1, float high1, float low2, float high2, float value) {
    return low2 + (value - low1) * (high2 - low2) / (high1 - low1);
}

void main() {
    
    vec2 uv0;
    uv0.x = remap(0.0, 1.0, slice_position.x, slice_position.x + slice_dimensions.x, f_uv_pos.x);
    uv0.y = remap(0.0, 1.0, slice_position.y, slice_position.y + slice_dimensions.y, f_uv_pos.y);

    vec2 uv;
    uv.x = remap(0.0, image_dimensions.x, 0.0, 1.0, uv0.x);
    uv.y = remap(0.0, image_dimensions.y, 0.0, 1.0, uv0.y);

    float dfdx = dFdx(texture(image_texture, uv).x);
    float dfdy = dFdy(texture(image_texture, uv).x);

    float d = length(vec2(dfdx, dfdy)) * 0.5;
    // float d = 0.0001;

    FragColor = vec4(vec3(1), remap(0.5 - d, 0.5 + d, 0.0, 1.0, texture(image_texture, uv).x));
    // FragColor = vec4(vec3(1), texture(image_texture, uv).x);

    // FragColor = vec4(texture(image_texture, uv).xyz, 1.0);
    // FragColor = vec4(f_uv_pos, 0, 1);
}