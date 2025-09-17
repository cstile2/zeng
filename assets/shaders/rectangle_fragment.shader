#version 410 core
out vec4 FragColor;

uniform sampler2D image_texture;
uniform vec4 _color;
uniform int image;
uniform vec2 dims;
uniform float radius;

in vec2 f_uv;

float inv_lerp(float a, float b, float value) {
    return (value - a) / (b - a);
}

void main() {
    vec2 pixelCoord = f_uv * dims;
    
    float alph = 1.0;
    if (pixelCoord.x < radius && pixelCoord.y < radius) {
        float len = length(pixelCoord - vec2(radius));
        alph = inv_lerp(radius + 1.0, radius - 1.0, len);
    }
    if (pixelCoord.x > dims.x - radius && pixelCoord.y < radius) {
        float len = length(pixelCoord - vec2(dims.x - radius, radius));

        alph = inv_lerp(radius + 1.0, radius - 1.0, len);
    }
    if (pixelCoord.x < radius && pixelCoord.y > dims.y - radius) {
        float len = length(pixelCoord - vec2(radius, dims.y - radius));

        alph = inv_lerp(radius + 1.0, radius - 1.0, len);
    }
    if (pixelCoord.x > dims.x - radius && pixelCoord.y > dims.y - radius) {
        float len = length(pixelCoord - vec2(dims.x - radius, dims.y - radius));

        alph = inv_lerp(radius + 1.0, radius - 1.0, len);
    }
    

    if (image == 0) {
        FragColor = vec4(_color.rgb, _color.a * alph);
    } else {
        vec3 texcolor = texture(image_texture, f_uv).xyz;
        FragColor = vec4(_color.rgb * texcolor, _color.a * alph);
    }
}