#version 410 core
out vec4 FragColor;

uniform sampler2D image_texture;

in vec3 f_normal;
in vec2 f_tex_coord;

void main()
{
    vec3 normal = normalize(f_normal);
    float shade = dot(normal, normalize(vec3(3.0, 5.0, 1.0)));
    shade = clamp(shade, 0.0, 1.0);
    FragColor = vec4(texture(image_texture, f_tex_coord).xyz * (shade + 0.3), 1.0);
}