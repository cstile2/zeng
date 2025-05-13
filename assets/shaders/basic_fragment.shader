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

    vec3 texcolor = texture(image_texture, f_tex_coord).xyz;

    FragColor = vec4(texcolor * (2.0 * shade + 0.1), 1.0);
    // FragColor.rgb = pow(FragColor.rgb, vec3(1.0/2.2));
}