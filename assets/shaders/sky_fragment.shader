#version 410 core
out vec4 FragColor;

in vec2 f_uv_pos;
uniform mat4 camera_world_space;
uniform mat4 camera_perspective;

float s(float a) {
    return tanh(a) * 0.5 + 0.5;
}

void main() {
    // Reconstruct view direction from screen UV and inverse projection
    vec4 ndc = vec4(f_uv_pos * 2.0 - 1.0, 1.0, 1.0); // NDC space
    mat4 inv_proj = inverse(camera_perspective);
    mat4 inv_view = inverse(camera_world_space);

    // Unproject to view space
    vec4 view_dir = inv_proj * ndc;
    view_dir /= view_dir.w;
    view_dir = normalize(view_dir);

    // Transform to world space
    vec4 world_dir = camera_world_space * vec4(view_dir.xyz, 0.0);
    vec3 dir = normalize(world_dir.xyz);

    // Simple gradient sky: horizon to zenith
    float t = clamp(dir.y * 6.0 + 0.5, 0.0, 1.0);
    vec3 horizon = vec3(0.02, 0.03, 0.02);
     vec3 zenith_a = vec3(0.1, 0.1, 0.3);
     vec3 zenith_b = vec3(0.4, 0.2, 0.4);
    vec3 zenith = mix(zenith_b, zenith_a, smoothstep(0.0, 1.0, dir.y * 1.3));

    vec3 skyColor = mix(horizon, zenith, smoothstep(0.0, 1.0, t));
    // vec3 skyColor = zenith;

    FragColor = vec4(skyColor, 1.0);
}