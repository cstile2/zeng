#version 410 core

layout (location = 0) in vec3 v_position;

uniform mat4 world;
uniform mat4 clip;

void main()
{
    gl_Position = clip * vec4(v_position, 1.0);
}