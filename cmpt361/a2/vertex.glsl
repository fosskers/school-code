#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 colour;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

out vec4 vColour;

void main() {
        gl_Position = proj * view * model * vec4(position, 1.0);
        vColour = vec4(colour,1.0);
}
