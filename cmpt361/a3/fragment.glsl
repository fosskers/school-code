#version 330

in vec2 texCoords;

out vec4 colour;

uniform sampler2D texture;

void main() {
	colour = texture2D(texture, texCoords);
}
