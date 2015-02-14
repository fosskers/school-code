#version 330 core

layout (location = 0) in vec2 position;
layout (location = 1) in vec3 colour;

out vec4 vColour;

void main() {
        // Used to scale the entire game.
        mat4 scale = mat4(2.0/400, 	0.0, 		0.0, 0.0,
			  0.0,  	2.0/720, 	0.0, 0.0,
			  0.0, 		0.0, 		1.0, 0.0,
			  0.0, 		0.0, 		0.0, 1.0 );

        gl_Position = scale * (vec4(position, 0.0, 1.0) +
                               vec4(-200, -360, 0, 0));
        vColour = vec4(colour,1.0);
}
