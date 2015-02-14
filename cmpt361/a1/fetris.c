#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "cog/dbg.h"
#include "cog/shaders/shaders.h"

// --- //

bool keys[1024];
GLuint wWidth  = 400;
GLuint wHeight = 720;
GLuint gVAO;
GLuint gVBO;

// --- //

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        if(key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
                glfwSetWindowShouldClose(w, GL_TRUE);
        }

        if(action == GLFW_PRESS) {
                keys[key] = true;
        } else if(action == GLFW_RELEASE) {
                keys[key] = false;
        }
}

/* Initialize the Grid */
// Insert TRON pun here.
void grid() {
        GLfloat gridPoints[320];  // Contains colour info as well.
        int i;

        debug("Initializing Grid.");

        // Vertical lines
	for (i = 0; i < 11; i++){
                // Bottom coord
		gridPoints[10*i]     = 33.0 + (33.0 * i);
                gridPoints[10*i + 1] = 33.0;
                // Bottom colour
                gridPoints[10*i + 2] = 1;
                gridPoints[10*i + 3] = 1;
                gridPoints[10*i + 4] = 1;
                // Top coord
		gridPoints[10*i + 5] = 33.0 + (33.0 * i);
                gridPoints[10*i + 6] = 693.0;
                // Top colour
                gridPoints[10*i + 7] = 1;
                gridPoints[10*i + 8] = 1;
                gridPoints[10*i + 9] = 1;
	}

	// Horizontal lines
	for (i = 0; i < 21; i++){
                // Left coord
		gridPoints[110 + 10*i]     = 33.0;
                gridPoints[110 + 10*i + 1] = 33.0 + (33.0 * i);
                // Left colour
                gridPoints[110 + 10*i + 2] = 1;
                gridPoints[110 + 10*i + 3] = 1;
                gridPoints[110 + 10*i + 4] = 1;
                // Right coord
		gridPoints[110 + 10*i + 5] = 363.0;
                gridPoints[110 + 10*i + 6] = 33.0 + (33.0 * i);
                // Right colour
                gridPoints[110 + 10*i + 7] = 1;
                gridPoints[110 + 10*i + 8] = 1;
                gridPoints[110 + 10*i + 9] = 1;
	}

        // Set up VAO/VBO
        glGenVertexArrays(1,&gVAO);
        glBindVertexArray(gVAO);
        glGenBuffers(1,&gVBO);
        glBindBuffer(GL_ARRAY_BUFFER, gVBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(gridPoints),gridPoints,GL_STATIC_DRAW);

        // Tell OpenGL how to process Grid Vertices
        glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),
                              (GLvoid*)(2 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);

        glBindVertexArray(0);  // Reset the VAO binding.
}

int main(int argc, char** argv) {
        // Initial settings.
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
        
        // Make a window.
        GLFWwindow* w = glfwCreateWindow(wWidth,wHeight,"Fetris",NULL,NULL);
        glfwMakeContextCurrent(w);

        // Fire up GLEW.
        glewExperimental = GL_TRUE;  // For better compatibility.
        glewInit();

        // For the rendering window.
        glViewport(0,0,wWidth,wHeight);

        // Register callbacks.
        glfwSetKeyCallback(w, key_callback);

        // Depth Testing
        glEnable(GL_DEPTH_TEST);
        
        // Create Shader Program
        debug("Making shader program.");
        shaders_t* shaders = cogsShaders("vertex.glsl", "fragment.glsl");
        GLuint shaderProgram = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(shaderProgram > 0, "Shaders didn't compile.");
        debug("Shaders good.");

        // Initialize Grid
        grid();
        
        // Render until you shouldn't.
        while(!glfwWindowShouldClose(w)) {
                glfwPollEvents();
                
                glClearColor(0.0f,0.0f,0.0f,1.0f);
                glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

                glUseProgram(shaderProgram);

                // Draw Grid
                glBindVertexArray(gVAO);
                glDrawArrays(GL_LINES, 0, 64);
                glBindVertexArray(0);

                // Always comes last.
                glfwSwapBuffers(w);
        }

        // Clean up.
        glfwTerminate();
        log_info("Thanks for playing!");

        return EXIT_SUCCESS;
 error:
        return EXIT_FAILURE;
}
