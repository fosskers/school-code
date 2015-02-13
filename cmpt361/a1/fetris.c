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
        log_info("Making shader program.");
        shaders_t* shaders = cogsShaders("vertex.glsl", "fragment.glsl");
        GLuint shaderProgram = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(shaderProgram > 0, "Shaders didn't compile.");

        /*
        // Vertex Array
        GLuint VAO;
        glGenVertexArrays(1,&VAO);

        // Vertex buffer for our data
        GLuint VBO;
        glBindVertexArray(VAO);  // VAO!
        glGenBuffers(1,&VBO);
        glBindBuffer(GL_ARRAY_BUFFER, VBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(verts),verts,GL_STATIC_DRAW);
        
        // Tell OpenGL how to process Vertex data.
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),
                              (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);

        glBindVertexArray(0);  // Reset the VAO binding.
        */

        // Render until you shouldn't.
        while(!glfwWindowShouldClose(w)) {
                glfwPollEvents();
                
                glClearColor(0.0f,0.0f,0.0f,1.0f);
                glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

                glUseProgram(shaderProgram);

                /*
                glBindVertexArray(VAO);
                glBindVertexArray(0);
                */

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
