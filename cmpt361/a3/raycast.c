#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "cog/dbg.h"
#include "sphere.h"
#include "cog/shaders/shaders.h"

// --- //

#define wWidth 800
#define wHeight 600
#define PIXEL_DATA wWidth * wHeight * 3

GLuint VAO;
GLuint VBO;
GLfloat buffer[wHeight][wWidth][3];

// --- //

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        if(action == GLFW_PRESS || key == GLFW_KEY_Q) {
                glfwSetWindowShouldClose(w, GL_TRUE);
        }
}

/* Initial VAO/VBO */
GLuint init_buffers() {
        GLfloat points[] = {
                // Locations  // Texture Coords
                -1,-1,    0,0,
                 1,-1,    1,0,
                -1,1,     0,1,

                -1,1,     0,1,
                1,1,      1,1,
                1,-1,     1,0
        };
        
        debug("Initializing Buffer objects...");

        glGenVertexArrays(1,&VAO);
        glBindVertexArray(VAO);

        /* Generate Texture */
        GLuint tex;
	glGenTextures(1, &tex);
	glBindTexture(GL_TEXTURE_2D, tex);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, wWidth, wHeight, 0,
                     GL_RGB, GL_FLOAT, buffer);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glActiveTexture(GL_TEXTURE0);

        /* Generate Buffer */
        glGenBuffers(1,&VBO);
        glBindBuffer(GL_ARRAY_BUFFER,VBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(points),points,GL_STATIC_DRAW);

        /* Vertex Attributes */
        glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,
                              4 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,
                              4 * sizeof(GLfloat),
                              (GLvoid*)(2 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);

        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        debug("Buffer object initialized.");

        return tex;
}

int main(int argc, char** argv) {
        /* Initial settings */
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
        
        /* Window creation */
        GLFWwindow* w = glfwCreateWindow(wWidth,wHeight,"Raycast",NULL,NULL);
        glfwMakeContextCurrent(w);

        /* GLEW settings */
        glewExperimental = GL_TRUE;  // For better compatibility.
        glewInit();

        glViewport(0,0,wWidth,wHeight);

        /* Register callbacks */
        glfwSetKeyCallback(w, key_callback);
        glfwSetInputMode(w,GLFW_CURSOR,GLFW_CURSOR_DISABLED);
        
        /* Depth Testing */
        glEnable(GL_DEPTH_TEST);

        /* Blending */
        //glEnable(GL_BLEND);
        //glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

        /* Fill `buffer` */
        GLuint i,j;
        for (i = 0; i < wHeight; i++) {
                for (j = 0; j < wWidth; j++) {
                        buffer[i][j][0] = rand() / (GLfloat)RAND_MAX;
                        buffer[i][j][1] = rand() / (GLfloat)RAND_MAX;
                        buffer[i][j][2] = rand() / (GLfloat)RAND_MAX;
                }
        }

        /* Initialize VAO/VBO/Texture */
        GLuint tex = init_buffers();

        /* Compile Shader Program */
        debug("Compiling shaders.");
        shaders_t* shaders = cogsShaders("vertex.glsl", "fragment.glsl");
        GLuint shaderP = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(shaderP > 0, "Shaders didn't compile.");

        /* Bind Texture */
        glUseProgram(shaderP);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, tex);
        glUniform1i(glGetUniformLocation(shaderP,"tex"),0);
        
        /* Default Scene - Three Spheres */
        Sphere* spheres[3] = {
                newSphere(1.5, -0.2, -3.2, 1.23),
                newSphere(-1.5, 0.0, -3.5, 1.5),
                newSphere(-0.35, 1.75, -2.25, 0.5)
        };

        // Might be important
        glClearColor(0.5f,0.05f,0.8f,1.0f);

        debug("Entering Loop.");
        // Render until you shouldn't.
        while(!glfwWindowShouldClose(w)) {
                glfwPollEvents();

                //glClearColor(0.5f,0.05f,0.8f,1.0f);
                glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

                /* Draw Pixels */
                glBindVertexArray(VAO);
                glDrawArrays(GL_TRIANGLES, 0, 6);
                glBindVertexArray(0);
                              
                glfwSwapBuffers(w);
        }
        
        // Clean up.
        glfwTerminate();
        log_info("Done.");

        return EXIT_SUCCESS;
 error:
        return EXIT_FAILURE;
}
