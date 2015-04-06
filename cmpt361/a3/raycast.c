#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "cog/dbg.h"
#include "cog/shaders/shaders.h"
#include "env.h"
#include "lighting.h"
#include "material.h"
#include "scene.h"
#include "sphere.h"

// --- //

#define W_WIDTH 600
#define W_HEIGHT 600
#define PIXEL_DATA W_WIDTH * W_HEIGHT * 3

GLuint VAO;
GLuint VBO;
GLfloat buffer[W_HEIGHT][W_WIDTH][3];

// --- //

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        if(action == GLFW_PRESS && key == GLFW_KEY_Q) {
                glfwSetWindowShouldClose(w, GL_TRUE);
        }
}

/* Fills pixel buffer with random colours */
void fill_buffer_randomly() {
        GLuint i,j;

        for(i = 0; i < W_WIDTH; i++) {
                for (j = 0; j < W_HEIGHT; j++) {
                        buffer[j][i][0] = rand() / (GLfloat)RAND_MAX;
                        buffer[j][i][1] = rand() / (GLfloat)RAND_MAX;
                        buffer[j][i][2] = rand() / (GLfloat)RAND_MAX;
                }
        }

        // For testing how array dims affect pixels displayed
        for(i = 0; i < 10; i++) {
                buffer[i][0][0] = 0.0;
                buffer[i][0][1] = 0.0;
                buffer[i][0][2] = 0.0;
        }
}

/* Ray Trace a few Spheres. Modifies given buffer */
void default_scene(Env* env, matrix_t* eye) {
        GLuint i,j;
        GLuint total_hits = 0;
        matrix_t* colour;
        matrix_t* ray;
       
        check(env, "Null environment given.");

        debug("Ray Tracing default scene...");

        // For every pixel
        for(i = 0; i < W_WIDTH; i++) {
                for (j = 0; j < W_HEIGHT; j++) {
                        ray = coglVNormalize(
                              coglV3(-2 + (4*i/(GLfloat)W_WIDTH) - eye->m[0],
                                     -2 + (4*j/(GLfloat)W_HEIGHT) - eye->m[1],
                                     -eye->m[2]));

                        colour = pixel_colour(ray,eye,env,env->rec_depth,-1);

                        if(colour) {
                                total_hits++;

                                buffer[j][i][0] = colour->m[0];
                                buffer[j][i][1] = colour->m[1];
                                buffer[j][i][2] = colour->m[2];
                        } else {
                                // Background colour.
                                buffer[j][i][0] = 0.5;
                                buffer[j][i][1] = 0.05;
                                buffer[j][i][2] = 0.8;
                        }

                        coglMDestroy(colour);
                        coglMDestroy(ray);
                }
        }

        debug("Total Ray hits: %d", total_hits);

 error:
        return;
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
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, W_WIDTH, W_HEIGHT, 0,
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
        GLFWwindow* w = glfwCreateWindow(W_WIDTH,W_HEIGHT,"Raycast",NULL,NULL);
        glfwMakeContextCurrent(w);

        /* GLEW settings */
        glewExperimental = GL_TRUE;  // For better compatibility.
        glewInit();

        glViewport(0,0,W_WIDTH,W_HEIGHT);

        /* Register callbacks */
        glfwSetKeyCallback(w, key_callback);
        glfwSetInputMode(w,GLFW_CURSOR,GLFW_CURSOR_DISABLED);
        
        /* Depth Testing */
        glEnable(GL_DEPTH_TEST);

        /* Blending */
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

        /* Set initial randomness */
        srand((GLuint)(100000 * glfwGetTime()));

        /* Set default Environment */
        Env* env = newEnv(1,
                          true,                    // Render default scene?
                          false,                   // Check board
                          false,                   // Show shadows?
                          false,                   // Show reflections?
                          false,                   // Show refractions?
                          coglV3(-2.0, 5.0, 1.0),  // Light position
                          coglV3(0.2, 0.2, 0.2));  // Global ambient colour

        /* Set user-specified options */
        check(argc >= 3, "Not enough arguments given.");
        check(strcmp(argv[1],"-d") == 0, "-u not implemented yet.");
        env->rec_depth = (GLint)atoi(argv[2]);
        int i;
        for(i = 3; i < argc; i++) {
                if(strcmp(argv[i],"+s") == 0) {
                        env->shadows = true;
                } else if(strcmp(argv[i],"+l") == 0) {
                        env->reflections = true;
                }
        }

        /* Set default spheres */
        spheres[0] = newSphere(0,
                               1.23,
                               coglV3(1.5, -0.2, -3.2), // Center
                               coglV3(0.7, 0.7, 0.7),   // Ambient colour
                               coglV3(0.1, 0.5, 0.8),   // Diffuse colour
                               coglV3(1.0, 1.0, 1.0),   // Specular colour
                               10,                      // Shininess
                               0.4);                    // Ambient Reflectance
        spheres[1] = newSphere(1,
                               1.5,
                               coglV3(-1.5, 0.0, -3.5),
                               coglV3(0.6, 0.6, 0.6),
                               coglV3(1.0, 0.0, 0.25),
                               coglV3(1.0, 1.0, 1.0),
                               6,
                               0.3);
        spheres[2] = newSphere(2,
                               0.5,
                               coglV3(-0.35, 1.75, -2.25),
                               coglV3(0.2, 0.2, 0.2),
                               coglV3(0.0, 1.0, 0.25),
                               coglV3(0.0, 1.0, 0.0),
                               30,
                               0.3);
        
        /* Set default scene */
        matrix_t* eye = coglV3(0,0,2);
        default_scene(env,eye);
        envDestroy(env);
        
        /* Fill `buffer` */
        //fill_buffer_randomly();

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
