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
        GLuint i,j,k;
        GLfloat i_len = 1/(GLfloat)W_WIDTH;
        GLfloat j_len = 1/(GLfloat)W_HEIGHT;
        GLfloat r,g,b;
        GLuint hits;  // Total Ray hits for this pixel.
        GLuint total_hits = 0;
        matrix_t* colours[5];
        matrix_t* ray;
       
        check(env, "Null environment given.");

        debug("Ray Tracing default scene...");

        // For every pixel
        for(i = 0; i < W_WIDTH; i++) {
                for (j = 0; j < W_HEIGHT; j++) {
                        r = 0.0;
                        g = 0.0;
                        b = 0.0;
                        hits = 0;
                        ray = coglVNormalize(
                              coglV3(-2 + 4*i*i_len - eye->m[0],
                                     -2 + 4*j*j_len - eye->m[1],
                                     -eye->m[2]));

                        colours[0] = pixel_colour(ray,eye,env,
                                                  env->rec_depth,-1,false);
                        if(env->anti_aliasing) {
                        colours[1] = pixel_colour(
                                     coglVNormalize(
                                     coglV3(-2 + 4*i*i_len + i_len-eye->m[0],
                                            -2 + 4*j*j_len + j_len-eye->m[1],
                                            -eye->m[2])),
                                     eye,env,env->rec_depth,-1,false);
                        colours[2] = pixel_colour(
                                     coglVNormalize(
                                     coglV3(-2 + 4*i*i_len + i_len-eye->m[0],
                                            -2 + 4*j*j_len - j_len-eye->m[1],
                                            -eye->m[2])),
                                     eye,env,env->rec_depth,-1,false);
                        colours[3] = pixel_colour(
                                     coglVNormalize(
                                     coglV3(-2 + 4*i*i_len - i_len-eye->m[0],
                                            -2 + 4*j*j_len + j_len-eye->m[1],
                                            -eye->m[2])),
                                     eye,env,env->rec_depth,-1,false);
                        colours[4] = pixel_colour(
                                     coglVNormalize(
                                     coglV3(-2 + 4*i*i_len - i_len-eye->m[0],
                                            -2 + 4*j*j_len - j_len-eye->m[1],
                                            -eye->m[2])),
                                     eye,env,env->rec_depth,-1,false);
                        
                        for(k = 0; k < 5; k++) {
                                if(colours[k]) {
                                        total_hits++;

                                        r += colours[k]->m[0];
                                        g += colours[k]->m[1];
                                        b += colours[k]->m[2];
                                } else {
                                        r += env->bgc->m[0];
                                        g += env->bgc->m[1];
                                        b += env->bgc->m[2];
                                }
                        }
                        r = r / 5.0;
                        g = g / 5.0;
                        b = b / 5.0;
                        } else if(colours[0] != NULL) {
                                r = colours[0]->m[0];
                                g = colours[0]->m[1];
                                b = colours[0]->m[2];
                        } else {
                                r = env->bgc->m[0];
                                g = env->bgc->m[1];
                                b = env->bgc->m[2];
                        }

                        buffer[j][i][0] = r;
                        buffer[j][i][1] = g;
                        buffer[j][i][2] = b;

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
        //glfwSetInputMode(w,GLFW_CURSOR,GLFW_CURSOR_DISABLED);
        
        /* Depth Testing */
        glEnable(GL_DEPTH_TEST);

        /* Blending */
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

        /* Set initial randomness */
        srand((GLuint)(100000 * glfwGetTime()));

        /* Set default Environment */
        Env* env = newEnv(1,                       // Recursion depth
                          true,                    // Render default scene?
                          false,                   // Check board
                          false,                   // Show shadows?
                          false,                   // Show reflections?
                          false,                   // Show refractions?
                          coglV3(-2.0, 5.0, 1.0),  // Light position
                          coglV3(0.2, 0.2, 0.2),   // Global ambient colour
                          NULL,                    // Spheres
                          0,                       // # of spheres
                          NULL,                    // Chess Board  
                          coglV3(0.5,0.05,0.8),    // Background colour
                          false,                   // Anti-aliasing?
                          false);                  // Diffuse reflections?

        /* Set user-specified options */
        check(argc >= 3, "Not enough arguments given.");
        env->rec_depth = (GLint)atoi(argv[2]);
        int i;
        for(i = 3; i < argc; i++) {
                if(strcmp(argv[i],"+s") == 0) {
                        env->shadows = true;
                } else if(strcmp(argv[i],"+l") == 0) {
                        env->reflections = true;
                } else if(strcmp(argv[i],"+c") == 0) {
                        env->chess_board = true;
                } else if(strcmp(argv[i],"+r") == 0) {
                        env->refraction = true;
                } else if(strcmp(argv[i],"+p") == 0) {
                        env->anti_aliasing = true;
                } else if(strcmp(argv[i],"+f") == 0) {
                        env->diff_refl = true;
                }
        }

        /* Set default spheres */
        Sphere* spheres[3] = {
                newSphere(0,
                          1.23,                    // Radius
                          coglV3(1.5, -0.2, -3.2), // Center
                          coglV3(0.7, 0.7, 0.7),   // Ambient colour
                          coglV3(0.1, 0.5, 0.8),   // Diffuse colour
                          coglV3(1.0, 1.0, 1.0),   // Specular colour
                          10,                      // Shininess
                          0.4),                    // Ambient Reflectance
                newSphere(1,
                          1.5,
                          coglV3(-1.5, 0.0, -3.5),
                          coglV3(0.6, 0.6, 0.6),
                          coglV3(1.0, 0.0, 0.25),
                          coglV3(1.0, 1.0, 1.0),
                          6,
                          0.3),
                newSphere(2,
                          0.5,
                          coglV3(-0.35, 1.75, -2.25),
                          coglV3(0.2, 0.2, 0.2),
                          coglV3(0.0, 1.0, 0.25),
                          coglV3(0.0, 1.0, 0.0),
                          30,
                          0.3)
        };

        env->spheres = spheres;
        env->num_spheres = 3;

        /* Set Chess Board */
        env->board = newBoard(coglVNormalize(coglV3(0,1,1)),     // Normal
                              coglV3(-8,-8,-8),  // Origin
                              newMaterial(coglV3(0.7, 0.7, 0.7),
                                          coglV3(0.1, 0.5, 0.8),
                                          coglV3(1.0, 1.0, 1.0),
                                          10,
                                          0.4,
                                          2.0));

        /* Set default scene */
        matrix_t* eye = coglV3(0,0,2);
        default_scene(env,eye);
        destroyEnv(env);
        
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
