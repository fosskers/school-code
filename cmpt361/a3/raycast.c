#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "cog/dbg.h"
#include "cog/shaders/shaders.h"
#include "material.h"
#include "sphere.h"

// --- //

#define W_WIDTH 600
#define W_HEIGHT 600
#define PIXEL_DATA W_WIDTH * W_HEIGHT * 3

GLuint VAO;
GLuint VBO;
GLfloat buffer[W_HEIGHT][W_WIDTH][3];

matrix_t* global_ambient = NULL;

// --- //

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        if(action == GLFW_PRESS && key == GLFW_KEY_Q) {
                glfwSetWindowShouldClose(w, GL_TRUE);
        }
}

GLfloat max_of(GLfloat f1, GLfloat f2) {
        if(f1 > f2) {
                return f1;
        } else {
                return f2;
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

// I think we can make this more general, to light any scene.
matrix_t* light_scene(Material* m, matrix_t* x, matrix_t* n, matrix_t* eye, matrix_t* lPos) {
        matrix_t* am = NULL;  // Ambient colour
        matrix_t* di = NULL;  // Diffuse colour
        matrix_t* sp = NULL;  // Specular colour

        // Constants for the scene
        GLfloat scene_ambient  = 0.1;
        GLfloat scene_diffuse  = 1.0;
        GLfloat scene_specular = 1.0;

        // Light decay with distance
        GLfloat dec_a = 0.5;
        GLfloat dec_b = 0.3;
        GLfloat dec_c = 0.0;

        check(m, "Null Material given.");
        check(x, "Null Point given.");
        check(n, "Null Normal given.");
        check(eye, "Null eye position given.");
        check(lPos, "Null light position given.");

        matrix_t* l = coglMSubP(lPos, x);
        GLfloat lDist = coglVLength(l);
        GLfloat decay = 1 / (dec_a + dec_b * lDist + dec_c * lDist * lDist);
        
        //matrix_t* n = coglVNormalize(coglMSubP(x, s->center));
        l = coglVNormalize(l);
        matrix_t* v = coglVNormalize(coglMSubP(eye, x));
        matrix_t* r = coglVNormalize(coglMSub(coglMScaleP(
                              n, 2 * coglVDotProduct(l,n)), l));

        // `i` is global, `k` is constant for material
        am = coglMScaleP(m->ambient, scene_ambient);
        di = coglMScaleP(m->diffuse,
                        scene_diffuse * max_of(0.0,
                                               coglVDotProduct(l,n)));
        sp = coglMScaleP(m->specular,
                        scene_specular * powf(max_of(0.0,
                                                     coglVDotProduct(r,v)),
                                              m->shininess));

        matrix_t* ref = coglMScaleP(global_ambient, m->reflectance);
        matrix_t* di_sp = coglMScale(coglMAddP(di,sp), decay);
        matrix_t* terms[3] = { ref, am, di_sp };
        matrix_t* sum = coglMSumsP(terms, 3);

        // Free memory
        coglMDestroy(l);
        coglMDestroy(v);
        coglMDestroy(r);
        coglMDestroy(am);
        coglMDestroy(di);
        coglMDestroy(sp);
        coglMDestroy(ref);
        coglMDestroy(di_sp);

        return sum;
 error:
        return NULL;
}

/* Returns the scaling factor `d`, if there is one.
 * Arguments aren't checked for NULL, so don't fuck it up.
 *
 * s   := The sphere the Ray might be hitting.
 * eye := Our eye position
 * ray := Normalized direction vector from eye.
 */
GLfloat scaling_factor(Sphere* s, matrix_t* eye, matrix_t* ray) {
        matrix_t* ray_to_center = coglMSubP(eye, s->center);
        GLfloat dotProd = coglVDotProduct(ray, ray_to_center);
        GLfloat len = coglVLength(ray_to_center);
        GLfloat inner = dotProd * dotProd - len * len + s->radius * s->radius;

        GLfloat d1 = -dotProd + sqrt(inner);
        GLfloat d2 = -dotProd - sqrt(inner);

        coglMDestroy(ray_to_center);

        // If one is NaN, they both are.
        if(isnan(d1)) {
                return NAN;
        } else if(d1 < 0 || d2 < 0) {  // Keep an eye on this.
                return NAN;
        }else if (d1 < d2) {
                return d1;
        } else {
                return d2;
        }
}

/* Ray Trace a few Spheres. Modifies given buffer */
void default_scene(matrix_t* eye, matrix_t* lPos) {
        GLfloat min_d, curr_d;
        GLuint i,j,k;
        GLuint total_hits = 0;
        Sphere* curr_s = NULL;
        matrix_t* colour = NULL;  // Final pixel colour.
        matrix_t* ray = NULL;     // Normalized ray from eye.
        matrix_t* x = NULL;       // The Ray/Sphere intersection point.
        matrix_t* n = NULL;       // Normal from point `x`.
       
        check(eye, "Bad eye position given.");

        debug("Ray Tracing default scene...");
        
        /* These could be ordered by distance by calculating their
         * eye_to_center lengths. You could stop ray tracing on the first
         * sphere you hit, as you know it's the closest one.
         */ 
        Sphere* spheres[3] = {
                newSphere(1.23,
                          coglV3(1.5, -0.2, -3.2),  // Center
                          coglV3(0.7, 0.7, 0.7),    // Ambient colour
                          coglV3(0.1, 0.5, 0.8),    // Diffuse colour
                          coglV3(1.0, 1.0, 1.0),    // Specular colour
                          10,                       // Shininess
                          0.4),                     // Ambient Reflectance
                newSphere(1.5,
                          coglV3(-1.5, 0.0, -3.5),
                          coglV3(0.6, 0.6, 0.6),
                          coglV3(1.0, 0.0, 0.25),
                          coglV3(1.0, 1.0, 1.0),
                          6,
                          0.3),
                newSphere(0.5,
                          coglV3(-0.35, 1.75, -2.25),
                          coglV3(0.2, 0.2, 0.2),
                          coglV3(0.0, 1.0, 0.25),
                          coglV3(0.0, 1.0, 0.0),
                          30,
                          0.3)
        };

        // n^2. Love it.
        for(i = 0; i < W_WIDTH; i++) {
                for (j = 0; j < W_HEIGHT; j++) {
                        min_d  = INFINITY;
                        colour = NULL;
                        curr_s = NULL;
                        x      = NULL;
                        n      = NULL;

                        ray = coglVNormalize(coglV3(-2 + (4*i/(GLfloat)W_WIDTH) - eye->m[0],
                                                    -2 + (4*j/(GLfloat)W_HEIGHT) - eye->m[1],
                                                    -eye->m[2]));

                        // For each Sphere
                        for(k = 0; k < 3; k++) {
                                curr_d = scaling_factor(spheres[k], eye, ray);

                                // Fails if curr_d == NAN.
                                if(curr_d < min_d) {
                                        min_d = curr_d;
                                        curr_s = spheres[k];
                                }
                        }

                        if(curr_s) {
                                total_hits++;

                                // Calculate colour.
                                x = coglMAddP(eye,coglMScale(ray,min_d));
                                n = coglVNormalize(coglMSubP(x, curr_s->center));
                                colour = light_scene(curr_s->mat,x,n,eye,lPos);

                                // Sphere's colour.
                                buffer[j][i][0] = colour->m[0];
                                buffer[j][i][1] = colour->m[1];
                                buffer[j][i][2] = colour->m[2];

                                // Free Vectory memory.
                                coglMDestroy(colour);
                                coglMDestroy(x);
                                coglMDestroy(n);
                        } else {
                                // Background colour.
                                buffer[j][i][0] = 0.5;
                                buffer[j][i][1] = 0.05;
                                buffer[j][i][2] = 0.8;
                        }

                        coglMDestroy(ray);
                }
        }

        debug("Total Ray hits: %d", total_hits);

        destroySphere(spheres[0]);
        destroySphere(spheres[1]);
        destroySphere(spheres[2]);
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

        /* Set default scene */
        matrix_t* eye  = coglV3(0,0,2);
        matrix_t* lPos = coglV3(-2.0, 5.0, 1.0);
        global_ambient = coglV3(0.2, 0.2, 0.2);
        //matrix_t* lPos = coglV3(-2.0, 5.0, 7.5);
        default_scene(eye, lPos);
        
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
        coglMDestroy(eye);
        coglMDestroy(lPos);
        coglMDestroy(global_ambient);
        glfwTerminate();
        log_info("Done.");

        return EXIT_SUCCESS;
 error:
        return EXIT_FAILURE;
}
