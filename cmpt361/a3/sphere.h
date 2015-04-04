#ifndef __sphere_h__
#define __sphere_h__

#include <GL/glew.h>

#include "cog/linalg/linalg.h"
#include "material.h"

// --- //

typedef struct {
        GLint id;          // Sphere's ID
        GLfloat radius;
        matrix_t* center;  // Center position :: vec3
        Material* mat;     // Colour data
} Sphere;

// --- //

/* Create a sphere, given its center coordinates, a radius, and colours */
Sphere* newSphere(GLint id, GLfloat r, matrix_t* c, matrix_t* a, matrix_t* d, matrix_t* s, GLfloat shininess, GLfloat reflectance);

/* Free a Sphere's memory */
void destroySphere(Sphere* s);

#endif
