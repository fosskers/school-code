#ifndef __material_h__
#define __material_h__

#include <GL/glew.h>
#include "cog/linalg/linalg.h"

// --- //

typedef struct {
        matrix_t* ambient;  // Ambient colour  :: vec3
        matrix_t* diffuse;  // Diffuse colour  :: vec3
        matrix_t* specular; // Specular colour :: vec3
        GLfloat shininess;
        GLfloat reflectance;  // Of global ambient light.
} Material;

/* Given colour and reflectance data, create a new material */
Material* newMaterial(matrix_t* a, matrix_t* d, matrix_t* s, GLfloat sh, GLfloat r);

/* Free a Material's memory */
void destroyMaterial(Material* m);

#endif
