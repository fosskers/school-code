#ifndef __lighting_h__
#define __lighting_h__

#include "env.h"
#include "sphere.h"

// --- //

/* Which is the greater of two floats? */
GLfloat max_of(GLfloat f1, GLfloat f2);

/* Is a particular point facing the light source? */
bool is_facing_light(matrix_t* x, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard);

/* Finds colour for a Point, based on its material properties */
matrix_t* material_colour(Material* m, matrix_t* x, matrix_t* n, matrix_t* eye, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard);

/* The final colour of a pixel pointed to by a Ray */
matrix_t* pixel_colour(matrix_t* ray, matrix_t* eye, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard);

#endif
