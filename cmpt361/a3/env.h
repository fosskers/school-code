#ifndef __env_h__
#define __env_h__

#include <GL/glew.h>
#include <stdbool.h>

#include "cog/linalg/linalg.h"

// --- //

typedef struct {
        GLuint rec_depth;
        bool chess_board;
        bool shadows;
        bool reflections;
        bool refraction;
        matrix_t* lPos;
        matrix_t* global_ambient;
} Env;

/* Set the rendering environment */
Env* newEnv(GLuint rd, bool c, bool s, bool refl, bool refr, matrix_t* l, matrix_t* ga);

/* Free environment memory */
void envDestroy(Env* env);

#endif
