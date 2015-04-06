#ifndef __env_h__
#define __env_h__

#include <GL/glew.h>
#include <stdbool.h>

#include "board.h"
#include "cog/linalg/linalg.h"
#include "sphere.h"

// --- //

typedef struct {
        /* Flags */
        bool render_default;
        bool chess_board;
        bool shadows;
        bool reflections;
        bool refraction;
        bool anti_aliasing;

        /* Scene objects */
        Sphere** spheres;
        GLuint num_spheres;
        Board* board;
        
        /* Other settings */
        GLint rec_depth;
        matrix_t* lPos;
        matrix_t* global_ambient;
        matrix_t* bgc;
} Env;

/* Set the rendering environment */
Env* newEnv(GLint rd, bool d, bool c, bool s, bool refl, bool refr, matrix_t* l, matrix_t* ga, Sphere** ss, GLuint ns, Board* b, matrix_t* bgc, bool anti);

/* Free environment memory */
void destroyEnv(Env* env);

#endif
