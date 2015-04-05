#ifndef __env_h__
#define __env_h__

#include <GL/glew.h>
#include <stdbool.h>

// --- //

typedef struct {
        GLuint rec_depth;
        bool chess_board;
        bool shadows;
        bool reflections;
        bool refraction;
} Env;

/* Set the rendering environment */
Env* newEnv(GLuint rd, bool c, bool s, bool refl, bool refr);

#endif
