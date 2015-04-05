#include <stdlib.h>

#include "cog/dbg.h"
#include "env.h"

// --- //

/* Set the rendering environment */
Env* newEnv(GLuint rd, bool c, bool s, bool refl, bool refr) {
        Env* e = malloc(sizeof(Env));
        check_mem(e);

        e->rec_depth = rd;
        e->chess_board = c;
        e->shadows = s;
        e->reflections = refl;
        e->refraction = refr;

        return e;
 error:
        return NULL;
}
