#include <stdlib.h>

#include "cog/dbg.h"
#include "env.h"

// --- //

/* Set the rendering environment */
Env* newEnv(GLint rd, bool d, bool c, bool s, bool refl, bool refr, matrix_t* l, matrix_t* ga) {
        Env* e = malloc(sizeof(Env));
        check_mem(e);

        e->rec_depth = rd;
        e->render_default = d;
        e->chess_board = c;
        e->shadows = s;
        e->reflections = refl;
        e->refraction = refr;
        e->lPos = l;
        e->global_ambient = ga;

        return e;
 error:
        return NULL;
}

/* Free environment memory */
void envDestroy(Env* env) {
        check(env, "Can't free Null environment.");

        free(env->lPos);
        free(env->global_ambient);
        free(env);

 error:
        return;
}
