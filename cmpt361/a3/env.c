#include <stdlib.h>

#include "cog/dbg.h"
#include "env.h"

// --- //

/* Set the rendering environment */
Env* newEnv(GLint rd, bool d, bool c, bool s, bool refl, bool refr, matrix_t* l, matrix_t* ga, Sphere** ss, GLuint ns, Board* b, matrix_t* bgc, bool anti, bool diff_refl) {
        Env* e = malloc(sizeof(Env));
        check_mem(e);

        
        /* Flags */
        e->render_default = d;
        e->chess_board = c;
        e->shadows = s;
        e->reflections = refl;
        e->diff_refl = diff_refl;
        e->refraction = refr;
        e->anti_aliasing = anti;

        /* Scene objects */
        e->spheres = ss;
        e->num_spheres = ns;
        e->board = b;
        
        /* Other settings */
        e->rec_depth = rd;
        e->lPos = l;
        e->global_ambient = ga;
        e->bgc = bgc;

        return e;
 error:
        return NULL;
}

/* Free environment memory */
void destroyEnv(Env* env) {
        check(env, "Can't free Null environment.");

        free(env->lPos);
        free(env->global_ambient);

        GLuint i;
        for(i = 0; i < env->num_spheres; i++) {
                destroySphere(env->spheres[i]);
        }

        destroyBoard(env->board);
        
        free(env);

 error:
        return;
}
