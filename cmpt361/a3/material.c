#include <stdlib.h>

#include "material.h"
#include "cog/dbg.h"

// --- //

/* Given colour and reflectance data, create a new material */
Material* newMaterial(matrix_t* a, matrix_t* d, matrix_t* s, GLfloat sh, GLfloat r, GLfloat i) {
        check(a && d && s, "Null input Vector given.");

        Material* m = malloc(sizeof(Material));
        check_mem(m);

        m->ambient = a;
        m->diffuse = d;
        m->specular = s;
        m->shininess = sh;
        m->reflectance = r;
        m->refr_index = i;

        return m;
 error:
        return NULL;
}

/* Free a Material's memory */
void destroyMaterial(Material* m) {
        check(m, "Cannot free Null Material.");

        free(m->ambient);
        free(m->diffuse);
        free(m->specular);
        free(m);
 error:
        return;
}
