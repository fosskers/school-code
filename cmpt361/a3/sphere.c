#include <stdlib.h>

#include "cog/dbg.h"
#include "cog/linalg/linalg.h"
#include "sphere.h"

// --- //

/* Create a sphere, given its center coordinates, a radius, and colours */
Sphere* newSphere(GLuint id, GLfloat r, matrix_t* c, matrix_t* a, matrix_t* d, matrix_t* s, GLfloat shininess, GLfloat reflectance) {
        Sphere* newS = NULL;

        check(r > 0, "Non-positive radius given.");

        newS = malloc(sizeof(Sphere));
        check_mem(newS);

        newS->id = id;
        newS->radius = r;
        newS->center = c;
        newS->mat = newMaterial(a,d,s,shininess,reflectance);

        return newS;
 error:
        return NULL;
}

/* Free a Sphere's memory */
void destroySphere(Sphere* s) {
        check(s, "Can't free NULL Sphere.");

        free(s->center);
        free(s);

 error:
        return;
}
