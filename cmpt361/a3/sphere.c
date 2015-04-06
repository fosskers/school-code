#include <stdlib.h>
#include <math.h>

#include "cog/dbg.h"
#include "cog/linalg/linalg.h"
#include "sphere.h"

// --- //

/* Create a sphere, given its center coordinates, a radius, and colours */
Sphere* newSphere(GLint id, GLfloat r, matrix_t* c, matrix_t* a, matrix_t* d, matrix_t* s, GLfloat shininess, GLfloat reflectance) {
        Sphere* newS = NULL;

        check(r > 0, "Non-positive radius given.");

        newS = malloc(sizeof(Sphere));
        check_mem(newS);

        newS->id = id;
        newS->radius = r;
        newS->center = c;
        newS->mat = newMaterial(a,d,s,shininess,reflectance,1.5);

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

/* Returns the scaling factor `d`, if there is one.
 * Arguments aren't checked for NULL, so don't fuck it up.
 *
 * s   := The sphere the Ray might be hitting.
 * eye := Our eye position
 * ray := Normalized direction vector from eye.
 */
GLfloat scalar_to_sphere(Sphere* s, matrix_t* eye, matrix_t* ray) {
        matrix_t* ray_to_center = coglMSubP(eye, s->center);
        GLfloat dotProd = coglVDotProduct(ray, ray_to_center);
        GLfloat len = coglVLength(ray_to_center);
        GLfloat inner = dotProd * dotProd - len * len + s->radius * s->radius;

        GLfloat d1 = -dotProd + sqrt(inner);
        GLfloat d2 = -dotProd - sqrt(inner);

        coglMDestroy(ray_to_center);

        // If one is NaN, they both are.
        if(isnan(d1)) {
                return NAN;
        } else if(d1 < 0 || d2 < 0) {  // Keep an eye on this.
                return NAN;
        } else if (d1 < d2 && d1 > 0) {
                return d1;
        } else {
                return d2;
        }
}
