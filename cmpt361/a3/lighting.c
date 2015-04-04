#include <math.h>

#include "cog/dbg.h"
#include "lighting.h"
#include "scene.h"

// --- //

GLuint ignoredSphere = -1;

/* Which is the greater of two floats? */
GLfloat max_of(GLfloat f1, GLfloat f2) {
        if(f1 > f2) {
                return f1;
        } else {
                return f2;
        }
}

/* Is a particular point facing the light source? */
bool is_facing_light(matrix_t* x, matrix_t* lPos) {
        matrix_t* ray = coglVNormalize(coglMSubP(lPos, x));
        matrix_t* colour = pixel_colour(ray,x,lPos);
        
        // If a colour was found, the ray hit something, and thus
        // the Point isn't facing the light source.
        if(colour) {
                coglMDestroy(ray);
                coglMDestroy(colour);

                return false;
        } else {
                coglMDestroy(ray);
                coglMDestroy(colour);

                return true;
        }
}

/* Finds colour for a Point, based on its material properties */
matrix_t* material_colour(Material* m, matrix_t* x, matrix_t* n, matrix_t* eye, matrix_t* lPos) {
        matrix_t* am = NULL;  // Ambient colour
        matrix_t* di = NULL;  // Diffuse colour
        matrix_t* sp = NULL;  // Specular colour

        // Constants for the scene
        GLfloat scene_ambient  = 0.1;
        GLfloat scene_diffuse  = 1.0;
        GLfloat scene_specular = 1.0;

        // Light decay with distance
        GLfloat dec_a = 0.5;
        GLfloat dec_b = 0.3;
        GLfloat dec_c = 0.0;

        check(m, "Null Material given.");
        check(x, "Null Point given.");
        check(n, "Null Normal given.");
        check(eye, "Null eye position given.");
        check(lPos, "Null light position given.");

        matrix_t* l = coglMSubP(lPos, x);
        GLfloat lDist = coglVLength(l);
        GLfloat decay = 1 / (dec_a + dec_b * lDist + dec_c * lDist * lDist);
        
        //matrix_t* n = coglVNormalize(coglMSubP(x, s->center));
        l = coglVNormalize(l);
        matrix_t* v = coglVNormalize(coglMSubP(eye, x));
        matrix_t* r = coglVNormalize(coglMSub(coglMScaleP(
                              n, 2 * coglVDotProduct(l,n)), l));

        // `i` is global, `k` is constant for material
        am = coglMScaleP(m->ambient, scene_ambient);
        di = coglMScaleP(m->diffuse,
                        scene_diffuse * max_of(0.0,
                                               coglVDotProduct(l,n)));

        sp = coglMScaleP(m->specular,
                         scene_specular * powf(max_of(0.0,
                                                      coglVDotProduct(r,v)),
                                               m->shininess));

        matrix_t* ref = coglMScaleP(global_ambient, m->reflectance);
        matrix_t* di_sp = coglMScale(coglMAddP(di,sp), decay);

        // Shadows
        if(!is_facing_light(x,lPos)) {
                coglMScale(di_sp, 0);
        }

        matrix_t* terms[3] = { ref, am, di_sp };
        matrix_t* sum = coglMSumsP(terms, 3);

        // Free memory
        coglMDestroy(l);
        coglMDestroy(v);
        coglMDestroy(r);
        coglMDestroy(am);
        coglMDestroy(di);
        coglMDestroy(sp);
        coglMDestroy(ref);
        coglMDestroy(di_sp);

        return sum;
 error:
        return NULL;
}

/* Returns the scaling factor `d`, if there is one.
 * Arguments aren't checked for NULL, so don't fuck it up.
 *
 * s   := The sphere the Ray might be hitting.
 * eye := Our eye position
 * ray := Normalized direction vector from eye.
 */
GLfloat scaling_factor(Sphere* s, matrix_t* eye, matrix_t* ray) {
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
        }else if (d1 < d2) {
                return d1;
        } else {
                return d2;
        }
}

/* The final colour of a pixel pointed to by a Ray */
matrix_t* pixel_colour(matrix_t* ray, matrix_t* eye, matrix_t* lPos) {
        GLfloat curr_d;
        GLfloat min_d = INFINITY;
        GLuint k;
        Sphere* curr_s   = NULL;
        matrix_t* colour = NULL;  // Final pixel colour.
        matrix_t* n      = NULL;  // Normal from point `x`.
        matrix_t* x      = NULL;  // The Ray/Sphere intersection point.

        // For each Sphere
        for(k = 0; k < 3; k++) {
                if(spheres[k]->id == ignoredSphere) {
                        continue;
                }

                curr_d = scaling_factor(spheres[k], eye, ray);

                // Fails if curr_d == NAN.
                if(curr_d < min_d) {
                        min_d = curr_d;
                        curr_s = spheres[k];
                }
        }

        if(curr_s) {
                ignoredSphere = curr_s->id;

                // Calculate colour.
                x = coglMAddP(eye,coglMScale(ray,min_d));
                n = coglVNormalize(coglMSubP(x, curr_s->center));
                colour = material_colour(curr_s->mat,x,n,eye,lPos);

                // Free Vectory memory.
                coglMDestroy(x);
                coglMDestroy(n);
        } else {
                // There is no sphere to ignore on recursion.
                ignoredSphere = -1;
        }

        return colour;
}
