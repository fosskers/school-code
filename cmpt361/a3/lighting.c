#include <math.h>

#include "cog/dbg.h"
#include "lighting.h"

// --- //

/* Which is the greater of two floats? */
GLfloat max_of(GLfloat f1, GLfloat f2) {
        if(f1 > f2) {
                return f1;
        } else {
                return f2;
        }
}

/* Is a particular point facing the light source? */
bool is_facing_light(matrix_t* x, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard) {
        matrix_t* ray = coglVNormalize(coglMSubP(env->lPos, x));
        matrix_t* colour = pixel_colour(ray,x,env,rec_depth - 1, ignore, ignoreBoard);
        
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
matrix_t* material_colour(Material* m, matrix_t* x, matrix_t* n, matrix_t* eye, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard) {
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
        check(env->lPos, "Null light position given.");

        matrix_t* l = coglMSubP(env->lPos, x);
        GLfloat lDist = coglVLength(l);
        GLfloat decay = 1 / (dec_a + dec_b * lDist + dec_c * lDist * lDist);
        
        l = coglVNormalize(l);
        matrix_t* v = coglVNormalize(coglMSubP(eye, x));
        matrix_t* r = coglVNormalize(coglMSub(coglMScaleP(
                              n, 2 * coglVDotProduct(l,n)), l));

        am = coglMScaleP(m->ambient, scene_ambient);
        di = coglMScaleP(m->diffuse,
                        scene_diffuse * max_of(0.0,
                                               coglVDotProduct(l,n)));

        sp = coglMScaleP(m->specular,
                         scene_specular * powf(max_of(0.0,
                                                      coglVDotProduct(r,v)),
                                               m->shininess));

        // This might be wrong.
        matrix_t* g_am  = coglMScaleP(env->global_ambient,
                                      m->reflectance);
        matrix_t* di_sp = coglMScale(coglMAddP(di,sp), decay);

        /* Shadows */
        if(env->shadows && !is_facing_light(x,env,rec_depth,ignore,ignoreBoard)) {
                coglMScale(di_sp, 0);
        }

        matrix_t* ray = coglVNormalize(coglMSubP(x, eye));
        matrix_t* ref_ray = coglVNormalize(
                            coglMSubP(
                            ray, coglMScaleP(n,
                                             2 * coglVDotProduct(ray,n))));

        /* Reflections */
        matrix_t* ref;
        if(env->reflections) {
                ref = pixel_colour(ref_ray, x, env, rec_depth - 1, ignore, ignoreBoard);
                if(ref) {
                        ref = coglMScale(ref, m->reflectance);
                } else {
                        ref = coglV3(0,0,0);
                }
        } else {
                ref = coglV3(0,0,0);
        }

        matrix_t* terms[4] = { g_am, am, di_sp, ref };
        matrix_t* sum = coglMSumsP(terms, 4);

        // Free memory
        coglMDestroy(l);
        coglMDestroy(v);
        coglMDestroy(r);
        coglMDestroy(am);
        coglMDestroy(di);
        coglMDestroy(sp);
        coglMDestroy(g_am);
        coglMDestroy(di_sp);
        coglMDestroy(ray);
        coglMDestroy(ref_ray);
        coglMDestroy(ref);

        return sum;
 error:
        return NULL;
}

/* The final colour of a pixel pointed to by a Ray */
matrix_t* pixel_colour(matrix_t* ray, matrix_t* eye, Env* env, GLint rec_depth, GLint ignore, bool ignoreBoard) {
        GLfloat board_d = INFINITY;
        GLfloat curr_d;
        GLfloat min_d = INFINITY;
        GLuint k;
        Sphere* curr_s   = NULL;
        bool board_hit   = false; // Was the Chess board hit?
        matrix_t* colour = NULL;  // Final pixel colour.
        matrix_t* n      = NULL;  // Normal from point `x`.
        matrix_t* x      = NULL;  // The Ray/Sphere intersection point.

        check(env, "Null environment given.");

        if(rec_depth < 0) {
                return NULL;
        }

        /* Chess Board intersection check */
        if(env->chess_board && !ignoreBoard) {
                board_d = scalar_to_board(env->board, eye, ray);
                if(!isnan(board_d)) {
                        board_hit = true;
                } else {
                        board_d = INFINITY;
                }
        }

        // For each Sphere
        for(k = 0; k < env->num_spheres; k++) {
                if(env->spheres[k]->id == ignore) {
                        continue;
                }

                // Is there a `d`?
                curr_d = scalar_to_sphere(env->spheres[k], eye, ray);

                // Fails if curr_d == NAN.
                if(curr_d < min_d) {
                        min_d = curr_d;
                        curr_s = env->spheres[k];
                }
        }

        if(curr_s && min_d < board_d) {
                // Calculate colour.
                x = coglMAddP(eye,coglMScale(ray,min_d));
                n = coglVNormalize(coglMSubP(x, curr_s->center));
                colour = material_colour(curr_s->mat,x,n,eye,env,
                                         rec_depth,curr_s->id,false);

                // Free Vector memory.
                coglMDestroy(x);
                coglMDestroy(n);
        } else if(board_hit) {
                x = coglMAddP(eye,coglMScale(ray,board_d));
                n = env->board->normal;
                colour = material_colour(env->board->mat,x,n,eye,env,
                                         rec_depth,-1,true);

                // Free Vector memory.
                coglMDestroy(x);
        }

        return colour;
 error:
        return NULL;
}
