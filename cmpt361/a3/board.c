#include <math.h>
#include <stdlib.h>

#include "board.h"
#include "cog/dbg.h"
#include "cog/linalg/linalg.h"

// --- //

/* Create a new Chess board */
Board* newBoard(matrix_t* n, matrix_t* o, Material* m) {
        Board* newB = NULL;

        check(n, "Null Normal given.");
        check(o, "Null origin given.");
        check(m, "Null Material given.");

        newB = malloc(sizeof(Board));
        check_mem(newB);

        newB->normal = n;
        newB->origin = o;
        newB->mat = m;
        
        return newB;
 error:
        return NULL;
}

/* Free a Board's memory */
void destroyBoard(Board* b) {
        check(b, "Can't destroy Null Board.");

        free(b->normal);
        free(b->origin);
        destroyMaterial(b->mat);
        free(b);
 error:
        return;
}

/* If a Ray hits a Board, how far away is the contact Point? */
GLfloat scalar_to_board(Board* b, matrix_t* eye, matrix_t* ray) {
        matrix_t* eye_to_origin = coglMSubP(b->origin,eye);
        GLfloat numer = coglVDotProduct(eye_to_origin,b->normal);
        GLfloat denom = coglVDotProduct(ray,b->normal);
        GLfloat result = numer / denom;
        
        if(result > 0) {
                return result;
        } else {
                return NAN;
        }
}

/* Does a given point sit on the Board? */
bool point_in_board(Board* b, matrix_t* point) {
        check(b, "Null Board given.");
        check(point, "Null point given.");

        if(point->m[0] >= b->origin->m[0] &&
           point->m[0] <= b->origin->m[0] + 16 &&
           point->m[1] >= b->origin->m[1] &&
           point->m[1] <= b->origin->m[1] + 16 &&
           point->m[2] <= b->origin->m[2] &&
           point->m[2] >= b->origin->m[2] - 16) {
                return true;
        }
 error:
        return false;
}
