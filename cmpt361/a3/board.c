#include <stdlib.h>

#include "board.h"
#include "cog/dbg.h"

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
