#include "block.h"
#include "collision.h"
#include "cog/dbg.h"

// --- //

/* Would dropping the Block result in a collision? */
bool isColliding(block_t* b, Fruit* board) {
        check(b, "Null Block given.");
        check(board, "Null Board given.");

 error:
        return false;
}

/* Are all the Block's Cells within the Board? */
bool isInBoard(block_t* b, matrix_t** centers) {
        GLuint i;

        check(b, "Null Block given.");

        matrix_t* nearest = nearestCell(b, centers);
        check(nearest, "Failed to find nearest Cell.");

        for(i = 0; i < 6; i+=2) {
                if(b->coords[i] + nearest->m[0] < 0   ||
                   b->coords[i] + nearest->m[0] > 9   ||
                   b->coords[i+1] + nearest->m[1] < 0 ||
                   b->coords[i+1] + nearest->m[1] > 19) {
                        return false;
                }
        }

        return true;
 error:
        return false;
}
