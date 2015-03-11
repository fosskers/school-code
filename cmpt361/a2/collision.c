#include "block.h"
#include "collision.h"
#include "cog/dbg.h"

// --- //

/* Would dropping the Block result in a collision? */
bool isColliding(block_t* b, Fruit* board, matrix_t* nearest) {
        GLuint i;
        GLint x,y;

        check(b, "Null Block given.");
        check(board, "Null Board given.");
        check(nearest, "Null nearest Cell given.");

        if(isInBoard(b, nearest)) {
                for(i = 0; i < 6; i+=2) {
                        x = b->coords[i] + nearest->m[0];
                        y = b->coords[i+1] + nearest->m[1];

                        if(board[y*10 + x] != None) {
                                return true;
                        }
                }
        } else {
                return true;
        }

 error:
        return false;
}

/* Are all the Block's Cells within the Board? */
bool isInBoard(block_t* b, matrix_t* nearest) {
        GLuint i;

        check(b, "Null Block given.");
        check(nearest, "Null nearest Cell given.");

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
