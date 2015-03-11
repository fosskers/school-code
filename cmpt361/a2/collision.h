#ifndef __collision_h__
#define __collision_h__

#include <stdbool.h>
#include "block.h"

// --- //

/* Would dropping the Block result in a collision? */
bool isColliding(block_t* b, Fruit* board, matrix_t* nearest);

/* Are all the Block's Cells within the Board? */
bool isInBoard(block_t* b, matrix_t* nearest);

#endif
