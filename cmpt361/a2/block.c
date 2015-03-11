#include <stdlib.h>
#include <math.h>

#include "block.h"
#include "cog/dbg.h"
#include "defines.h"
#include "util.h"

// --- //

/* All coordinates for all blocks in "Block Space".
 * Values come in pairs that represent the positions of cell
 * A, B, and D in that order. C is always at (0,0) in Block Space,
 * so we need not list it here.
 */
GLint iBlock[2][6] = {{ -2,0, -1,0, 1,0  },
                      { 0,-2, 0,-1, 0,1  }};
GLint sBlock[2][6] = {{ -1,-1, 0,-1, 1,0 },
                      { 1,-1, 1,0, 0,1   }};
GLint zBlock[2][6] = {{ 1,-1, 0,-1, -1,0 },
                      { -1,-1, -1,0, 0,1 }};
GLint lBlock[4][6] = {{ -1,-1, -1,0, 1,0 },
                      { 1,-1, 0,-1, 0,1  },
                      { 1,1, 1,0, -1,0   },
                      { -1,1, 0,1, 0,-1  }};
GLint oBlock[1][6] = {{ -1,0, -1,-1, 0,-1 }};

// Fruit Colours
GLfloat black[]  = { 0.0, 0.0, 0.0 };
GLfloat purple[] = { 0.76, 0.41, 0.99 };
GLfloat red[]    = { 1.0,  0.56, 0.56 };
GLfloat yellow[] = { 1.0,  1.0,  0.52 };
GLfloat green[]  = { 0.66, 0.99, 0.56 };
GLfloat orange[] = { 1.0,  0.78, 0.28 };

// --- //

/* Create a I block in the default position */
block_t* newI() {
        block_t* b = malloc(sizeof(block_t));
        check_mem(b);

        Fruit* fs = randFruits();
        check(fs, "Failed to generate Fruits.");
        
        b->coords = &iBlock[0][0];
        b->variations = 2;
        b->curr = 0;
        b->x = 0.0f;
        b->y = 0.0f;
        b->fs = fs;
        b->name = 'I';
        b->colliding = false;

        return b;
 error:
        return NULL;
}

/* Create a S block in the default position */
block_t* newS() {
        block_t* b = malloc(sizeof(block_t));
        check_mem(b);

        Fruit* fs = randFruits();
        check(fs, "Failed to generate Fruits.");

        b->coords = &sBlock[0][0];
        b->variations = 2;
        b->curr = 0;
        b->x = 0.0f;
        b->y = 0.0f;
        b->fs = fs;
        b->name = 'S';
        b->colliding = false;

        return b;
 error:
        return NULL;
}

/* Create a Z block in the default position */
block_t* newZ() {
        block_t* b = malloc(sizeof(block_t));
        check_mem(b);

        Fruit* fs = randFruits();
        check(fs, "Failed to generate Fruits.");

        b->coords = &zBlock[0][0];
        b->variations = 2;
        b->curr = 0;
        b->x = 0.0f;
        b->y = 0.0f;
        b->fs = fs;
        b->name = 'Z';
        b->colliding = false;

        return b;
 error:
        return NULL;
}

/* Create a L block in the default position */
block_t* newL() {
        block_t* b = malloc(sizeof(block_t));
        check_mem(b);

        Fruit* fs = randFruits();
        check(fs, "Failed to generate Fruits.");

        b->coords = &lBlock[0][0];
        b->variations = 4;
        b->curr = 0;
        b->x = 0.0f;
        b->y = 0.0f;
        b->fs = fs;
        b->name = 'L';
        b->colliding = false;

        return b;
 error:
        return NULL;
}

/* Create a O block (a square) in the default position */
block_t* newO() {
        block_t* b = malloc(sizeof(block_t));
        check_mem(b);

        Fruit* fs = randFruits();
        check(fs, "Failed to generate Fruits.");

        b->coords = &oBlock[0][0];
        b->variations = 1;
        b->curr = 0;
        b->x = 0.0f;
        b->y = 0.0f;
        b->fs = fs;
        b->name = 'O';
        b->colliding = false;

        return b;
 error:
        return NULL;
}

/* Generate four random Fruits */
Fruit* randFruits() {
        int i;
        Fruit* fs = malloc(sizeof(Fruit) * 4);
        check_mem(fs);

        // There are five Fruit types available, so we mod5.
        for(i = 0; i < 4; i++) {
                fs[i] = (rand() % 5) + 1;
        }
 error:
        return fs;
}

/* Get the colour of a Fruit. Cannot fail */
GLfloat* fruitColour(Fruit f) {
        GLfloat* colour = NULL;

        switch(f) {
        case Grape:
                colour = purple;
                break;
        case Apple:
                colour = red;
                break;
        case Banana:
                colour = yellow;
                break;
        case Pear:
                colour = green;
                break;
        case Orange:
                colour = orange;
                break;
        default:
                colour = black;  // You should never see this!
        }

        return colour;
}

/* Coordinate/Colour Data of a given Block */
GLfloat* blockCoords(block_t* block) {
        GLfloat* temp1;
        GLfloat* temp2;
        GLfloat* cs = NULL;

        check(block, "Null Block given.");

        // 4 cells, each has 36 vertices of 6 data points each.
        //GLfloat* cs = malloc(sizeof(GLfloat) * 4 * 36 * 6);
        //check_mem(cs);

        // Coords and colours for each cell.
        GLfloat* a = cellCoords(block->x, block->y,
                                block->coords[0],block->coords[1],
                                block->fs[0],
                                block->colliding);
        GLfloat* b = cellCoords(block->x, block->y,
                                block->coords[2],block->coords[3],
                                block->fs[1],
                                block->colliding);
        GLfloat* c = cellCoords(block->x, block->y,
                                0,0,
                                block->fs[2],
                                block->colliding);
        GLfloat* d = cellCoords(block->x, block->y,
                                block->coords[4],block->coords[5],
                                block->fs[3],
                                block->colliding);

        check(a && b && c && d, "Couldn't get Cell coordinates.");

         // Construct return value
        temp1 = append(a, CELL_FLOATS, b, CELL_FLOATS);
        temp2 = append(c, CELL_FLOATS, d, CELL_FLOATS);
        cs    = append(temp1, CELL_FLOATS * 2, temp2, CELL_FLOATS * 2);
        check(cs, "Couldn't construct final list of coords/colours.");

        free(temp1); free(temp2);
        free(a); free(b); free(c); free(d);

        return cs;
 error:
        if(cs) { free(cs); }
        return NULL;
}

/* Coordinate/Colour Data for a Cell of the Block
 * x:    x-coord of C Cell in World Space
 * y:    y-coord of C Cell in World Space
 * xoff: Block-space x-offset from C for this Cell
 * yoff: Block-space y-offset from C for this Cell
 */
GLfloat* cellCoords(GLfloat x, GLfloat y, GLint xoff, GLint yoff, Fruit f, bool colliding) {
        GLfloat* coords = NULL;
        GLfloat* colour = fruitColour(f);
        GLuint i;
        GLfloat c[] = { colour[0], colour[1], colour[2] };

        // If the Block is colliding, colour it gray.
        if(colliding) {
                c[0] = 0.5; c[1] = 0.5; c[2] = 0.5;
        }

        GLfloat temp[CELL_FLOATS] = {
                // Back T1
                x-16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                x-16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                // Back T2
                x-16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                x+16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,0,-1,
                // Front T1
                x-16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                x+16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                // Front T2
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                x+16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                x+16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,0,1,
                // Left T1
                x-16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],-1,0,0,
                x-16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],-1,0,0,
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],-1,0,0,
                // Left T2
                x-16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],-1,0,0,
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],-1,0,0,
                x-16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],-1,0,0,
                // Right T1
                x+16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],1,0,0,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],1,0,0,
                x+16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],1,0,0,
                // Right T2
                x+16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],1,0,0,
                x+16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],1,0,0,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],1,0,0,
                // Top T1
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,1,0,
                x+16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,1,0,
                x+16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,1,0,
                // Top T2
                x-16.5+33*xoff,y+16.5+33*yoff,16.5,c[0],c[1],c[2],0,1,0,
                x-16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,1,0,
                x+16.5+33*xoff,y+16.5+33*yoff,-16.5,c[0],c[1],c[2],0,1,0,
                // Bottom T1
                x-16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,-1,0,
                x+16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,-1,0,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,-1,0,
                // Bottom T2
                x-16.5+33*xoff,y-16.5+33*yoff,16.5,c[0],c[1],c[2],0,-1,0,
                x-16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,-1,0,
                x+16.5+33*xoff,y-16.5+33*yoff,-16.5,c[0],c[1],c[2],0,-1,0
        };

        coords = malloc(sizeof(GLfloat) * CELL_FLOATS);
        check_mem(coords);

        if(f == None) {
                // Nullify all the coordinates
                for(i = 0; i < CELL_FLOATS; i++) {
                        temp[i] = 0.0;
                }
        }

        // Copy values.
        for(i = 0; i < CELL_FLOATS; i++) {
                coords[i] = temp[i];
        }

        return coords;
 error:
        return NULL;
}

/* Finds the nearest Grid Cell (x,y) location to the given Block */
matrix_t* nearestCell(block_t* b, matrix_t** centers) {
        GLuint i;
        GLfloat curr;
        GLfloat currMin = 1000000;  // Arbitrarily large.
        GLuint minLoc = 0;
        GLfloat xdiff;
        GLfloat ydiff;

        check(b, "Null Block given.");
        check(centers, "Null list of Grid Cell centers given.");

        // Assumption: `centers` has 200 elements.
        for(i = 0; i < 200; i++) {
                xdiff = b->x - centers[i]->m[0];
                ydiff = b->y - centers[i]->m[1];

                curr = sqrt(xdiff*xdiff + ydiff*ydiff);

                if(curr < currMin) {
                        currMin = curr;
                        minLoc = i;
                }
        }

        return coglV2(centers[minLoc]->m[2],centers[minLoc]->m[3]);
 error:
        return NULL;
}

/* Generate a random Block */
block_t* randBlock() {
        block_t* b = NULL;
        int choice = rand() % 5;

        switch(choice) {
        case 0:
                b = newL();
                break;
        case 1:
                b = newS();
                break;
        case 2:
                b = newZ();
                break;
        case 3:
                b = newO();
                break;
        default:
                b = newI();
        }

        check(b, "Failed to randomly generate a Block.");

        return b;
 error:
        return NULL;
}

/* Rotate a Block to its next configuration */
block_t* rotateBlock(block_t* b) {
        check(b, "Null Block given.");

        b->curr = (b->curr + 1) % b->variations;

        switch(b->name) {
        case 'I':
                b->coords = &iBlock[b->curr][0];
                break;
        case 'S':
                b->coords = &sBlock[b->curr][0];
                break;
        case 'Z':
                b->coords = &zBlock[b->curr][0];
                break;
        case 'O':
                // O block doesn't rotate.
                break;
        default:
                b->coords = &lBlock[b->curr][0];
        }

        return b;
 error:
        return NULL;
}

/* Yield a list of grid-space coordinates relative to (x,y) and the current
 * Block's shape */
GLint* blockCells(block_t* b, GLuint x, GLuint y) {
        GLint* cells = NULL;

        check(b, "Null Block given.");

        cells = malloc(sizeof(GLint) * 8);
        check_mem(cells);
        
        cells[0] = x + b->coords[0]; cells[1] = y + b->coords[1];
        cells[2] = x + b->coords[2]; cells[3] = y + b->coords[3];
        cells[4] = x;                cells[5] = y;
        cells[6] = x + b->coords[4]; cells[7] = y + b->coords[5];
        
        return cells;
 error:
        return NULL;
}

/* Shuffle the order of the fruits */
block_t* shuffleFruit(block_t* b) {
        check(b, "Null Block given.");

        Fruit d = b->fs[3];
        b->fs[3] = b->fs[2];
        b->fs[2] = b->fs[1];
        b->fs[1] = b->fs[0];
        b->fs[0] = d;

        return b;
 error:
        return NULL;
}

/* Copy a Block */
block_t* copyBlock(block_t* b) {
        block_t* newB = NULL;
        int i;

        check(b, "Null Block given.");

        newB = malloc(sizeof(block_t));
        check_mem(newB);

        newB->coords = b->coords;
        newB->variations = b->variations;
        newB->curr = b->curr;
        newB->x = b->x;
        newB->y = b->y;
        newB->fs = malloc(sizeof(Fruit) * 4);
        check_mem(newB->fs);
        
        for(i = 0; i < 4; i++) {
                newB->fs[i] = b->fs[i];
        }

        newB->name = b->name;
        newB->colliding = b->colliding;

        return newB;
 error:
        return NULL;
}

/* Deallocate a Block */
void destroyBlock(block_t* b) {
        if(b) {
                free(b->fs);
                free(b);
        }
}
