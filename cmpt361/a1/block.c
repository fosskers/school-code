#include <stdlib.h>

#include "block.h"
#include "cog/dbg.h"

// --- //

/* All coordinates for all blocks in "Block Space".
 * Values come in pairs that represent the positions of cell
 * A, B, and D in that order. C is always at (0,0) in Block Space,
 * so we need not list it here.
 */
int iBlock[2][6] = {{ -2,0, -1,0, 1,0  },
                    { 0,-2, 0,-1, 0,1  }};
int sBlock[2][6] = {{ -1,-1, 0,-1, 1,0 },
                    { 1,-1, 1,0, 0,1   }};
int lBlock[4][6] = {{ -1,-1, -1,0, 1,0 },
                    { 1,-1, 0,-1, 0,1  },
                    { 1,1, 1,0, -1,0   },
                    { -1,1, 0,1, 0,-1  }};

// Fruit Colours
GLfloat black[]  = { 0.0, 0.0, 0.0 };
GLfloat purple[] = { 1.0, 0.0, 1.0 };
GLfloat red[]    = { 1.0, 0.0, 0.0 };
GLfloat yellow[] = { 1.0, 1.0, 0.0 };
GLfloat green[]  = { 0.0, 1.0, 0.0 };
GLfloat orange[] = { 1.0, 0.5, 0.0 };

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
        b->x = 5;
        b->y = 19;
        b->fs = fs;
        b->name = 'I';

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
        b->x = 5;
        b->y = 19;
        b->fs = fs;
        b->name = 'S';

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
        b->x = 5;
        b->y = 19;
        b->fs = fs;
        b->name = 'L';

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

/* Generate a random Block */
block_t* randBlock() {
        block_t* b = NULL;
        int choice = rand() % 3;

        switch(choice) {
        case 0:
                b = newI();
                break;
        case 1:
                b = newS();
                break;
        default:
                b = newL();
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
        default:
                b->coords = &lBlock[b->curr][0];
        }

        return b;
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
