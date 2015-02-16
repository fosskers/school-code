#ifndef __block_h__
#define __block_h__

#include <GL/glew.h>

typedef enum { Grape, Apple, Banana, Pear, Orange } Fruit;

typedef struct block_t {
        // Block Shape/Rotation
        int* coords;     // Set of all "Block Space" coordinates
        int variations;  // How many "images" does this block have?
        int curr;        // Which variation are we on?
        // Block's grid coordinates (coord of C block)
        int x;
        int y;
        // Block Colours
        Fruit* fs;
        // Which Block is it?
        char name;
} block_t;

// --- //

/* Create a I block in the default position */
block_t* newI();

/* Create a S block in the default position */
block_t* newS();

/* Create a L block in the default position */
block_t* newL();

/* Generate four random Fruits */
Fruit* randFruits();

/* Get the colour of a Fruit. Cannot fail */
GLfloat* fruitColour(Fruit f);

/* Generate a random Block */
block_t* randBlock();

/* Deallocate a Block */
void destroyBlock(block_t* b);

#endif
