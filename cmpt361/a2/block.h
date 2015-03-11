#ifndef __block_h__
#define __block_h__

#include <GL/glew.h>
#include "cog/linalg/linalg.h"

// --- //

typedef enum { None, Grape, Apple, Banana, Pear, Orange } Fruit;

typedef struct block_t {
        // Block Shape/Rotation
        GLint* coords;     // Set of all "Block Space" coordinates
        GLint variations;  // How many "images" does this block have?
        GLint curr;        // Which variation are we on?
        // Block's World-space coordinates (coord of C block)
        GLfloat x;
        GLfloat y;
        // Block Colours
        Fruit* fs;
        // Which Block is it?
        char name;
        // Is the Block colliding with anything, or outside the Board?
        bool colliding;
} block_t;

// --- //

/* Create a I block in the default position */
block_t* newI();

/* Create a S block in the default position */
block_t* newS();

/* Create a Z block in the default position */
block_t* newZ();

/* Create a L block in the default position */
block_t* newL();

/* Create a O block (a square) in the default position */
block_t* newO();

/* Generate four random Fruits */
Fruit* randFruits();

/* Get the colour of a Fruit. Cannot fail */
GLfloat* fruitColour(Fruit f);

/* Coordinate/Colour Data of a given Block */
GLfloat* blockCoords(block_t* b);

/* Coordinate/Colour Data for a Cell of the Block
 * x:    x-coord of C Cell in World Space
 * y:    y-coord of C Cell in World Space
 * xoff: Block-space x-offset from C for this Cell
 * yoff: Block-space y-offset from C for this Cell
 */
GLfloat* cellCoords(GLfloat x, GLfloat y, GLint xoff, GLint yoff, Fruit f, bool colliding);

/* Finds the nearest Grid Cell (x,y) location to the given Block */
matrix_t* nearestCell(block_t* b, matrix_t** centers);

/* Generate a random Block */
block_t* randBlock();

/* Rotate a Block to its next configuration */
block_t* rotateBlock(block_t* b);

/* Yield a list of grid-space coordinates relative to (x,y) and the current
 * Block's shape */
GLint* blockCells(block_t* b, GLuint x, GLuint y);

/* Shuffle the order of the fruits */
block_t* shuffleFruit(block_t* b);

/* Copy a Block */
block_t* copyBlock(block_t* b);

/* Deallocate a Block */
void destroyBlock(block_t* b);

#endif
