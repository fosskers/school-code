#ifndef __board_h__
#define __board_h__

#include <GL/glew.h>
#include "material.h"
#include "cog/linalg/linalg.h"

// --- //

typedef struct {
        matrix_t* normal;
        matrix_t* origin;
        Material* mat;
} Board;

/* Create a new Chess board */
Board* newBoard(matrix_t* n, matrix_t* o, Material* m);

/* Free a Board's memory */
void destroyBoard(Board* b);

/* If a Ray hits a Board, how far away is the contact Point? */
GLfloat scalar_to_board(Board* s, matrix_t* eye, matrix_t* ray);

#endif
