#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
//#include <ft2build.h>
//#include FT_FREETYPE_H

#include "block.h"
#include "cog/camera/camera.h"
#include "cog/dbg.h"
#include "cog/shaders/shaders.h"
#include "collision.h"
#include "defines.h"
#include "util.h"

// --- //

void clearBoard();
void fruitCheck();
void initBoard();
void lineCheck();
void newBlock();
void placeBlock();
void refreshBlock();
int refreshBoard();
matrix_t* rotateLShaft(int);
matrix_t* rotateUShaft(int);
matrix_t* resetShaft(matrix_t*);

// --- //

// Global state info
bool gameOver = false;
bool running  = true;
bool keys[1024];

// Buffer Objects
GLuint gVAO;  // Grid
GLuint gVBO;
GLuint bVAO;  // Block
GLuint bVBO;
GLuint fVAO;  // Board's Fruits
GLuint fVBO;
GLuint aVAO[3];  // Robot Arm
GLuint aVBO[3];
GLuint lVAO;  // Lamp
GLuint lVBO;
GLuint cVAO;  // Text
GLuint cVBO;

// Transformable Model Matrices
matrix_t* lModel = NULL;
matrix_t* uModel = NULL;
GLfloat lShaftAngle = 0.0f;
GLfloat uShaftAngle = 0.0f;

// Timing Info
GLfloat deltaTime = 0.0f;
GLfloat lastFrame = 0.0f;
GLfloat keyDelta  = 0.0f;
GLfloat lastKey   = 0.0f;

/* Needed to calculate to distance between the Block's current position
 * in World Space and the center of the nearest Grid Cell.
 */
matrix_t* cellCenters[BOARD_CELLS];

camera_t* camera;
matrix_t* view;
block_t*  block;   // The Tetris block.
Fruit board[BOARD_CELLS];  // The Board, represented as Fruits.

// --- //

/* Init/Reset the Camera */
void resetCamera() {
        matrix_t* camPos = coglV3(0,0,4);
        matrix_t* camDir = coglV3(0,0,-1);
        matrix_t* camUp = coglV3(0,1,0);

        camera = cogcCreate(camPos,camDir,camUp);
}

/* Clears the board and starts over */
void resetGame() {
        debug("Restarting game.");

        lShaftAngle = 0.0f;
        uShaftAngle = 0.0f;
        lModel = resetShaft(lModel);
        uModel = resetShaft(uModel);
        initBoard();
        newBlock();
}

void newBlock() {
        block = randBlock();
        refreshBlock();
}

void refreshBlock() {
        matrix_t* nearest = nearestCell(block,cellCenters);
        block->colliding = isColliding(block,board,nearest);

        GLfloat* coords = blockCoords(block);

        glBindVertexArray(bVAO);
        glBindBuffer(GL_ARRAY_BUFFER, bVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, 
                        CELL_FLOATS * 4 * sizeof(GLfloat), coords);
        glBindVertexArray(0);

        free(coords);  // Necessary?
}

/* Place the Block into the Board in the nearest Cell */
void placeBlock() {
        GLuint i,j;
        GLint* cells;
        matrix_t* cell = nearestCell(block, cellCenters);
        check(cell, "Could't find a nearest Cell!");

        block->colliding = isColliding(block,board,cell);

        if(!(block->colliding)) {
                cells = blockCells(block,
                                   (GLuint)(cell->m[0]),
                                   (GLuint)(cell->m[1]));

                // Add the Block's cells to the master Board
                for(i = 0,j=0; i < 8; i+=2,j++) {
                        board[cells[i] + 10*cells[i+1]] = block->fs[j];
                }

                lineCheck();
                fruitCheck();
                refreshBoard();
                newBlock();
        }

 error:
        return;
}

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        GLfloat currentTime = glfwGetTime();
        static double x = 1;

        // Update key timing.
        keyDelta = currentTime - lastKey;
        lastKey  = currentTime;

        if(action == GLFW_PRESS ||
           (action == GLFW_REPEAT && keyDelta > 0.01)) {
                keys[key] = true;

                if(keys[GLFW_KEY_Q]) {
                        glfwSetWindowShouldClose(w, GL_TRUE);
                } else if(key == GLFW_KEY_P) {
                        if(running) {
                                log_info("Paused.");
                                running = false;
                        } else {
                                log_info("Unpaused.");
                                running = true;
                        }
                } else if(key == GLFW_KEY_C) {
                        resetCamera();
                } else if(key == GLFW_KEY_R) {
                        resetGame();
                } else if(keys[GLFW_KEY_UP]) {
                        block = rotateBlock(block);
                        refreshBlock();
                } else if(keys[GLFW_KEY_LEFT_CONTROL] &&
                          keys[GLFW_KEY_LEFT]) {
                        // Pan left.
                        x += 5;
                        cogcPan(camera,x,0);
                } else if(keys[GLFW_KEY_LEFT_CONTROL] &&
                          keys[GLFW_KEY_RIGHT]) {
                        // Pan right.
                        x -= 5;
                        cogcPan(camera,x,0);
                } else if(keys[GLFW_KEY_A]) {
                        lModel = rotateLShaft(1);
                        refreshBlock();
                } else if(keys[GLFW_KEY_D]) {
                        lModel = rotateLShaft(-1);
                        refreshBlock();
                } else if(keys[GLFW_KEY_W]) {
                        uModel = rotateUShaft(1);
                        refreshBlock();
                } else if(keys[GLFW_KEY_S]) {
                        uModel = rotateUShaft(-1);
                        refreshBlock();
                } else if(keys[GLFW_KEY_SPACE]) {
                        placeBlock();
                }
        } else if(action == GLFW_RELEASE) {
                keys[key] = false;
        }
}

void mouse_callback(GLFWwindow* w, double xpos, double ypos) {
        cogcPan(camera,xpos,ypos);
}

GLfloat* gridLocToCoords(int x, int y, Fruit f) {
        GLfloat* coords = NULL;
        GLfloat* c      = fruitColour(f);
        GLuint i;

        GLfloat temp[CELL_FLOATS] = {
                // Back T1
                33 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1,
                33 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1,
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1,
                // Back T2
                33 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1, 
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1,
                66 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 0, -1,
                // Front T1
                33 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                66 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                // Front T2
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                66 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                66 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2], 0, 0, 1,
                // Left T1
                33 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], -1, 0, 0,
                33 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], -1, 0, 0,
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  -1, 0, 0,
                // Left T2
                33 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], -1, 0, 0,
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  -1, 0, 0,
                33 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2],  -1, 0, 0,
                // Right T1
                66 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 1, 0, 0,
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 1, 0, 0,
                66 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  1, 0, 0,
                // Right T2
                66 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  1, 0, 0,
                66 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2],  1, 0, 0,
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 1, 0, 0,
                // Top T1
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  0, 1, 0,
                66 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  0, 1, 0,
                66 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 1, 0,
                // Top T2
                33 + x*33, 66 + y*33, 16.5, c[0], c[1], c[2],  0, 1, 0,
                33 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 1, 0,
                66 + x*33, 66 + y*33, -16.5, c[0], c[1], c[2], 0, 1, 0,
                // Bottom T1
                33 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2],  0, -1, 0,
                66 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2],  0, -1, 0,
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, -1, 0,
                // Bottom T2
                33 + x*33, 33 + y*33, 16.5, c[0], c[1], c[2],  0, -1, 0,
                33 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, -1, 0,
                66 + x*33, 33 + y*33, -16.5, c[0], c[1], c[2], 0, -1, 0
        };

        check(x > -1 && x < 10 &&
              y > -1 && x < 20,
              "Invalid coords given.");

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

/* Initialize the Block */
int initBlock() {
        block = randBlock();
        check(block, "Failed to initialize first Block.");
        debug("Got a: %c", block->name);

        debug("Initializing Block.");

        GLfloat* coords = blockCoords(block);
        
        // Set up VAO/VBO
        glGenVertexArrays(1,&bVAO);
        glBindVertexArray(bVAO);
        glGenBuffers(1,&bVBO);
        glBindBuffer(GL_ARRAY_BUFFER,bVBO);
        glBufferData(GL_ARRAY_BUFFER,
                     CELL_FLOATS * 4 * sizeof(GLfloat),coords,
                     GL_DYNAMIC_DRAW);

        // Tell OpenGL how to process Block Vertices
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),
                              (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),
                              (GLvoid*)(6 * sizeof(GLfloat)));
        glEnableVertexAttribArray(2);
        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        return 1;
 error:
        return 0;
}

/* Initialize the Grid */
// Insert TRON pun here.
void initGrid(matrix_t* t) {
        GLfloat gridPoints[768];  // Contains colour info as well.
        matrix_t* temp1 = NULL;
        matrix_t* temp2 = NULL;
        GLuint i,j;

        debug("Initializing Grid.");

        // Back Vertical lines
	for (i = 0; i < 11; i++) {
                // Bottom coord
		gridPoints[12*i]     = 33.0 + (33.0 * i);
                gridPoints[12*i + 1] = 33.0;
                gridPoints[12*i + 2] = -16.5;
                // Bottom normal
                gridPoints[12*i + 3] = 0;
                gridPoints[12*i + 4] = 0;
                gridPoints[12*i + 5] = 1;
                // Top coord
		gridPoints[12*i + 6] = 33.0 + (33.0 * i);
                gridPoints[12*i + 7] = 693.0;
                gridPoints[12*i + 8] = -16.5;
                // Top normal
                gridPoints[12*i + 9]  = 0;
                gridPoints[12*i + 10] = 0;
                gridPoints[12*i + 11] = 1;
	}

	// Back Horizontal lines
	for (i = 0; i < 21; i++) {
                // Left coord
		gridPoints[132 + 12*i]     = 33.0;
                gridPoints[132 + 12*i + 1] = 33.0 + (33.0 * i);
                gridPoints[132 + 12*i + 2] = -16.5;
                // Left normal
                gridPoints[132 + 12*i + 3] = 0;
                gridPoints[132 + 12*i + 4] = 0;
                gridPoints[132 + 12*i + 5] = 1;
                // Right coord
		gridPoints[132 + 12*i + 6] = 363.0;
                gridPoints[132 + 12*i + 7] = 33.0 + (33.0 * i);
                gridPoints[132 + 12*i + 8] = -16.5;
                // Right normal
                gridPoints[132 + 12*i + 9]  = 0;
                gridPoints[132 + 12*i + 10] = 0;
                gridPoints[132 + 12*i + 11] = 1;
	}

        // Front Vertical lines
	for (i = 0; i < 11; i++) {
                // Bottom coord
		gridPoints[384 + 12*i]     = 33.0 + (33.0 * i);
                gridPoints[384 + 12*i + 1] = 33.0;
                gridPoints[384 + 12*i + 2] = 16.5;
                // Bottom normal
                gridPoints[384 + 12*i + 3] = 0;
                gridPoints[384 + 12*i + 4] = 0;
                gridPoints[384 + 12*i + 5] = 1;
                // Top coord
		gridPoints[384 + 12*i + 6] = 33.0 + (33.0 * i);
                gridPoints[384 + 12*i + 7] = 693.0;
                gridPoints[384 + 12*i + 8] = 16.5;
                // Top normal
                gridPoints[384 + 12*i + 9]  = 0;
                gridPoints[384 + 12*i + 10] = 0;
                gridPoints[384 + 12*i + 11] = 1;
	}

	// Front Horizontal lines
	for (i = 0; i < 21; i++) {
                // Left coord
		gridPoints[516 + 12*i]     = 33.0;
                gridPoints[516 + 12*i + 1] = 33.0 + (33.0 * i);
                gridPoints[516 + 12*i + 2] = 16.5;
                // Left normal
                gridPoints[516 + 12*i + 3] = 0;
                gridPoints[516 + 12*i + 4] = 0;
                gridPoints[516 + 12*i + 5] = 1;
                // Right coord
		gridPoints[516 + 12*i + 6] = 363.0;
                gridPoints[516 + 12*i + 7] = 33.0 + (33.0 * i);
                gridPoints[516 + 12*i + 8] = 16.5;
                // Right normal
                gridPoints[516 + 12*i + 9]  = 0;
                gridPoints[516 + 12*i + 10] = 0;
                gridPoints[516 + 12*i + 11] = 1;
	}

        // Set up list of Board Cell Centers
        for(j = 0; j < 20; j++) {
                for(i = 0; i < 10; i++) {
                        temp1 = coglV4(49.5+33*i,49.6+33*j,0,1);
                        // Scale and translate the vertex to World Space.
                        temp2 = coglMMultiplyP(t,temp1);

                        cellCenters[j*10 + i] = coglV4(temp2->m[0],
                                                       temp2->m[1],
                                                       i,
                                                       j);

                        coglMDestroy(temp1);
                        coglMDestroy(temp2);
                }
        }

        // Set up VAO/VBO
        glGenVertexArrays(1,&gVAO);
        glBindVertexArray(gVAO);
        glGenBuffers(1,&gVBO);
        glBindBuffer(GL_ARRAY_BUFFER, gVBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(gridPoints),
                     gridPoints,GL_STATIC_DRAW);

        // Tell OpenGL how to process Grid Vertices
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                              6 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              6 * sizeof(GLfloat),
                              (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);
}

/* Initialize the game board */
void initBoard() {
        debug("Initializing Board.");

        GLfloat temp[TOTAL_FLOATS];
        
        // Set up VAO/VBO
        glGenVertexArrays(1,&fVAO);
        glBindVertexArray(fVAO);
        glGenBuffers(1,&fVBO);
        glBindBuffer(GL_ARRAY_BUFFER,fVBO);
        glBufferData(GL_ARRAY_BUFFER, 
                     TOTAL_FLOATS * sizeof(GLfloat),
                     temp,
                     GL_DYNAMIC_DRAW);
        
        // Vertex Location
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        // Vertex Colour
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),
                              (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        // Vertex Normal
        glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,
                              9 * sizeof(GLfloat),
                              (GLvoid*)(6 * sizeof(GLfloat)));
        glEnableVertexAttribArray(2);
        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        
        clearBoard();
}

/* Set up a debugging lamp to be drawn */
void initLamp() {
        GLfloat verts[] = {
                -0.5f, -0.5f, -0.5f,
                0.5f, -0.5f, -0.5f,
                0.5f,  0.5f, -0.5f,
                0.5f,  0.5f, -0.5f,
                -0.5f,  0.5f, -0.5f,
                -0.5f, -0.5f, -0.5f,

                -0.5f, -0.5f,  0.5f,
                0.5f, -0.5f,  0.5f,
                0.5f,  0.5f,  0.5f,
                0.5f,  0.5f,  0.5f,
                -0.5f,  0.5f,  0.5f,
                -0.5f, -0.5f,  0.5f,

                -0.5f,  0.5f,  0.5f,
                -0.5f,  0.5f, -0.5f,
                -0.5f, -0.5f, -0.5f,
                -0.5f, -0.5f, -0.5f,
                -0.5f, -0.5f,  0.5f,
                -0.5f,  0.5f,  0.5f,

                0.5f,  0.5f,  0.5f,
                0.5f,  0.5f, -0.5f,
                0.5f, -0.5f, -0.5f,
                0.5f, -0.5f, -0.5f,
                0.5f, -0.5f,  0.5f,
                0.5f,  0.5f,  0.5f,

                -0.5f, -0.5f, -0.5f,
                0.5f, -0.5f, -0.5f,
                0.5f, -0.5f,  0.5f,
                0.5f, -0.5f,  0.5f,
                -0.5f, -0.5f,  0.5f,
                -0.5f, -0.5f, -0.5f,

                -0.5f,  0.5f, -0.5f,
                0.5f,  0.5f, -0.5f,
                0.5f,  0.5f,  0.5f,
                0.5f,  0.5f,  0.5f,
                -0.5f,  0.5f,  0.5f,
                -0.5f,  0.5f, -0.5f
        };

        
        glGenVertexArrays(1,&lVAO);
        glBindVertexArray(lVAO);
        glGenBuffers(1,&lVBO);
        glBindBuffer(GL_ARRAY_BUFFER,lVBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(verts),verts,GL_STATIC_DRAW);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                              3 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glBindVertexArray(0);
}

/* Set up the text VAO/VBO */
void initText() {
        glGenVertexArrays(1, &cVAO);
        glGenBuffers(1, &cVBO);
        glBindVertexArray(cVAO);
        glBindBuffer(GL_ARRAY_BUFFER,cVBO);
        glBufferData(GL_ARRAY_BUFFER,
                     sizeof(GLfloat) * 6 * 4,
                     NULL, GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE,
                              4 * sizeof(GLfloat), 0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);    
}

/*
void renderText(ftchar_t** all, GLuint p, const char* s, GLuint len,
                GLfloat x, GLfloat y, GLfloat scale) {
        GLuint i,j;
        ftchar_t* ch;

        glUseProgram(p);
        glUniform3f(glGetUniformLocation(p,"textColour"),0.5,0.8,0.2);
        glActiveTexture(GL_TEXTURE0);
        glBindVertexArray(cVAO);

        for(i = 0; i < len; i++) {
                // Find the Glyph data
                for(j = 0; j < 128; j++) {
                        ch = NULL;

                        if(s[i] == all[j]->c) {
                                ch = all[j];
                                break;
                        }
                }

                check(ch, "Couldn't find character glyph data.");

                GLfloat xpos = x + ch->bearingX * scale;
                GLfloat ypos = y - (ch->sizeY - ch->bearingY) * scale;
                GLfloat w = ch->sizeX * scale;
                GLfloat h = ch->sizeY * scale;

                // Update VBO for each character
                GLfloat vertices[24] = {
                        xpos, ypos + h, 0.0, 0.0,
                        xpos, ypos, 0.0, 1.0,
                        xpos + w, ypos, 1.0, 1.0,

                        xpos,     ypos + h,   0.0, 0.0,
                        xpos + w, ypos,       1.0, 1.0,
                        xpos + w, ypos + h,   1.0, 0.0
                };

                // Render glyph texture over quad
                glBindTexture(GL_TEXTURE_2D, ch->textureID);
                // Update content of VBO memory
                glBindBuffer(GL_ARRAY_BUFFER, cVBO);
                glBufferSubData(GL_ARRAY_BUFFER, 0,
                                sizeof(vertices), vertices);
                glBindBuffer(GL_ARRAY_BUFFER, 0);

                // Render quad
                glDrawArrays(GL_TRIANGLES,0,6);
                x += ((ch->advance) >> 6) * scale;
        }
        
        glBindVertexArray(0);
        glBindTexture(GL_TEXTURE_2D,0);

 error:
        return;
}
*/

/* Move all coloured Cells from the Board */
void clearBoard() {
        int i;

        for(i = 0; i < BOARD_CELLS; i++) {
                board[i] = 0;
        }

        refreshBoard();
}

/* Draw all the coloured Board Cells */
int refreshBoard() {
        GLuint i,j,k;
        GLfloat* cellData;

        //debug("Refreshing Board...");
        
        GLfloat* coords = malloc(sizeof(GLfloat) * TOTAL_FLOATS);
        check_mem(coords);

        for(i = 0; i < BOARD_CELLS; i++) {
                cellData = gridLocToCoords(i % 10, i / 10, board[i]);
                check(cellData, "Couldn't get coord data for Cell.");
                
                for(j = i*CELL_FLOATS, k=0; k < CELL_FLOATS; j++, k++) {
                        coords[j] = cellData[k];
                }
        }

        glBindVertexArray(fVAO);
        glBindBuffer(GL_ARRAY_BUFFER, fVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, 
                        TOTAL_FLOATS * sizeof(GLfloat), coords);
        glBindVertexArray(0);

        free(coords);
        
        return 1;
 error:
        return 0;        
}

/* Initialize the Robot Arm visuals */
void initArm() {
        GLuint i;

        // Location and Normal data
        GLfloat base[216] = {
                // Back Face
                -0.5f, -0.25f, -0.5f,  0.0f,  0.0f, -1.0f,
                0.5f, -0.25f, -0.5f,  0.0f,  0.0f, -1.0f,
                0.5f,  0.25f, -0.5f,  0.0f,  0.0f, -1.0f,
                0.5f,  0.25f, -0.5f,  0.0f,  0.0f, -1.0f,
                -0.5f,  0.25f, -0.5f,  0.0f,  0.0f, -1.0f,
                -0.5f, -0.25f, -0.5f,  0.0f,  0.0f, -1.0f,

                // Front Face
                -0.5f, -0.25f,  0.5f,  0.0f,  0.0f, 1.0f,
                0.5f, -0.25f,  0.5f,  0.0f,  0.0f, 1.0f,
                0.5f,  0.25f,  0.5f,  0.0f,  0.0f, 1.0f,
                0.5f,  0.25f,  0.5f,  0.0f,  0.0f, 1.0f,
                -0.5f,  0.25f,  0.5f,  0.0f,  0.0f, 1.0f,
                -0.5f, -0.25f,  0.5f,  0.0f,  0.0f, 1.0f,

                // Left Face
                -0.5f,  0.25f,  0.5f, -1.0f,  0.0f,  0.0f,
                -0.5f,  0.25f, -0.5f, -1.0f,  0.0f,  0.0f,
                -0.5f, -0.25f, -0.5f, -1.0f,  0.0f,  0.0f,
                -0.5f, -0.25f, -0.5f, -1.0f,  0.0f,  0.0f,
                -0.5f, -0.25f,  0.5f, -1.0f,  0.0f,  0.0f,
                -0.5f,  0.25f,  0.5f, -1.0f,  0.0f,  0.0f,

                // Right Face
                0.5f,  0.25f,  0.5f,  1.0f,  0.0f,  0.0f,
                0.5f,  0.25f, -0.5f,  1.0f,  0.0f,  0.0f,
                0.5f, -0.25f, -0.5f,  1.0f,  0.0f,  0.0f,
                0.5f, -0.25f, -0.5f,  1.0f,  0.0f,  0.0f,
                0.5f, -0.25f,  0.5f,  1.0f,  0.0f,  0.0f,
                0.5f,  0.25f,  0.5f,  1.0f,  0.0f,  0.0f,

                // Bottom Face
                -0.5f, -0.25f, -0.5f,  0.0f, -1.0f,  0.0f,
                0.5f, -0.25f, -0.5f,  0.0f, -1.0f,  0.0f,
                0.5f, -0.25f,  0.5f,  0.0f, -1.0f,  0.0f,
                0.5f, -0.25f,  0.5f,  0.0f, -1.0f,  0.0f,
                -0.5f, -0.25f,  0.5f,  0.0f, -1.0f,  0.0f,
                -0.5f, -0.25f, -0.5f,  0.0f, -1.0f,  0.0f,

                // Top Face
                -0.5f,  0.25f, -0.5f,  0.0f,  1.0f,  0.0f,
                0.5f,  0.25f, -0.5f,  0.0f,  1.0f,  0.0f,
                0.5f,  0.25f,  0.5f,  0.0f,  1.0f,  0.0f,
                0.5f,  0.25f,  0.5f,  0.0f,  1.0f,  0.0f,
                -0.5f,  0.25f,  0.5f,  0.0f,  1.0f,  0.0f,
                -0.5f,  0.25f, -0.5f,  0.0f,  1.0f,  0.0f
        };

        // Lower Shaft of Arm
        GLfloat shaft[216] = {
                -0.1f, -SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,
                0.1f,  -SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,
                0.1f,   SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,
                0.1f,   SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,
                -0.1f,  SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,
                -0.1f, -SHAFT_LEN, -0.1f,  0.0f,  0.0f, -1.0f,

                -0.1f, -SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,
                0.1f,  -SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,
                0.1f,   SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,
                0.1f,   SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,
                -0.1f,  SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,
                -0.1f, -SHAFT_LEN,  0.1f,  0.0f,  0.0f, 1.0f,

                -0.1f,  SHAFT_LEN,  0.1f, -1.0f,  0.0f,  0.0f,
                -0.1f,  SHAFT_LEN, -0.1f, -1.0f,  0.0f,  0.0f,
                -0.1f, -SHAFT_LEN, -0.1f, -1.0f,  0.0f,  0.0f,
                -0.1f, -SHAFT_LEN, -0.1f, -1.0f,  0.0f,  0.0f,
                -0.1f, -SHAFT_LEN,  0.1f, -1.0f,  0.0f,  0.0f,
                -0.1f,  SHAFT_LEN,  0.1f, -1.0f,  0.0f,  0.0f,

                0.1f,  SHAFT_LEN,  0.1f,  1.0f,  0.0f,  0.0f,
                0.1f,  SHAFT_LEN, -0.1f,  1.0f,  0.0f,  0.0f,
                0.1f, -SHAFT_LEN, -0.1f,  1.0f,  0.0f,  0.0f,
                0.1f, -SHAFT_LEN, -0.1f,  1.0f,  0.0f,  0.0f,
                0.1f, -SHAFT_LEN,  0.1f,  1.0f,  0.0f,  0.0f,
                0.1f,  SHAFT_LEN,  0.1f,  1.0f,  0.0f,  0.0f,

                -0.1f, -SHAFT_LEN, -0.1f,  0.0f, -1.0f,  0.0f,
                0.1f,  -SHAFT_LEN, -0.1f,  0.0f, -1.0f,  0.0f,
                0.1f,  -SHAFT_LEN,  0.1f,  0.0f, -1.0f,  0.0f,
                0.1f,  -SHAFT_LEN,  0.1f,  0.0f, -1.0f,  0.0f,
                -0.1f, -SHAFT_LEN,  0.1f,  0.0f, -1.0f,  0.0f,
                -0.1f, -SHAFT_LEN, -0.1f,  0.0f, -1.0f,  0.0f,

                -0.1f, SHAFT_LEN, -0.1f,  0.0f,  1.0f,  0.0f,
                0.1f,  SHAFT_LEN, -0.1f,  0.0f,  1.0f,  0.0f,
                0.1f,  SHAFT_LEN,  0.1f,  0.0f,  1.0f,  0.0f,
                0.1f,  SHAFT_LEN,  0.1f,  0.0f,  1.0f,  0.0f,
                -0.1f, SHAFT_LEN,  0.1f,  0.0f,  1.0f,  0.0f,
                -0.1f, SHAFT_LEN, -0.1f,  0.0f,  1.0f,  0.0f
        };

        // Upper Shaft of Arm
        GLfloat ushaft[216] = {
                -SHAFT_LEN, -0.1f, -0.1f,  0.0f,  0.0f, -1.0f,
                SHAFT_LEN,  -0.1f, -0.1f,  0.0f,  0.0f, -1.0f,
                SHAFT_LEN,   0.1f, -0.1f,  0.0f,  0.0f, -1.0f,
                SHAFT_LEN,   0.1f, -0.1f,  0.0f,  0.0f, -1.0f,
                -SHAFT_LEN,  0.1f, -0.1f,  0.0f,  0.0f, -1.0f,
                -SHAFT_LEN, -0.1f, -0.1f,  0.0f,  0.0f, -1.0f,

                -SHAFT_LEN, -0.1f,  0.1f,  0.0f,  0.0f, 1.0f,
                SHAFT_LEN,  -0.1f,  0.1f,  0.0f,  0.0f, 1.0f,
                SHAFT_LEN,   0.1f,  0.1f,  0.0f,  0.0f, 1.0f,
                SHAFT_LEN,   0.1f,  0.1f,  0.0f,  0.0f, 1.0f,
                -SHAFT_LEN,  0.1f,  0.1f,  0.0f,  0.0f, 1.0f,
                -SHAFT_LEN, -0.1f,  0.1f,  0.0f,  0.0f, 1.0f,

                -SHAFT_LEN,  0.1f,  0.1f, -1.0f,  0.0f,  0.0f,
                -SHAFT_LEN,  0.1f, -0.1f, -1.0f,  0.0f,  0.0f,
                -SHAFT_LEN, -0.1f, -0.1f, -1.0f,  0.0f,  0.0f,
                -SHAFT_LEN, -0.1f, -0.1f, -1.0f,  0.0f,  0.0f,
                -SHAFT_LEN, -0.1f,  0.1f, -1.0f,  0.0f,  0.0f,
                -SHAFT_LEN,  0.1f,  0.1f, -1.0f,  0.0f,  0.0f,

                SHAFT_LEN,  0.1f,  0.1f,  1.0f,  0.0f,  0.0f,
                SHAFT_LEN,  0.1f, -0.1f,  1.0f,  0.0f,  0.0f,
                SHAFT_LEN, -0.1f, -0.1f,  1.0f,  0.0f,  0.0f,
                SHAFT_LEN, -0.1f, -0.1f,  1.0f,  0.0f,  0.0f,
                SHAFT_LEN, -0.1f,  0.1f,  1.0f,  0.0f,  0.0f,
                SHAFT_LEN,  0.1f,  0.1f,  1.0f,  0.0f,  0.0f,

                -SHAFT_LEN, -0.1f, -0.1f,  0.0f, -1.0f,  0.0f,
                SHAFT_LEN,  -0.1f, -0.1f,  0.0f, -1.0f,  0.0f,
                SHAFT_LEN,  -0.1f,  0.1f,  0.0f, -1.0f,  0.0f,
                SHAFT_LEN,  -0.1f,  0.1f,  0.0f, -1.0f,  0.0f,
                -SHAFT_LEN, -0.1f,  0.1f,  0.0f, -1.0f,  0.0f,
                -SHAFT_LEN, -0.1f, -0.1f,  0.0f, -1.0f,  0.0f,

                -SHAFT_LEN, 0.1f, -0.1f,  0.0f,  1.0f,  0.0f,
                SHAFT_LEN,  0.1f, -0.1f,  0.0f,  1.0f,  0.0f,
                SHAFT_LEN,  0.1f,  0.1f,  0.0f,  1.0f,  0.0f,
                SHAFT_LEN,  0.1f,  0.1f,  0.0f,  1.0f,  0.0f,
                -SHAFT_LEN, 0.1f,  0.1f,  0.0f,  1.0f,  0.0f,
                -SHAFT_LEN, 0.1f, -0.1f,  0.0f,  1.0f,  0.0f
        };

        debug("Initializing Robot Arm.");

        // Set up Base VAO/VBO
        glGenVertexArrays(1,&aVAO[0]);
        glBindVertexArray(aVAO[0]);
        glGenBuffers(1,&aVBO[0]);
        glBindBuffer(GL_ARRAY_BUFFER, aVBO[0]);
        glBufferData(GL_ARRAY_BUFFER,sizeof(base),
                     base,GL_STATIC_DRAW);

        // Set up Lower Shaft VAO/VBO
        glGenVertexArrays(1,&aVAO[1]);
        glBindVertexArray(aVAO[1]);
        glGenBuffers(1,&aVBO[1]);
        glBindBuffer(GL_ARRAY_BUFFER, aVBO[1]);
        glBufferData(GL_ARRAY_BUFFER,sizeof(shaft),
                     shaft,GL_STATIC_DRAW);

        // Set up Upper Shaft VAO/VBO
        glGenVertexArrays(1,&aVAO[2]);
        glBindVertexArray(aVAO[2]);
        glGenBuffers(1,&aVBO[2]);
        glBindBuffer(GL_ARRAY_BUFFER, aVBO[2]);
        glBufferData(GL_ARRAY_BUFFER,sizeof(ushaft),
                     ushaft,GL_STATIC_DRAW);
        
        for(i = 0; i < 3; i++) {
                glBindVertexArray(aVAO[i]);
                glBindBuffer(GL_ARRAY_BUFFER, aVBO[i]);

                // Tell OpenGL how to process Grid Vertices
                glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,
                                      6 * sizeof(GLfloat),(GLvoid*)0);
                glEnableVertexAttribArray(0);
                glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                                      6 * sizeof(GLfloat),
                                      (GLvoid*)(3 * sizeof(GLfloat)));
                glEnableVertexAttribArray(1);
                glBindVertexArray(0);  // Reset the VAO binding.
                glBindBuffer(GL_ARRAY_BUFFER, 0);
        }
}

/* `dir` should be -1 or 1 */
matrix_t* rotateLShaft(int dir) {
        /* Reset lModel */
        resetShaft(lModel);
        lShaftAngle += dir * tau/64;

        if(lShaftAngle > tau/4) {
                lShaftAngle = tau/4;
        } else if(lShaftAngle < -tau/4) {
                lShaftAngle = -tau/4;
        }

        return coglM4Rotate(lModel, lShaftAngle, 0,0,1);
}

/* TODO: Unnecessary repeated code? */
matrix_t* rotateUShaft(int dir) {
        /* Reset uModel */
        resetShaft(uModel);
        uShaftAngle += dir * tau/64;

        if(uShaftAngle > tau/3) {
                uShaftAngle = tau/3;
        } else if(uShaftAngle < -tau/3) {
                uShaftAngle = -tau/3;
        }

        return coglM4Rotate(uModel, uShaftAngle, 0,0,1);
}

/* Reset a shaft's Model Matrix to its default values */
matrix_t* resetShaft(matrix_t* s) {
        if(s) { coglMDestroy(s); }
        s = coglMIdentity(4);
        return coglM4Translate(s,-1.1,-1.25,0);
}

/* Removes any solid lines, if it can */
void lineCheck() {
        int i,j;
        bool fullRow = true;

        for(i = 0; i < BOARD_CELLS; i+=10) {
                fullRow = true;

                // Check for full row
                for(j = 0; j < 10; j++) {
                        if(board[i + j] == None) {
                                fullRow = false;
                                break;
                        }
                }

                if(fullRow) {
                        log_info("Filled a full row!");
                        // Empty the row
                        for(j = 0; j < 10; j++) {
                                board[i + j] = None;
                        }

                        break;
                }
        }
}

/* Removes sets of 3 matching Fruits, if it can */
void fruitCheck() {
        int i,j;
        Fruit curr;
        Fruit streakF = None;
        int streakN;

        // Check columns
        for(i = 0; i < 10; i++) {
                streakN = 1;

                for(j = 0; j < 20; j++) {
                        curr = board[i + j*10];

                        if(curr != None && curr == streakF) {
                                streakN++;

                                if(streakN == 3) {
                                        board[i + j*10] = None;
                                        board[i + (j-1)*10] = None;
                                        board[i + (j-2)*10] = None;

                                        log_info("Matched Fruit!");

                                        break;
                                }
                        } else {
                                streakF = curr;
                                streakN = 1;
                        }
                }
        }

        // Check rows
        for(j = 0; j < 20; j++) {
                streakN = 1;

                for(i = 0; i < 10; i++) {
                        curr = board[i + j*10];

                        if(curr != None && curr == streakF) {
                                streakN++;

                                if(streakN == 3) {
                                        board[i + j*10] = None;
                                        board[i-1 + j*10] = None;
                                        board[i-2 + j*10] = None;

                                        log_info("Matched Fruit!");
                                }
                        } else {
                                streakF = curr;
                                streakN = 1;
                        }
                }
        }
}

int main(int argc, char** argv) {
        /* Initial settings */
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
        
        /* Window creation */
        GLFWwindow* w = glfwCreateWindow(wWidth,wHeight,"Fetris",NULL,NULL);
        glfwMakeContextCurrent(w);

        /* GLEW settings */
        glewExperimental = GL_TRUE;  // For better compatibility.
        glewInit();

        glViewport(0,0,wWidth,wHeight);

        /* Register callbacks */
        glfwSetKeyCallback(w, key_callback);
        glfwSetInputMode(w,GLFW_CURSOR,GLFW_CURSOR_DISABLED);
        glfwSetCursorPosCallback(w,mouse_callback);
        
        /* Depth Testing */
        glEnable(GL_DEPTH_TEST);

        /* Blending */
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
        
        /* Set initial randomness */
        srand((GLuint)(100000 * glfwGetTime()));

        /* Font settings */
        /*
        debug("Initializing font.");
        FT_Library ft;
        check(!FT_Init_FreeType(&ft), "Failed loading FreeType lib.");

        FT_Face face;
        check(!FT_New_Face(ft,"arial.ttf",0,&face), "Failed to load font.");
        // Width calculated dynamically based on height.
        FT_Set_Pixel_Sizes(face,0,48);
        ftchar_t** chars = cogfAllAscii(face);
        check(chars, "Failed to generate all ASCII data.");
        FT_Done_Face(face);
        FT_Done_FreeType(ft);
        */
        
        /* Tetris piece Shaders */
        debug("Compiling Game shaders.");
        shaders_t* shaders = cogsShaders("vertex.glsl", "fragment.glsl");
        GLuint tetrisShader = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(tetrisShader > 0, "Game shaders didn't compile.");

        /* Arm Shaders */
        debug("Compiling Arm shaders.");
        shaders = cogsShaders("aVertex.glsl","aFragment.glsl");
        GLuint armShader = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(armShader > 0, "Arm shaders didn't compile.");

        /* Text Shaders */
        debug("Compiling Text shaders.");
        shaders = cogsShaders("cVertex.glsl","cFragment.glsl");
        GLuint textShader = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(textShader > 0, "Text shaders didn't compile.");

        // Lamp Shaders
        /*
        shaders = cogsShaders("lVertex.glsl", "lFragment.glsl");
        GLuint lShaderP = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(lShaderP > 0, "Lamp shaders didn't compile.");
        */
        
        /* Model Matrix for Game */
        matrix_t* tModel = coglMIdentity(4);
        tModel = coglM4Translate(tModel,-200,-360,0);
        tModel = coglMScale(tModel,2.0/450);
        check(tModel, "tModel creation failed.");

        /* Initialize Board, Grid, Arm, and first Block */
        initBoard();
        initGrid(tModel);  // This must come after `tModel` creation.
        initArm();
        initText();
        //initLamp();
        quiet_check(initBlock());

        /* Set initial Camera state */
        resetCamera();

        /* Model Matrix for Block */
        matrix_t* bModel = coglMIdentity(4);
        bModel = coglMScale(bModel,2.0/450);
        bModel = coglM4Translate(bModel,-1.1,-1.25,0);
        matrix_t* bModelFinal = NULL;
        check(bModel, "bModel creation failed.");

        /* Model Matrix for Robot Arm Base */
        matrix_t* aModel = coglMIdentity(4);
        aModel = coglMScale(aModel,0.5);
        aModel = coglM4Translate(aModel,-1.1,-1.25,0);
        check(aModel, "aModel creation failed.");

        /* Model Matrix for Robot Arm Shaft */
        // `lModel` is also rotated by other code.
        lModel = coglMIdentity(4);
        lModel = coglM4Translate(lModel,-1.1,-1.25,0);
        uModel = coglMCopy(lModel);
        matrix_t* shift = NULL;
        matrix_t* lModelFinal = NULL;
        matrix_t* uModelFinal = NULL;
        check(uModel, "uModel creation failed.");

        /* Projection Matrix */
        matrix_t* proj = coglMPerspectiveP(tau/8, 
                                           (GLfloat)wWidth/(GLfloat)wHeight,
                                           0.1f,1000.0f);

        /* Ortho Matrix */
        matrix_t* ortho = coglMOrthoP(0.0f,(GLfloat)wWidth,
                                      0.0f,(GLfloat)wHeight,
                                      0.1f,1000.0f);

        GLfloat currentFrame;

        /* Lighting settings */
        glUseProgram(armShader);
        matrix_t* lightPos = coglV3(1.2f,1.0f,2.0f);
        GLuint lightPosLoc = glGetUniformLocation(armShader,"lightPos");
        GLuint cubeL  = glGetUniformLocation(armShader,"cubeColour");
        GLuint lightL = glGetUniformLocation(armShader,"lightColour");

        glUniform3f(lightPosLoc,lightPos->m[0],lightPos->m[1],lightPos->m[2]);
        //glUniform3f(cubeL,1.0f,0.5f,0.31f);
        glUniform3f(cubeL,0.4,0.78,0.89);
        glUniform3f(lightL,1.0f,1.0f,1.0f);

        // Lamp Model
        /*
        matrix_t* laModel = coglMIdentity(4);
        laModel = coglMScale(laModel,0.2f);
        laModel = coglM4Translate(laModel,
                                  lightPos->m[0],
                                  lightPos->m[1],
                                  lightPos->m[2]);
        */
        
        debug("Entering Loop.");
        // Render until you shouldn't.
        while(!glfwWindowShouldClose(w)) {
                if(gameOver) {
                        sleep(1);
                        break;
                }

                currentFrame = glfwGetTime();
                deltaTime = currentFrame - lastFrame;
                lastFrame = currentFrame;

                glfwPollEvents();

                glClearColor(0.1f,0.1f,0.1f,1.0f);
                glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
                              
                // Update View Matrix
                coglMDestroy(view);
                view = coglM4LookAtP(camera->pos,camera->tar,camera->up);

                /* Draw Arm Base */
                glUseProgram(armShader);
                               
                GLuint modlLoc = glGetUniformLocation(armShader,"model");
                GLuint viewLoc = glGetUniformLocation(armShader,"view");
                GLuint projLoc = glGetUniformLocation(armShader,"proj");

                glUniformMatrix4fv(modlLoc,1,GL_FALSE,aModel->m);
                glUniformMatrix4fv(viewLoc,1,GL_FALSE,view->m);
                glUniformMatrix4fv(projLoc,1,GL_FALSE,proj->m);

                glBindVertexArray(aVAO[0]);
                glDrawArrays(GL_TRIANGLES,0,36);
                glBindVertexArray(0);

                /* Draw Arm Lower Shaft */
                if(shift)       { coglMDestroy(shift);       }
                if(lModelFinal) { coglMDestroy(lModelFinal); }
                GLfloat adj = SHAFT_LEN * cos(tau/4 + lShaftAngle);
                GLfloat opp = SHAFT_LEN * sin(tau/4 + lShaftAngle);
                shift = coglMIdentity(4);
                shift = coglM4Translate(shift,adj,opp,0);
                lModelFinal = coglMMultiplyP(shift,lModel);

                glUniformMatrix4fv(modlLoc,1,GL_FALSE,lModelFinal->m);
                glBindVertexArray(aVAO[1]);
                glDrawArrays(GL_TRIANGLES,0,36);
                glBindVertexArray(0);

                /* Draw Arm Upper Shaft */
                if(uModelFinal) { coglMDestroy(uModelFinal); }
                shift->m[12] *= 2;
                shift->m[13] *= 2;
                adj = SHAFT_LEN * cos(uShaftAngle);
                opp = SHAFT_LEN * sin(uShaftAngle);
                shift->m[12] += adj;
                shift->m[13] += opp;
                uModelFinal = coglMMultiplyP(shift,uModel);

                glUniformMatrix4fv(modlLoc,1,GL_FALSE,uModelFinal->m);
                glBindVertexArray(aVAO[2]);
                glDrawArrays(GL_TRIANGLES,0,36);
                glBindVertexArray(0);

                /* Draw Grid */
                glUniformMatrix4fv(modlLoc,1,GL_FALSE,tModel->m);
                glBindVertexArray(gVAO);
                glDrawArrays(GL_LINES, 0, 128);
                glBindVertexArray(0);
                
                // Set transformation Matrices
                glUseProgram(tetrisShader);

                lightPosLoc = glGetUniformLocation(tetrisShader,"lightPos");
                modlLoc     = glGetUniformLocation(tetrisShader,"model");
                viewLoc     = glGetUniformLocation(tetrisShader,"view");
                projLoc     = glGetUniformLocation(tetrisShader,"proj");

                glUniform3f(lightPosLoc,lightPos->m[0],
                            lightPos->m[1],lightPos->m[2]);
                glUniformMatrix4fv(modlLoc,1,GL_FALSE,tModel->m);
                glUniformMatrix4fv(viewLoc,1,GL_FALSE,view->m);
                glUniformMatrix4fv(projLoc,1,GL_FALSE,proj->m);

                /* Draw Board */
                glBindVertexArray(fVAO);
                glDrawArrays(GL_TRIANGLES,0,7200);
                glBindVertexArray(0);

                /* Draw Block */
                if(bModelFinal) { coglMDestroy(bModelFinal); }
                shift->m[12] += adj;
                shift->m[13] += opp;
                bModelFinal = coglMMultiplyP(shift,bModel);
                glUniformMatrix4fv(modlLoc,1,GL_FALSE,bModelFinal->m);

                // Tell the Block where its sitting in World Space.
                block->x = bModelFinal->m[12];
                block->y = bModelFinal->m[13];

                glBindVertexArray(bVAO);
                glDrawArrays(GL_TRIANGLES,0,36 * 4);
                glBindVertexArray(0);

                // Draw Lamp
                /*
                glUseProgram(lShaderP);

                modlLoc = glGetUniformLocation(lShaderP,"model");
                viewLoc = glGetUniformLocation(lShaderP,"view");
                projLoc = glGetUniformLocation(lShaderP,"proj");

                glUniformMatrix4fv(modlLoc,1,GL_FALSE,laModel->m);
                glUniformMatrix4fv(viewLoc,1,GL_FALSE,view->m);
                glUniformMatrix4fv(projLoc,1,GL_FALSE,proj->m);

                glBindVertexArray(lVAO);
                glDrawArrays(GL_TRIANGLES,0,36);
                glBindVertexArray(0);
                */

                /* Draw Text */
                /*
                glUseProgram(textShader);
                projLoc = glGetUniformLocation(textShader,"proj");
                glUniformMatrix4fv(projLoc,1,GL_FALSE,ortho->m);
                renderText(chars,textShader,"Hello!",6,25.0f,25.0f,1.0f);
                renderText(chars,textShader,"This is great!",
                           14,540.0f,570.0f,0.5f);
                */
                        
                // Always comes last.
                glfwSwapBuffers(w);
        }
        
        // Clean up.
        glfwTerminate();
        log_info("Thanks for playing!");

        return EXIT_SUCCESS;
 error:
        return EXIT_FAILURE;
}
