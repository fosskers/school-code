#include <GL/glew.h>  // This must be before other GL libs.
#include <GLFW/glfw3.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "collision.h"
#include "block.h"
#include "cog/camera/camera.h"
#include "cog/dbg.h"
#include "cog/shaders/shaders.h"
#include "util.h"

// --- //

GLfloat* blockToCoords();
void initBoard();
void clearBoard();
void newBlock();
void refreshBlock();
int refreshBoard();

// --- //

#define BOARD_CELLS 200

Fruit board[BOARD_CELLS];      // The Board, represented as Fruits.
bool keys[1024];
GLuint wWidth  = 400;
GLuint wHeight = 720;

// Buffer Objects
GLuint gVAO;
GLuint gVBO;
GLuint bVAO;
GLuint bVBO;
GLuint fVAO;
GLuint fVBO;

camera_t* camera;
matrix_t* view;
block_t*  block;   // The Tetris block.

// --- //

/* Move Camera with WASD */
void moveCamera() {
        cogcMove(camera,
                 keys[GLFW_KEY_W],
                 keys[GLFW_KEY_S],
                 keys[GLFW_KEY_A],
                 keys[GLFW_KEY_D]);
}

/* Init/Reset the Camera */
void resetCamera() {
        GLfloat camFS[] = {0,0,4};
        matrix_t* camPos = coglVFromArray(3,camFS);
        camFS[0] = 0; camFS[1] = 0; camFS[2] = -1;
        matrix_t* camDir = coglVFromArray(3,camFS);
        camFS[0] = 0; camFS[1] = 1; camFS[2] = 0;
        matrix_t* camUp = coglVFromArray(3,camFS);

        camera = cogcCreate(camPos,camDir,camUp);
}

/* Clears the board and starts over */
void resetGame() {
        initBoard();
        newBlock();
}

void newBlock() {
        block = randBlock();
        refreshBlock();
}

void refreshBlock() {
        GLfloat* coords = blockToCoords();
        
        glBindVertexArray(bVAO);
        glBindBuffer(GL_ARRAY_BUFFER, bVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, 
                        120 * sizeof(GLfloat), coords);
        glBindVertexArray(0);

        free(coords);  // Necessary?
}

void key_callback(GLFWwindow* w, int key, int code, int action, int mode) {
        if(action == GLFW_PRESS) {
                keys[key] = true;

                if(key == GLFW_KEY_Q) {
                        glfwSetWindowShouldClose(w, GL_TRUE);
                } else if(key == GLFW_KEY_C) {
                        resetCamera();
                } else if(key == GLFW_KEY_R) {
                        resetGame();
                } else if(key == GLFW_KEY_LEFT && block->x > 0) {
                        block->x -= 1;
                        refreshBlock();
                } else if(key == GLFW_KEY_RIGHT && block->x < 9) {
                        block->x += 1;
                        refreshBlock();
                } else if(key == GLFW_KEY_DOWN && block->y > 0) {
                        block->y -= 1;
                        refreshBlock();
                } else if(key == GLFW_KEY_UP && block->y < 19) {
                        block = rotateBlock(block);
                        refreshBlock();
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
        GLfloat temp[30] = { 33 + x*33, 33 + y*33, c[0], c[1], c[2],
                             33 + x*33, 66 + y*33, c[0], c[1], c[2],
                             66 + x*33, 33 + y*33, c[0], c[1], c[2],
                             33 + x*33, 66 + y*33, c[0], c[1], c[2], 
                             66 + x*33, 33 + y*33, c[0], c[1], c[2],
                             66 + x*33, 66 + y*33, c[0], c[1], c[2] };

        check(x > -1 && x < 10 &&
              y > -1 && x < 20,
              "Invalid coords given.");

        coords = malloc(sizeof(GLfloat) * 30);
        check_mem(coords);

        if(f == None) {
                // Nullify all the coordinates
                for(i = 0; i < 30; i++) {
                        temp[i] = 0.0;
                }
        }

        // Copy values.
        for(i = 0; i < 30; i++) {
                coords[i] = temp[i];
        }
        
        return coords;
 error:
        return NULL;
}

/* Produce locations and colours based on the current global Block */
GLfloat* blockToCoords() {
        GLfloat* temp1;
        GLfloat* temp2;

        // 4 cells, each has 6 vertices of 5 data points each.
        GLfloat* cs = malloc(sizeof(GLfloat) * 4 * 6 * 5);
        check_mem(cs);

        // Coords and colours for each cell.
        GLfloat* a = gridLocToCoords(block->x+block->coords[0],
                                     block->y+block->coords[1],
                                     block->fs[0]);
        GLfloat* b = gridLocToCoords(block->x+block->coords[2],
                                     block->y+block->coords[3],
                                     block->fs[1]);
        GLfloat* c = gridLocToCoords(block->x,
                                     block->y,
                                     block->fs[2]);
        GLfloat* d = gridLocToCoords(block->x+block->coords[4],
                                     block->y+block->coords[5],
                                     block->fs[3]);

        check(a && b && c && d, "Couldn't get Cell coordinates.");

         // Construct return value
        temp1 = append(a, 30, b, 30);
        temp2 = append(c, 30, d, 30);
        cs    = append(temp1, 60, temp2, 60);
        check(cs, "Couldn't construct final list of coords/colours.");
        //cs = a;

        free(temp1); free(temp2);
        free(a); 
        free(b); free(c); free(d);
        
        return cs;
 error:
        if(cs) { free(cs); }
        return NULL;
}

/* Initialize the Block */
int initBlock() {
        block = randBlock();
        check(block, "Failed to initialize first Block.");
        debug("Got a: %c", block->name);

        debug("Initializing Block.");
        
        // Each block has 120 data points.
        GLfloat* coords = blockToCoords();
        
        // Set up VAO/VBO
        glGenVertexArrays(1,&bVAO);
        glBindVertexArray(bVAO);
        glGenBuffers(1,&bVBO);
        glBindBuffer(GL_ARRAY_BUFFER,bVBO);
        glBufferData(GL_ARRAY_BUFFER,120 * sizeof(GLfloat),coords,
                     GL_DYNAMIC_DRAW);

        // Tell OpenGL how to process Block Vertices
        glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),
                              (GLvoid*)(2 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        debug("Block initialized.");

        return 1;
 error:
        return 0;
}

/* Initialize the Grid */
// Insert TRON pun here.
void initGrid() {
        GLfloat gridPoints[320];  // Contains colour info as well.
        int i;

        debug("Initializing Grid.");

        // Vertical lines
	for (i = 0; i < 11; i++){
                // Bottom coord
		gridPoints[10*i]     = 33.0 + (33.0 * i);
                gridPoints[10*i + 1] = 33.0;
                // Bottom colour
                gridPoints[10*i + 2] = 1;
                gridPoints[10*i + 3] = 1;
                gridPoints[10*i + 4] = 1;
                // Top coord
		gridPoints[10*i + 5] = 33.0 + (33.0 * i);
                gridPoints[10*i + 6] = 693.0;
                // Top colour
                gridPoints[10*i + 7] = 1;
                gridPoints[10*i + 8] = 1;
                gridPoints[10*i + 9] = 1;
	}

	// Horizontal lines
	for (i = 0; i < 21; i++){
                // Left coord
		gridPoints[110 + 10*i]     = 33.0;
                gridPoints[110 + 10*i + 1] = 33.0 + (33.0 * i);
                // Left colour
                gridPoints[110 + 10*i + 2] = 1;
                gridPoints[110 + 10*i + 3] = 1;
                gridPoints[110 + 10*i + 4] = 1;
                // Right coord
		gridPoints[110 + 10*i + 5] = 363.0;
                gridPoints[110 + 10*i + 6] = 33.0 + (33.0 * i);
                // Right colour
                gridPoints[110 + 10*i + 7] = 1;
                gridPoints[110 + 10*i + 8] = 1;
                gridPoints[110 + 10*i + 9] = 1;
	}

        // Set up VAO/VBO
        glGenVertexArrays(1,&gVAO);
        glBindVertexArray(gVAO);
        glGenBuffers(1,&gVBO);
        glBindBuffer(GL_ARRAY_BUFFER, gVBO);
        glBufferData(GL_ARRAY_BUFFER,sizeof(gridPoints),gridPoints,GL_STATIC_DRAW);

        // Tell OpenGL how to process Grid Vertices
        glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),
                              (GLvoid*)(2 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        glBindVertexArray(0);  // Reset the VAO binding.
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        debug("Grid initialized.");
}

/* Initialize the game board */
void initBoard() {
        debug("Initializing Board.");

        GLfloat temp[] = { 0,0,0,0,0 };
        
        // Set up VAO/VBO
        glGenVertexArrays(1,&fVAO);
        glBindVertexArray(fVAO);
        glGenBuffers(1,&fVBO);
        glBindBuffer(GL_ARRAY_BUFFER,fVBO);
        glBufferData(GL_ARRAY_BUFFER,6000 * sizeof(GLfloat),temp,
                     GL_DYNAMIC_DRAW);

        // Tell OpenGL how to process Block Vertices
        glVertexAttribPointer(0,2,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),(GLvoid*)0);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,
                              5 * sizeof(GLfloat),
                              (GLvoid*)(2 * sizeof(GLfloat)));
        glEnableVertexAttribArray(1);
        glBindVertexArray(0);  // Reset the VAO binding.
        //        glBindBuffer(GL_ARRAY_BUFFER, 0);

        clearBoard();

        debug("Board initialized.");
}

/* Move all coloured Cells from the Board */
void clearBoard() {
        int i;

        for(i = 0; i < BOARD_CELLS; i++) {
                board[i] = 0;
        }

        board[0] = Apple;
        board[1] = Banana;
        board[2] = Grape;

        refreshBoard();
}

/* Draw all the coloured Board Cells */
int refreshBoard() {
        GLuint i,j,k;
        GLfloat* cellData;

        debug("Refreshing Board...");
        
        GLfloat* coords = malloc(sizeof(GLfloat) * 6000);
        check_mem(coords);

        for(i = 0; i < BOARD_CELLS; i++) {
                cellData = gridLocToCoords(i % 10, i / 10, board[i]);
                check(cellData, "Couldn't get coord data for Cell.");
                
                for(j = i*30, k=0; k < 30; j++, k++) {
                        coords[j] = cellData[k];
                }
        }

        glBindVertexArray(fVAO);
        glBindBuffer(GL_ARRAY_BUFFER, fVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, 
                        6000 * sizeof(GLfloat), coords);
        glBindVertexArray(0);

        // TODO: Free coords?
        free(coords);
        
        return 1;
 error:
        return 0;        
}

/* Scrolls the Block naturally down */
void scrollBlock() {
        static double lastTime = 0;
        double currTime = glfwGetTime();
        int* cells = NULL;
        int i,j;

        if(isColliding(block, board) != Bottom) {
                if(currTime - lastTime > 0.5) {

                        lastTime = currTime;
                        block->y -= 1;

                        GLfloat* coords = blockToCoords();
        
                        glBindVertexArray(bVAO);
                        glBindBuffer(GL_ARRAY_BUFFER, bVBO);
                        glBufferSubData(GL_ARRAY_BUFFER, 0, 
                                        120 * sizeof(GLfloat), coords);
                        glBindVertexArray(0);
                }
        } else {
                cells = blockCells(block);

                // Add the Block's cells to the master Board
                for(i = 0,j=0; i < 8; i+=2,j++) {
                        board[cells[i] + 10*cells[i+1]] = block->fs[j];
                }

                newBlock();
                refreshBoard();
        }
}

int main(int argc, char** argv) {
        // Initial settings.
        glfwInit();
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
        glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
        
        // Make a window.
        GLFWwindow* w = glfwCreateWindow(wWidth,wHeight,"Fetris",NULL,NULL);
        glfwMakeContextCurrent(w);

        // Fire up GLEW.
        glewExperimental = GL_TRUE;  // For better compatibility.
        glewInit();

        // For the rendering window.
        glViewport(0,0,wWidth,wHeight);

        // Register callbacks.
        glfwSetKeyCallback(w, key_callback);
        glfwSetInputMode(w,GLFW_CURSOR,GLFW_CURSOR_DISABLED);
        glfwSetCursorPosCallback(w,mouse_callback);
        
        // Depth Testing
        glEnable(GL_DEPTH_TEST);

        // Create Shader Program
        debug("Making shader program.");
        shaders_t* shaders = cogsShaders("vertex.glsl", "fragment.glsl");
        GLuint shaderProgram = cogsProgram(shaders);
        cogsDestroy(shaders);
        check(shaderProgram > 0, "Shaders didn't compile.");
        debug("Shaders good.");

        // Initialize Board, Grid, and first Block
        initBoard();
        initGrid();
        quiet_check(initBlock());

        // Set initial Camera state
        resetCamera();
        
        // Projection Matrix
        matrix_t* proj = coglMPerspectiveP(tau/8, 
                                           (float)wWidth/(float)wHeight,
                                           0.1f,1000.0f);

        debug("Entering Loop.");
        // Render until you shouldn't.
        while(!glfwWindowShouldClose(w)) {
                glfwPollEvents();
                moveCamera();
                
                glClearColor(0.5f,0.5f,0.5f,1.0f);
                glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

                glUseProgram(shaderProgram);

                // Move the block down.
                scrollBlock();
                
                GLuint viewLoc = glGetUniformLocation(shaderProgram,"view");
                GLuint projLoc = glGetUniformLocation(shaderProgram,"proj");

                // Update View Matrix
                coglMDestroy(view);
                view = coglM4LookAtP(camera->pos,camera->tar,camera->up);

                // Set transformation Matrices
                glUniformMatrix4fv(viewLoc,1,GL_FALSE,view->m);
                glUniformMatrix4fv(projLoc,1,GL_FALSE,proj->m);

                // Draw Grid
                glBindVertexArray(gVAO);
                glDrawArrays(GL_LINES, 0, 64);
                glBindVertexArray(0);
                
                // Draw Block
                glBindVertexArray(bVAO);
                glDrawArrays(GL_TRIANGLES,0,24);
                glBindVertexArray(0);

                // Draw Board
                glBindVertexArray(fVAO);
                glDrawArrays(GL_TRIANGLES,0,1200);
                glBindVertexArray(0);

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
