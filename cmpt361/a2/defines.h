#ifndef __defines_h__
#define __defines_h__

#define BOARD_CELLS 200
// 6 floats per vertex, 3 vertices per triangle, 12 triangles per Cell
#define CELL_FLOATS 6 * 3 * 12
#define TOTAL_FLOATS BOARD_CELLS * CELL_FLOATS
#define SHAFT_LEN 0.75  // Actually half of the total shaft length.
#define wWidth 650
#define wHeight 720

#endif
