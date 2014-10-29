#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>
#include <stdbool.h>
#include <string.h>

#include "lib/bstrlib.h"

int main(int argc, char** argv) {
        int row,col;
        char* msg = "Welcome to the Foundry!";

        /* Initialize the screen */
        initscr();
        raw();
        keypad(stdscr, true);
        noecho();

        getmaxyx(stdscr,row,col);
        mvprintw(row / 2, (col - strlen(msg)) / 2, "%s", msg);
        refresh();
        
        getch();
        endwin();

        return EXIT_SUCCESS;
}
