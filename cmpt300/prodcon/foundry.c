#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>
#include <stdbool.h>
#include <string.h>

#include "lib/bstrlib.h"
#include "dbg.h"
#include "utils.h"

#define WIDTH  30
#define HEIGHT 10
#define CHOICES 4

/*
 * Useful things:
 * clear();
 * getyx(y,x);
 * move(y,x)
 * getmaxyx(stdscr,row,col);
 * Window dumping: putwin(), getwin()
 */

/* How to make the cursor disappear? */

/* void menu(WINDOW* win, int highlight) { */
/*         int i; */
/*         int x = 2; */
/*         int y = 2; */
/*         char* choices[] = { */
/*                 "Cats", "Dogs", "Birds", "Snakes" */
/*         }; */

/*         box(win, 0, 0); */

/*         for(i = 0; i < CHOICES; i++) { */
/*                 if(highlight == i + 1) { */
/*                         wattron(win, A_REVERSE); */
/*                         mvwprintw(win, y, x, "%s", choices[i]); */
/*                         wattroff(win, A_REVERSE); */
/*                 } else { */
/*                         mvwprintw(win, y, x, "%s", choices[i]); */
/*                 } */

/*                 y++; */
/*         } */

/*         wrefresh(win); */
/* } */

void f_box(WINDOW* win, int n, int m) {
        init_pair(1, COLOR_CYAN, COLOR_BLACK);
        box(win, n, m);
        attron(A_BOLD | COLOR_PAIR(1));
        mvprintw(LINES - 2, 1, "Press q to quit.");
        refresh();
        attroff(A_BOLD | COLOR_PAIR(1));
}

int main(int argc, char** argv) {
        int ch;
        char* msg = "Welcome to the Foundry!";
        int tools, operators;

        /* Initialize the screen */
        initscr();
        raw();
        keypad(stdscr, true);
        start_color();

        /* Intro screen */
        f_box(stdscr, 0, 0);
        curs_set(0);
        mvprintw(LINES / 2, (COLS - strlen(msg)) / 2, "%s", msg);

        // Wait for the user.
        getch();
        clear();

        /* Get initial simulation settings */
        curs_set(1);
        f_box(stdscr, 0, 0);
        mvprintw(1, 1, "Tools in The Foundry: ");
        refresh();
        tools = ctoi(ignore_til_num());
        check(tools != -1, "tools: Number not given.");

        mvprintw(2, 1, "Operators in The Foundry: ");
        refresh();
        operators = ctoi(ignore_til_num());
        check(operators != -1, "operators: Number not given.");
        clear();

        /* Interaction is over */
        noecho();
        curs_set(0);

        /* Main page */
        f_box(stdscr, 0, 0);
        mvprintw(1, 1, "Tools: %d --- Operators: %d", tools, operators);
        refresh();

        /* Main event loop */
        while((ch = getch()) != 'q') {}

        endwin();
        puts("Goodbye!");

        return EXIT_SUCCESS;

 error:
        endwin();
        puts("You were kicked out of The Foundry.");
        return EXIT_FAILURE;
}
