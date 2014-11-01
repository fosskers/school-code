#include <ncurses.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "lib/bstrlib.h"
#include "dbg.h"
#include "utils.h"

/*
 * Useful things:
 * clear();
 * getyx(y,x);
 * move(y,x)
 * getmaxyx(stdscr,row,col);
 * Window dumping: putwin(), getwin()
 */

void f_box(WINDOW*, int, int);

// --- //

int run_simulation(int tools, int operators) {
        bool paused = false;
        int ch = 0;
        int time = 0;
        int x, y, i;

        /* Main page */
        nodelay(stdscr, true);
        f_box(stdscr, 0, 0);
        mvprintw(1, 1, "Tools: %d", tools);
        mvprintw(2, 1, "Operators: %d", operators);
        refresh();

        /* Display initial graphics */
        getmaxyx(stdscr, y, x);
        for(i = 0; i < tools; i++) {
                mvprintw(y/2 - 4, x/2 - tools + (i * 2), "T");
        }
        attron(A_REVERSE);
        mvprintw(y/2 - 2, x/2 - tools, "G");
        mvprintw(y/2, x/2 - tools, "G");
        mvprintw(y/2 + 2, x/2 - tools, "G");
        attroff(A_REVERSE);

        /* Main event loop */
        while(ch != 'q') {
                mvprintw(3, 1, "Time: %d", time);

                switch(ch) {
                case 'p':
                        if(paused) {
                                paused = false;
                                getmaxyx(stdscr, y, x);
                                mvprintw(1, x - 9, "        ");
                        } else {
                                paused = true;
                                attron(COLOR_PAIR(2));
                                getmaxyx(stdscr, y, x);
                                mvprintw(1, x - 9, "[PAUSED]");
                                attroff(COLOR_PAIR(2));
                        }

                        break;
                        
                default:
                        break;
                }

                if(!paused) {
                        time++;
                        sleep(1);
                }

                refresh();
                ch = getch();
        }

        return EXIT_SUCCESS;
}

void f_box(WINDOW* win, int n, int m) {
        box(win, n, m);
        attron(A_BOLD | COLOR_PAIR(1));
        mvprintw(LINES - 2, 1, "Press q to quit, p to pause.");
        refresh();
        attroff(A_BOLD | COLOR_PAIR(1));
}

int main(int argc, char** argv) {
        char* msg = "Welcome to the Foundry!";
        int tools, operators, result;

        /* Initialize the screen */
        initscr();
        raw();
        keypad(stdscr, true);
        start_color();
        init_pair(1, COLOR_CYAN, COLOR_BLACK);
        init_pair(2, COLOR_YELLOW, COLOR_BLACK);
        init_pair(3, COLOR_RED, COLOR_BLACK);

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
        check(tools > 0, "tools: Invalid number given.");

        mvprintw(2, 1, "Operators in The Foundry: ");
        refresh();
        operators = ctoi(ignore_til_num());
        check(operators > 0, "operators: Invalid number given.");
        clear();

        /* Interaction is over */
        noecho();
        curs_set(0);
        result = run_simulation(tools, operators);
        check(result == EXIT_SUCCESS, "Simulation failed.");

        endwin();
        puts("Goodbye!");

        return EXIT_SUCCESS;

 error:
        endwin();
        puts("You were kicked out of The Foundry.");
        return EXIT_FAILURE;
}
