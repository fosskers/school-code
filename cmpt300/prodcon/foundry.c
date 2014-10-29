#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>
#include <stdbool.h>
#include <string.h>

#include "lib/bstrlib.h"
#include "dbg.h"

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

void menu(WINDOW* win, int highlight) {
        int i;
        int x = 2;
        int y = 2;
        char* choices[] = {
                "Cats", "Dogs", "Birds", "Snakes"
        };

        box(win, 0, 0);

        for(i = 0; i < CHOICES; i++) {
                if(highlight == i + 1) {
                        wattron(win, A_REVERSE);
                        mvwprintw(win, y, x, "%s", choices[i]);
                        wattroff(win, A_REVERSE);
                } else {
                        mvwprintw(win, y, x, "%s", choices[i]);
                }

                y++;
        }

        wrefresh(win);
}

int main(int argc, char** argv) {
        WINDOW* win;
        int ch;
        char* msg = "Welcome to the Foundry!";
        int startx, starty;
        int highlight = 1;
        int tools;
        int operators;

        /* Initialize the screen */
        initscr();
        raw();
        keypad(stdscr, true);
        start_color();
        init_pair(1, COLOR_CYAN, COLOR_BLACK);

        /* Intro screen */
        box(stdscr, 0, 0);
        mvprintw(LINES / 2, (COLS - strlen(msg)) / 2, "%s", msg);
        attron(A_BOLD | COLOR_PAIR(1));
        mvprintw(LINES - 2, 1, "Press q to quit.");
        refresh();
        attroff(A_BOLD | COLOR_PAIR(1));

        // Wait for the user.
        getch();
        clear();

        /* Get initial simulation settings */
        printw("Tools in The Foundry: ");
        refresh();
        tools = getch();
        check(tools > 47 && tools < 58, "tools: Number not given.");

        printw("\nOperators in The Foundry: ");
        refresh();
        operators = getch();
        check(operators > 47 && operators < 58, "operators: Number not given.");
        clear();

        /* Interaction is over */
        noecho();
        curs_set(0);

        /* Main page */
        printw("Tools: %d --- Operators: %d", tools, operators);
        refresh();

        /*
        startx = (COLS - WIDTH) / 2;
        starty = (LINES - HEIGHT) / 2;
        win = newwin(10, 30, starty, startx);
        keypad(win, true);
        mvprintw(0,0, "Use arrows to select.");
        refresh();

        menu(win, highlight);

        while((ch = getch()) != 'q') {
                switch(ch) {
                case KEY_UP:
                        if(highlight == 1) {
                                highlight = CHOICES;
                        } else {
                                highlight--;
                        }
                        break;
                case KEY_DOWN:
                        if(highlight == CHOICES) {
                                highlight = 1;
                        } else {
                                highlight++;
                        }
                        break;
                default:
                        break;
                }
                refresh();
                menu(win, highlight);
        }
        */

        while((ch = getch()) != 'q') {}
        endwin();

        return EXIT_SUCCESS;

 error:
        endwin();
        return EXIT_FAILURE;
}
