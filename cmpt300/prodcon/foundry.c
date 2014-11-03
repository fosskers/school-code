/* The Foundry
 * An nCurses Producer-Consumer Problem Simulator
 * Metal choices made based on: http://en.wikipedia.org/wiki/Foundry
 *
 * author:   Colin Woodbury
 * created:  2014 October 28
 * modified: 2014 November  2 @ 14:25
 */

/* TODO:
 * - Speed up/down on arrow keys (use nanosleep)
 */

#include <ncurses.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/queue.h>
#include <unistd.h>

#include "dbg.h"
#include "lib/bstrlib.h"
#include "utils.h"

/*
 * Useful things:
 * clear();
 * getyx(y,x);
 * move(y,x)
 * getmaxyx(stdscr,row,col);
 * Window dumping: putwin(), getwin()
 */

// --- //

/* GENERAL DEFINES */
#define MAX_METALS 10
#define GENERATORS 3

typedef enum { NoMetal, Iron, Bronze, Zinc } Metal;

typedef enum { NoAlloy, IronBronze, IronZinc, BronzeZinc } Alloy;

typedef struct {
        int op_num;
        int tools_taken;
        Metal metal1;
        Metal metal2;
        Alloy produced;
} Operator;

/* SHARED RESOURCES */
typedef struct metals {
        Metal metal;
        TAILQ_ENTRY(metals) nexts;
} metals;

typedef struct alloys {
        Alloy alloy;
        TAILQ_ENTRY(alloys) nexts;
} alloys;

TAILQ_HEAD(metalhead, metals) metal_head;
TAILQ_HEAD(alloyhead, alloys) alloy_head;

// Lengths of the Metal and Alloys queues.
int metals_len = 0;
int alloys_len = 0;

pthread_mutex_t metal_mutex;
pthread_mutex_t alloy_mutex;
pthread_mutex_t tool_mutex;

int tool_counter = 0;

// --- //

/* Forward Declarations */
void f_box(WINDOW*, int, int);
Operator* operator_create(int op_num);

// --- //

void* generate_metal(void* metal) {
        // Be a thread.
}

Operator* operator_create(int op_num) {
        Operator* o = malloc(sizeof(Operator));
        check_mem(o);

        o->op_num      = op_num;
        o->tools_taken = 0;
        o->metal1      = NoMetal;
        o->metal2      = NoMetal;
        o->produced    = NoAlloy;

        return o;

 error:
        return NULL;
}

int run_simulation(int tools, int operators) {
        bool paused = false;
        int ch      = 0;
        int time    = 0;
        int x, y, i;
        pthread_t generator_ts[GENERATORS];
        pthread_t* operators_ts;

        // Make generators, give each an element
        // Associate with threads and join
        
        // Make operator list
        // Associate with threads and join

        /* Initialize Queues */
        TAILQ_INIT(&metal_head);
        TAILQ_INIT(&alloy_head);

        /* Initialize Mutexes */
        pthread_mutex_init(&metal_mutex, NULL);
        pthread_mutex_init(&alloy_mutex, NULL);
        pthread_mutex_init(&tool_mutex, NULL);

        /* Main page */
        nodelay(stdscr, true);
        f_box(stdscr, 0, 0);
        mvprintw(1, 1, "Tools: %d", tools);
        mvprintw(2, 1, "Operators: %d", operators);
        refresh();

        /* Display initial graphics */
        getmaxyx(stdscr, y, x);
        
        // Display Tools
        for(i = 0; i < tools; i++) {
                mvprintw(y/2 - 4, x/2 - tools + (i * 2), "T");
        }

        // Display Generators
        attron(A_REVERSE);
        mvprintw(y/2 - 2, x/2 - tools - 5, "G");
        mvprintw(y/2, x/2 - tools - 5, "G");
        mvprintw(y/2 + 2, x/2 - tools - 5, "G");
        attroff(A_REVERSE);

        // Display Operators
        for(i = 0; i < operators; i++) {
                mvprintw(y/2 - operators + (i * 2) + 1, x/2 + tools + 4, "O");
        }
        
        // Input Queue
        for(i = 0; i < tools; i++) {
                mvprintw(y/2, x/2 - tools - 3 + i, "=");
                mvprintw(y/2, x/2 + 3 + i, "=");
        }
        mvprintw(y/2, x/2 - 3, "[ %02d ]", metals_len);

        // Output Queue
        mvprintw(y/2, x/2 + tools + 6, ">=>[ %03d ]", alloys_len);

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
        const char* msg = "Welcome to the Foundry!";
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
        check(tools > 1, "tools: Invalid number given.");

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
        return EXIT_FAILURE;
}
