/* The Foundry
 * An nCurses Producer-Consumer Problem Simulator
 * Metal choices made based on: http://en.wikipedia.org/wiki/Foundry
 * Alloy choices made based on: http://en.wikipedia.org/wiki/List_of_alloys
 *
 * author:   Colin Woodbury
 * created:  2014 October 28
 * modified: 2014 November  4 @ 11:49
 */

#include <ncurses.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/queue.h>
#include <time.h>
#include <unistd.h>

#include "dbg.h"
#include "lib/bstrlib.h"
#include "metals.h"
#include "operator.h"
#include "utils.h"

// --- //

/* GENERAL DEFINES */
#define MAX_METALS 10
#define TOOLS_NEEDED 2

/* SHARED RESOURCES */
// --- DOMAIN OF METAL_MUTEX --- //
pthread_mutex_t metal_mutex;
pthread_cond_t metals_full;   // The metals queue is full.
pthread_cond_t metals_empty;  // The metals queue is empty.

char generated_metals[METAL_TYPES + 1];

typedef struct metals {
        Metal metal;
        TAILQ_ENTRY(metals) nexts;
} metals;

TAILQ_HEAD(metalhead, metals) metal_head;

int metals_len = 0;  // Length of the Metal queue.

// --- DOMAIN OF ALLOY_MUTEX --- //
pthread_mutex_t alloy_mutex;

typedef struct alloys {
        Alloy alloy;
        TAILQ_ENTRY(alloys) nexts;
} alloys;

TAILQ_HEAD(alloyhead, alloys) alloy_head;

int alloys_len = 0;  // Length of the Alloy queue.

// --- DOMAIN OF TOOLS_MUTEX --- //
pthread_mutex_t tools_mutex;
pthread_cond_t tools_gone;  // Who took all the damn tools?
int tool_counter = 0;
int tools_taken  = 0;

// --- OTHER - DON'T TOUCH --- //
bool paused = false;
struct timespec t;

// --- //

/* Forward Declarations */
void f_box(WINDOW*, int, int);

// --- //

void* generate_metal(void* metalwrap) {
        Metal metal = ((MetalWrap*)metalwrap)->metal;
        metals* m;

        while(true) {
                while(paused) {
                        nanosleep(&t, NULL);
                }

                pthread_mutex_lock(&metal_mutex);

                // The Metals queue is full!
                while(metals_len >= MAX_METALS) {
                        pthread_cond_wait(&metals_full, &metal_mutex);
                }

                m = malloc(sizeof(metals));
                check_mem(m);

                // New metal in the queue.
                m->metal = metal;
                TAILQ_INSERT_TAIL(&metal_head, m, nexts);
                metals_len++;
                generated_metals[metal] += 1;
                pthread_cond_signal(&metals_empty);

                pthread_mutex_unlock(&metal_mutex);

                // Take a rest.
                nanosleep(&t, NULL);
        }

 error:
        free(metalwrap);
        pthread_exit(0);
}

void print_metals(int max_tools) {
        int y, x, i;
        char* metal_name;

        getmaxyx(stdscr,y,x);

        for(i = 1; i < METAL_TYPES + 1; i++) {
                metal_name = metal_to_string(i);
                mvprintw(i, 20, "%s: %d", metal_name, generated_metals[i]);

                if(metals_len >= MAX_METALS) {
                        mvprintw(y/2 - 2 + ((i - 1) * 2), x/2 - max_tools - 5, "G");
                } else {
                        attron(A_REVERSE);
                        mvprintw(y/2 - 2 + ((i - 1) * 2), x/2 - max_tools - 5, "G");
                        attroff(A_REVERSE);                
                }
        }
}

void print_tools(int max_tools) {
        int y, x, i;

        getmaxyx(stdscr, y, x);

        for(i = 0; i < max_tools; i++) {
                if(i < max_tools - tool_counter) {
                        attron(A_REVERSE);
                        mvprintw(y/2 - 4, x/2 - max_tools + (i * 2), "T");
                        attroff(A_REVERSE);
                } else {
                        mvprintw(y/2 - 4, x/2 - max_tools + (i * 2), "T");
                }
        }

        mvprintw(1, 40, "Tools Taken: %d", tools_taken);
}

/* Return any tools the Operator has */
void return_tools(Operator* o) {
        pthread_mutex_lock(&tools_mutex);

        // Kritikal Sektion
        tool_counter += o->tools_taken;
        o->tools_taken = 0;
        pthread_cond_signal(&tools_gone);

        pthread_mutex_unlock(&tools_mutex);
}

bool get_tool(Operator* o) {
        pthread_mutex_lock(&tools_mutex);

        // Give up our tool.
        if(tool_counter == 0 && o->tools_taken > 0) {
                //                o->tools_taken -= 1;
                //                tool_counter++;
                pthread_cond_signal(&tools_gone);
                pthread_mutex_unlock(&tools_mutex);
                return false;
        }

        // There aren't any tools left!
        while(tool_counter == 0) {
                pthread_cond_wait(&tools_gone, &tools_mutex);
        }

        // Take a tool.
        o->tools_taken += 1;
        tools_taken++;  // For testing.
        tool_counter--;
        pthread_mutex_unlock(&tools_mutex);
        return true;
}

Metal get_metal() {
        Metal m;  // Temporary!

        pthread_mutex_lock(&metal_mutex);

        // The metals queue is empty!
        while(metals_len == 0) {
                pthread_cond_wait(&metals_empty, &metal_mutex);
        }

        // Remove head from metal queue
        m = metal_head.tqh_first->metal;
        TAILQ_REMOVE(&metal_head, metal_head.tqh_first, nexts);
        metals_len--;

        // Tell other threads there's space in the metal queue.
        pthread_cond_signal(&metals_full);

        pthread_mutex_unlock(&metal_mutex);

        return m;
}

void* operate(void* operator) {
        Operator* o = (Operator*)operator;
        metals* m = NULL;
        alloys* a = NULL;
        //        int y, x;

        //        getmaxyx(stdscr,y,x);

        // Get two materials (one at a time).
        // Get two tools (one at a time).
        // -- On the second try, if there are none, give up your first one.
        // Lock the queue.
        // Check if contents of output queue are ok.
        // If not, give up and put everything back.
        // If so, make the alloy, put tools back, unlock queue.
        // Repeat.
        while(true) {
                while(paused) {
                        nanosleep(&t, NULL);
                }

                o->metal1 = get_metal();
                o->metal2 = get_metal();

                get_tool(o);
                if(get_tool(o)) {
                        pthread_mutex_lock(&alloy_mutex);

                        // Wait for varied time.
                        sleep(1);

                        a = malloc(sizeof(alloys));
                        check_mem(a);
                        a->alloy = make_alloy(o->metal1, o->metal2);
                        TAILQ_INSERT_TAIL(&alloy_head, a, nexts);
                        alloys_len += 1;

                        pthread_mutex_unlock(&alloy_mutex);
                }

                return_tools(o);

                // Take a rest.
                nanosleep(&t, NULL);
        }

 error:
        if(m) { free(m); }
        if(a) { free(a); }
        pthread_exit(0);
}

int run_simulation(int tools, int operators) {
        MetalWrap* mw;
        Operator* o;
        int ch      = 0;
        int time    = 0;
        int speed   = 0;
        int x, y, i, r;
        pthread_t generator_ts[METAL_TYPES];
        pthread_t* operators_ts;
        t.tv_sec  = 0;
        t.tv_nsec = 500000000L;  // Half a second.

        /* Set global in-use tool counter */
        tool_counter = tools;

        /* Initialize Queues */
        TAILQ_INIT(&metal_head);
        TAILQ_INIT(&alloy_head);

        /* Initialize Mutexes */
        pthread_mutex_init(&metal_mutex, NULL);
        pthread_mutex_init(&alloy_mutex, NULL);
        pthread_mutex_init(&tools_mutex, NULL);
        pthread_cond_init(&metals_full,  NULL);
        pthread_cond_init(&metals_empty, NULL);
        pthread_cond_init(&tools_gone,   NULL);

        /* Create Generator threads */
        for(i = 0; i < METAL_TYPES; i++) {
                mw = malloc(sizeof(MetalWrap));
                check_mem(mw);
                mw->metal = NoMetal + i + 1;
                mw->max_tools = tools;

                r = pthread_create(&generator_ts[i],
                                   NULL,
                                   generate_metal,
                                   (void*)mw);
                check(r == 0, "Failed to create Generator thread.");
        }

        /* Create Operator threads */
        operators_ts = malloc(sizeof(Operator) * operators);
        check_mem(operators_ts);
        for(i = 0; i < operators; i++) {
                o = operator_create(i, tools);
                check(o != NULL, "Failed to create an Operator.");

                r = pthread_create(&operators_ts[i],
                                   NULL,
                                   operate,
                                   (void*)o);
                check(r == 0, "Failed to create Operator thread.");
        }

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

        // Display Operators
        for(i = 0; i < operators; i++) {
                mvprintw(y/2 - operators + (i * 2) + 1, x/2 + tools + 4, "O");
        }
        
        // Input Queue
        for(i = 0; i < tools; i++) {
                mvprintw(y/2, x/2 - tools - 3 + i, "=");
                mvprintw(y/2, x/2 + 3 + i, "=");
        }

        /* Main event loop */
        while(ch != 'q') {
                mvprintw(3, 1, "Time: %d", time);

                // Queue lengths
                mvprintw(y/2, x/2 - 3, "[ %02d ]", metals_len);
                mvprintw(y/2, x/2 + tools + 6, ">=>[ %03d ]", alloys_len);

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

                case KEY_UP:
                        if(speed < 10) {
                                t.tv_nsec /= 2;
                                speed++;
                        }
                        break;

                case KEY_DOWN:
                        if(speed > 0) {
                                t.tv_nsec *= 2;
                                speed--;
                        }
                        break;

                default:
                        break;
                }

                if(!paused) {
                        time++;
                        nanosleep(&t, NULL);
                }
                
                print_metals(tools);
                print_tools(tools);
                refresh();
                ch = getch();
        }

        return EXIT_SUCCESS;

 error:
        return EXIT_FAILURE;
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
