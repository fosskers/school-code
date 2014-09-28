/* mShell, the moody shell */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <wait.h>

#include "dbg.h"
#include "defines.h"
#include "lib/bstrlib.h"

// --- //

char* check_mood(char* mood, int happiness) {
        if(happiness < 0) {
                mood = ANSI_MAGENTA ">:(" ANSI_RESET;
        } else if(happiness <= 1) {
                mood = ANSI_RED ":(" ANSI_RESET;
        } else if(happiness <= 3) {
                mood = ANSI_YELLOW ":|" ANSI_RESET;
        } else if(happiness <= 10) {
                mood = ANSI_GREEN ":)" ANSI_RESET;
        } else {
                mood = ANSI_CYAN ":D" ANSI_RESET;
        }

        return mood;
}

// Splits and writes the contents of `line` to a given String array.
void parse_cmd(const bstring line, char** args) {
        int i = 0;
        struct bstrList* list = bsplit(line, ' ');
        
        for(i = 0; i < list->qty && i < MAX_ARGS-1; i++) {
                args[i] = bdata(list->entry[i]);
        }

        args[i] = NULL;
}

Time* time_now() {
        // Get current time.
        time_t curr;
        struct tm* timeinfo;
        time(&curr);
        timeinfo = localtime(&curr);

        // Form `Time` data.
        Time* now = malloc(sizeof(Time));
        check_mem(now);
        now->mins = timeinfo->tm_min;
        now->hour = timeinfo->tm_hour; 

        return now;

 error:
        return NULL;
}

Status prompt() {
        Time* now;
        bstring line = NULL;
        char* args[MAX_ARGS];
        char* colour = ANSI_GREEN;
        char* mood;
        int happiness = 5;
        int status = EXIT_SUCCESS;  // For setting the initial colour.
        pid_t pid;

        while(true) {
                now = time_now();
                check(now, "Couldn't get the time.");

                // Set prompt mood.
                mood = check_mood(mood, happiness);

                // User input.
                printf("%s%02d:%02d%s %s ~> ",
                       colour, now->hour, now->mins, ANSI_RESET, mood);
                fflush(stdout);
                line = bgets((bNgetc)fgetc, stdin, '\n');
                btrimws(line);

                if(happiness < 0 && (rand() % 20) < 10) {
                        printf("%sMell%s >> No, I'm mad at you.\n",
                               ANSI_YELLOW,
                               ANSI_RESET);
                } else if(blength(line) > 0) {
                        // Empty input is silently ignored.
                        parse_cmd(line, args);

                        // Fork and execute.
                        pid = fork();
                        if(pid == 0) {
                                execvp(args[0], args);

                                if(errno != 0) {
                                        printf("%sMell%s >> %s\n",
                                               ANSI_YELLOW,
                                               ANSI_RESET,
                                               strerror(errno));

                                        goto error;
                                }
                        } else {
                                waitpid(pid, &status, 0);
                                status = WEXITSTATUS(status);
                                debug("Child exited with: %d", status);

                                // Set prompt happiness colour.
                                if(status == EXIT_SUCCESS) {
                                        colour = ANSI_GREEN;
                                        happiness++;
                                } else if(status == EXIT_FAILURE) {
                                        colour = ANSI_RED;
                                        happiness -= 2;
                                } else {
                                        colour = ANSI_YELLOW;
                                        happiness--;
                                }
                        }
                }
        }

        if(now)  { free(now); }
        if(line) { bdestroy(line); }
        return Success;

 error:
        if(now)  { free(now); }
        if(line) { bdestroy(line); }
        return Failure;
}

int main(int argc, char** argv) {
        Status r;

        // Set the generator seed.
        srand(time(NULL));

        debug("Starting prompt.");
        puts("Welcome to " ANSI_CYAN "Mell" ANSI_RESET ", the moody shell!");
        printf("Try not to make her %smad%s...\n", ANSI_RED, ANSI_RESET);
        r = prompt();
        quiet_check(r == Success);

        return EXIT_SUCCESS;

 error:
        return EXIT_FAILURE;
}
