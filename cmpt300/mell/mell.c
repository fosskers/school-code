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

void show_prompt(const char* colour, const Time* now, Mood mood, const char* path) {
        int hour = now->hour;
        int mins = now->mins;
        char* face;
        const char* loc = path;

        switch(mood) {
        case Great:
                face = ANSI_CYAN ":D" ANSI_RESET;
                break;

        case Happy:
                face = ANSI_GREEN ":)" ANSI_RESET;
                break;

        case Unimpressed:
                face = ANSI_YELLOW ":|" ANSI_RESET;
                loc = "??";
                break;

        case Mad:
                face = ANSI_RED ":(" ANSI_RESET;
                hour = rand() % 24;
                mins = rand() % 60;
                loc = "??";
                break;

        case Livid:
                face = ANSI_MAGENTA ">:(" ANSI_RESET;
                hour = rand() % 24;
                mins = rand() % 60;
                loc = "??";
                break;
        }

        printf("%s%02d:%02d%s %s %s> ",
               colour, hour, mins, ANSI_RESET, face, loc);
        fflush(stdout);
}

Mood check_mood(int happiness) {
        Mood mood;

        if(happiness < 0) {
                mood = Livid;
        } else if(happiness <= 1) {
                mood = Mad;
        } else if(happiness <= 3) {
                mood = Unimpressed;
        } else if(happiness <= 10) {
                mood = Happy;
        } else {
                mood = Great;
        }

        return mood;
}

Status check_errno() {
        if(errno != 0) {
                printf("%sMell%s >> %s\n",
                       ANSI_YELLOW, ANSI_RESET, strerror(errno));

                return Failure;
        }

        return Success;
}

// Yields the name of the current working directory.
char* pwd() {
        bstring path = NULL;
        struct bstrList* list = NULL;
        char* result;
        char* full_path = getcwd(NULL, 1024);
        check(full_path, "Failed to get cwd.");

        path = bfromcstr(full_path);
        list = bsplit(path, '/');

        result = malloc(sizeof(char) * (blength(list->entry[list->qty - 1]) + 1));
        check_mem("Failed to get memory for the parsed cwd.");
        result = strcpy(result, bdata(list->entry[list->qty - 1]));

        free(full_path);
        bdestroy(path);
        bstrListDestroy(list);
        return result;

 error:
        if(full_path) { free(full_path); }
        if(path)      { bdestroy(path);  }
        if(list)      { bstrListDestroy(list); }
        return NULL;
}

// Splits and writes the contents of `line` to a given String array.
void parse_cmd(const bstring line, char** args) {
        int i = 0;
        struct bstrList* list = bsplit(line, ' ');
        
        for(i = 0; i < list->qty && i < MAX_ARGS-1; i++) {
                args[i] = bdata(list->entry[i]);
        }

        args[i] = NULL;

        //        bstrListDestroy(list);
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

int prompt() {
        Mood mood;
        Time* now;
        bstring line = NULL;
        char* args[MAX_ARGS];
        char* colour = ANSI_GREEN;
        char* path;
        int happiness = 5;
        int status = EXIT_SUCCESS;  // For setting the initial colour.
        pid_t pid;

        while(true) {
                now = time_now();
                check(now, "Couldn't get the time.");

                // Set prompt mood.
                mood = check_mood(happiness);
                path = pwd();

                // User input.
                show_prompt(colour, now, mood, path);
                line = bgets((bNgetc)fgetc, stdin, '\n');
                btrimws(line);

                if(happiness < 0 && (rand() % 20) < 10) {
                        printf("%sMell%s >> No, I'm mad at you.\n",
                               ANSI_YELLOW,
                               ANSI_RESET);
                } else if(blength(line) > 0) {
                        // Empty input is silently ignored.
                        parse_cmd(line, args);

                        // Check for shell-specific commands.
                        if(strcmp(args[0], "exit") == 0) {
                                puts("Goodbye!");
                                break;
                        } else if(strcmp(args[0], "cd") == 0) {
                                if(args[1] != NULL) { chdir(args[1]); }
                                status = EXIT_SUCCESS;
                        } else {
                                // Fork and execute.
                                pid = fork();
                                if(pid == 0) {
                                        execvp(args[0], args);
                                        if(check_errno() == Failure) { goto error; }
                                } else {
                                        waitpid(pid, &status, 0);
                                        status = WEXITSTATUS(status);
                                        debug("Child exited with: %d", status);
                                }
                        }

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

                // Free memory used in this loop.
                free(path);
                free(now);
                bdestroy(line);
        }

        if(path) { free(path);     }
        if(now)  { free(now);      }
        if(line) { bdestroy(line); }
        return EXIT_SUCCESS;

 error:
        if(path) { free(path);     }
        if(now)  { free(now);      }
        if(line) { bdestroy(line); }
        return EXIT_FAILURE;
}

int main(int argc, char** argv) {
        // Set the generator seed.
        srand(time(NULL));

        debug("Starting prompt.");
        puts("Welcome to " ANSI_CYAN "Mell" ANSI_RESET ", the moody shell!");
        printf("Try not to make her %smad%s...\n", ANSI_RED, ANSI_RESET);
        return prompt();
}
