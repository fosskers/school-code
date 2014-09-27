/* Kaigara, a simple shell */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <wait.h>

#include "dbg.h"
#include "defines.h"

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
        pid_t pid;
        int status = EXIT_SUCCESS;  // For setting the initial colour.
        char* colour;

        Time* now = time_now();
        check(now, "Couldn't get the time.");

        // Set prompt happiness colour.
        if(status == EXIT_SUCCESS) {
                colour = ANSI_GREEN;
        } else {
                colour = ANSI_RED;
        }

        printf("%s%02d:%02d%s ~>\n", colour, now->hour, now->mins, ANSI_RESET);
        fflush(stdout);

        // Fork and execute.
        pid = fork();
        if(pid == 0) {
                debug("Testing `execvp`");
                char* args[] = { "ls", "-asldfj", NULL };
                execvp("ls", args);

                if(errno != 0) {
                        printf(ANSI_RED "Kaigara" ANSI_RESET " >> %s\n",
                               strerror(errno));

                        goto error;
                }
        } else {
                waitpid(pid, &status, 0);
                status = WEXITSTATUS(status);
                log_info("Back in parent - %d", status);
        }

        free(now);
        return Success;

 error:
        if(now) { free(now); }
        return Failure;
}

int main(int argc, char** argv) {
        Status r;

        debug("Starting prompt.");
        puts("Welcome to " ANSI_CYAN "Kaigara!" ANSI_RESET);
        r = prompt();
        quiet_check(r == Success);

        return EXIT_SUCCESS;

 error:
        return EXIT_FAILURE;
}
