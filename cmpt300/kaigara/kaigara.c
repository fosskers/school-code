/* Kaigara, a simple shell */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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
        now->mins = timeinfo->tm_min;
        now->hour = timeinfo->tm_hour; 

        return now;
}

Status prompt() {
        Time* now = time_now();
        check(now, "Couldn't get the time.");

        printf("%02d:%02d>", now->hour, now->mins);

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
        check(r == Success, "The prompt gave out.");

        return EXIT_SUCCESS;

 error:
        return EXIT_FAILURE;
}
