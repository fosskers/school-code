#include <ncurses.h>
#include <stdbool.h>

#include "utils.h"
#include "dbg.h"

bool is_num(char c) {
        return c >= 48 && c <= 57; 
}

// Convert a number as a char to an int.
int ctoi(char c) {
        check(is_num(c), "int out of legal char range.");
        
        return (int)(c - 48);

 error:
        return -1;
}

char ignore_til_num() {
        char c = getch();

        while(!is_num(c)) {
                c = getch();
        }

        return c;
}
