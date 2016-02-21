#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <readline/readline.h>

/*
 * Vim interface for the readline(3) function.
 *
 * Prints 'prompt' and reads a line from the input. If EOF is encountered,
 * returns the string "E"; otherwise, returns the string "S<line>" where <line>
 * is the line read from input.
 *
 * This function is not thread-safe.
 */
char* vimreadline(char* prompt) {
    static char buf[1024];
    char* res = readline(prompt);
    if (res) {
        buf[0] = 'S';
        strncpy(buf + 1, res, sizeof(buf) - 1);
        free(res);
    } else {
        buf[0] = 'E';
        buf[1] = '\0';
    }
    return buf;
}
