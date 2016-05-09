#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <readline/readline.h>
#include <sys/time.h>

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

#define UNIXTIME_2000_01_01 946684800

/*
 * Returns the number of milliseconds since 2000-01-01 00:00:00 UTC.
 *
 * This date is chosen (instead of the standard 1970 epoch) so the number of
 * milliseconds will not exceed a 32-bit integer, which is the limit for Vim
 * number variables.
 */
int vimtimems(int dummy) {
    struct timeval tv;
    (void) dummy; /* unused */
    gettimeofday(&tv, NULL);
    return (tv.tv_sec - UNIXTIME_2000_01_01) * 1000 + (tv.tv_usec / 1000);
}
