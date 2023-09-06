#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "readline.h"

static const char *HISTORY_FILENAME = ".mal_history";

char *READ(char *input)
{
    return input;
}

char *EVAL(char *input)
{
    return input;
}

char *PRINT(char *input)
{
    return input;
}

char *rep(char *input)
{
    return PRINT(EVAL(READ(input)));
}

char *get_history_filename()
{
    char *home_folder = getenv("HOME");
    char *history_file = malloc(strlen(home_folder) + strlen(HISTORY_FILENAME) + 1 + 1);
    sprintf(history_file, "%s/%s", home_folder, HISTORY_FILENAME);

    return history_file;
}

int main(int argc, char **argv)
{
    char *input = NULL;
    char *result;
    char *history_file = get_history_filename();
    _read_history(history_file);

    while (1)
    {
        input = _readline("user> ");

        if (input && *input && *input != 0x0)
        {
            _add_history(input);
            result = rep(input);
            printf("%s\n", result);
        }
        else
        {
            break;
        }
    }

    _save_history(history_file);
    free(history_file);
}