#include <stdio.h>
#include <stdlib.h>
#include "readline.h"

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

int main(int argc, char **argv)
{
    char *input = NULL;
    char *result;

    while (1)
    {
        input = _readline("user> ");

        if(input && *input && *input != 0x0)
        {
            _add_history(input);
            result = rep(input);
            printf("%s\n", result);
        } else {
            break;
        }
    }

    _save_history();
}