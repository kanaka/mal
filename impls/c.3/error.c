#include <malloc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "gc.h"

MalError *errors = NULL;

void print_error(FILE *stream)
{
    if (!errors)
    {
        return;
    }

    switch (errors->errno)
    {
    case INVALID_ARGUMENT:
        fprintf(stream, "invalid argument: %s", errors->args[0]);
        break;
    case INVALID_ARGUMENT_COUNT:
        fprintf(stream, "invalid count of arguments: %s", errors->args[0]);
        break;
        case ILLEGAL_ARGUMENT_TYPE:
        fprintf(stream, "invalid type of arguments: %s", errors->args[0]);
        break;
    case MISSING_CLOSING_BRACE:
        fprintf(stream, "unbalanced}");
        break;
    case MISSING_CLOSING_BRACKET:
        fprintf(stream, "unbalanced]");
        break;
    case MISSING_CLOSING_PAREN:
        fprintf(stream, "unbalanced)");
        break;
    case UNEXPECTED_EOF:
        fprintf(stream, "EOF");
        break;
    case SYMBOL_NOT_FOUND:
        fprintf(stream, "'%s' not found", errors->args[0]);
        break;
    case UNBALANCED_STRING:
        fprintf(stream, "missing closing quote: \"%s\"", errors->args[0]);
        break;
    default:
        break;
    }
}

void register_error(enum MalErrorCode errno, const char *value, ...)
{
    MalError *error = calloc(1, sizeof(MalError));

    error->errno = errno;
    error->args = mal_malloc(sizeof(char **[1]));
    error->args[0] = value;

    errors = error;
}