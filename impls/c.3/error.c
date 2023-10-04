#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "error.h"

void print_error(FILE *stream, MalError *error)
{
    switch (error->errno)
    {
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
        fprintf(stream, "'%s' not found", error->args[0]);
        break;
    case UNBALANCED_STRING:
        fprintf(stream, "missing closing quote: \"%s\"", error->args[0]);
        break;
    default:
        break;
    }
}