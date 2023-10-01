#include "error.h"
#include <stdlib.h>

char *get_error_message(enum MalErrorCode errno)
{
    char *message = NULL;

    switch (errno)
    {
    case MISSING_CLOSING_BRACE:
        message = "unbalanced}";
        break;
    case MISSING_CLOSING_BRACKET:
        message = "unbalanced]";
        break;
    case MISSING_CLOSING_PAREN:
        message = "unbalanced)";
        break;
    case UNEXPECTED_EOF:
        message = "EOF";
    default:
        break;
    }

    return message;
}
