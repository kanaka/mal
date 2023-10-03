#ifndef _MAL_ERROR_H
#define _MAL_ERROR_H

enum MalErrorCode
{
    SUCCESS,
    MISSING_CLOSING_PAREN,
    MISSING_CLOSING_BRACE,
    MISSING_CLOSING_BRACKET,
    SYMBOL_NOT_FOUND,
    UNEXPECTED_EOF,
    VALUE_REDEFINED
};

typedef struct MalError
{
    enum MalErrorCode errno;
    char **args;
} MalError;

char *get_error_message(enum MalErrorCode errno);
#endif