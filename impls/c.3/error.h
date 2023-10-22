#ifndef _MAL_ERROR_H
#define _MAL_ERROR_H

#include <stdio.h>

enum MalErrorCode
{
    VALUE_REDEFINED,
    SUCCESS,
    INVALID_ARGUMENT,
    INVALID_ARGUMENT_COUNT,
    ILLEGAL_ARGUMENT_TYPE,
    MISSING_CLOSING_PAREN,
    MISSING_CLOSING_BRACE,
    MISSING_CLOSING_BRACKET,
    SYMBOL_NOT_FOUND,
    UNBALANCED_STRING,
    UNEXPECTED_EOF
};

typedef struct MalError
{
    enum MalErrorCode errno;
    char **args;
} MalError;

void print_error(FILE *stream);
void register_error(enum MalErrorCode, const char *value, ...);
#endif