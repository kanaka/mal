#ifndef _MAL_ERROR_H
#define _MAL_ERROR_H

enum MalErrorCode
{
    VALUE_REDEFINED,
    SUCCESS,
    INVALID_ARGUMENT,
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

void print_error(FILE *stream, MalError *error);
#endif