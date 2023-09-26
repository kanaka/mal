#ifndef _MAL_TOKEN_H
#define _MAL_TOKEN_H

#include <stdint.h>

enum TokenType
{
    TOKEN_EOF,
    TOKEN_TILDE,
    TOKEN_TILDE_AT,
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_SINGLE_QUOTE,
    TOKEN_STRING,
    TOKEN_UNBALANCED_STRING,
    TOKEN_BACK_TICK,
    TOKEN_CARET,
    TOKEN_AT,
    TOKEN_COMMENT,
    TOKEN_SYMBOL,
    TOKEN_NUMBER,
    TOKEN_KOMMA,
    TOKEN_SEMI_COLON,
    TOKEN_ERROR
};

typedef struct Token
{
    enum TokenType tokenType;
    char *value;
    int64_t fixnum;
} Token;

#endif