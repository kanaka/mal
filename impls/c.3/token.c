#include <stdlib.h>
#include <string.h>
#include "token.h"

Token *make_token(enum TokenType tokenType, char *start, unsigned int len)
{
    Token *token = malloc(sizeof(Token));
    // internalize strings to save space!
    token->value = malloc(len + 1);
    strncpy(token->value, start, len);
    token->value[len + 1] = '\0';

    return token;
}
