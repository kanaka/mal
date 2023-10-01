#ifndef _MAL_READER_H
#define _MAL_READER_H
#include "token.h"
#include "types.h"

typedef struct Reader
{
    char *input;
    Token *token;
    enum MalErrorCode errno;
} Reader;

MalValue *read_str(Reader *reader);
#endif