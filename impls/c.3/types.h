#ifndef _MAL_TYPES_H
#define _MAL_TYPES_H

#include <stdint.h>
#include "libs/hashmap/hashmap.h"

enum MalValueType
{
    MAL_COMMENT,
    MAL_HASHMAP,
    MAL_FIXNUM,
    MAL_KEYWORD,
    MAL_LIST,
    MAL_NUMBER,
    MAL_STRING,
    MAL_SYMBOL,
    MAL_VECTOR,
    MAL_FUNCTION
};

typedef struct MalValue MalValue;
typedef struct MalCell MalCell;

typedef struct MalCell
{
    MalValue *value;
    MalCell *cdr;
} MalCell;

typedef struct MalValue
{
    enum MalValueType valueType;
    HashMap *metadata;
    union
    {
        char *value;
        int64_t fixnum;
        MalCell *list;
        MalValue *malValue;
        HashMap *hashMap;
        MalValue *(*function)(MalCell *);
    };
} MalValue;

extern const MalValue MAL_NIL;
extern const MalValue MAL_TRUE;
extern const MalValue MAL_EOF;

MalValue *new_value(enum MalValueType valueType);
MalValue *new_function(MalValue *(*function)(MalCell *args));
MalValue *make_value(enum MalValueType valueType, char *value);
MalValue *make_string(char *value);
MalValue *make_fixnum(int64_t value);

void push(MalValue *list, MalValue *value);
char *put(MalValue *map, char *key, MalValue *value);
void setMetadata(MalValue *value, HashMap *metadata);
#endif