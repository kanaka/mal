#ifndef _MAL_TYPES_H
#define _MAL_TYPES_H

#include "libs/hashmap/hashmap.h"

enum MalValueType
{
    MAL_COMMENT,
    MAL_HASHMAP,
    MAL_INTEGER,
    MAL_KEYWORD,
    MAL_LIST,
    MAL_NUMBER,
    MAL_STRING,
    MAL_SYMBOL,
    MAL_VECTOR
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
        MalCell *list;
        MalValue *malValue;
        HashMap *hashMap;
    };
} MalValue;

MalValue *new_value(enum MalValueType valueType);
MalValue *make_value(enum MalValueType valueType, char *value);
MalValue *make_string(char *value);
MalValue *make_list();
MalValue *make_vector();

void push(MalValue *list, MalValue *value);
char *put(MalValue *map, char *key, MalValue *value);
void setMetadata(MalValue *value, HashMap *metadata);
#endif