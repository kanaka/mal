#ifndef _MAL_TYPES_H
#define _MAL_TYPES_H

#include <stdint.h>
#include "error.h"
#include "libs/hashmap/hashmap.h"

enum MalValueType
{
    MAL_CLOSURE,
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

typedef struct MalEnvironment MalEnvironment;
typedef struct MalEnvironment
{
    MalEnvironment *parent;
    HashMap *map;
} MalEnvironment;

typedef struct MalClosure
{
    MalEnvironment *environment;
    MalValue *bindings;
    MalValue *ast;
    MalValue *rest_symbol;
} MalClosure;

typedef struct MalValue
{
    enum MalValueType valueType;
    HashMap *metadata;
    union
    {
        const char *value;
        int64_t fixnum;
        MalCell *list;
        MalValue *malValue;
        HashMap *hashMap;
        MalValue *(*function)(MalCell *, MalEnvironment *environment);
        MalClosure *closure;
    };
} MalValue;

extern MalValue MAL_NIL;
extern MalValue MAL_FALSE;
extern MalValue MAL_TRUE;
extern MalValue MAL_EOF;

MalValue *new_value(enum MalValueType valueType);
MalValue *new_function(MalValue *(*function)(MalCell *args, MalEnvironment *environment));
MalValue *make_value(enum MalValueType valueType, const char *value);
MalValue *make_closure(MalEnvironment *outer, MalCell *context);

/**
 * Create a new string value.
 * 
 * @param value the string to put into the returned MalValue object.
 * @param unescape indicates whether '\\' should be replaced with a single '\' or the given string shall be placed unchanged in the return value.
 */
MalValue *make_string(char *value, bool unescape);
MalValue *make_fixnum(int64_t value);
MalValue *make_list(MalCell *values);

void push(MalValue *list, MalValue *value);
const char *put(MalValue *map, const char *key, MalValue *value);
void setMetadata(MalValue *value, HashMap *metadata);
#endif