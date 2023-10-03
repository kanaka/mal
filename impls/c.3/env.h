#ifndef _MAL_ENV_H
#define _MAL_ENV_H

#include <stdlib.h>
#include "libs/hashmap/hashmap.h"
#include "types.h"

typedef struct MalEnvironment MalEnvironment;
typedef struct MalEnvironment
{
    MalEnvironment *parent;
    HashMap *map;
} MalEnvironment;

MalEnvironment *make_environment(MalEnvironment *parent);
MalEnvironment *find_environment(MalEnvironment *start, MalValue *symbol);
MalValue *lookup_in_environment(MalEnvironment *environment, MalValue *symbol);
bool set_in_environment(MalEnvironment *environment, MalValue *symbol, MalValue *value);
void free_environment(MalEnvironment *parent);
#endif