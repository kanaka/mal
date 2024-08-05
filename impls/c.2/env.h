#ifndef _MAL_ENV_H
#define _MAL_ENV_H

#include "libs/linked_list/linked_list.h"
#include "libs/hashmap/hashmap.h"
#include "types.h"

typedef struct Env_s Env;

struct Env_s {

  struct Env_s* outer;
  hashmap data;

};

Env* env_make(Env* outer, list binds, list exprs, MalType* variadic_symbol);
void env_set(Env* current, char* symbol, MalType* value);
MalType* env_get(Env* current, char* symbol);

#endif
