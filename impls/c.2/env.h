#ifndef _MAL_ENV_H
#define _MAL_ENV_H

#include "types.h"

//  types.h defines Env as struct Env_s.

Env* env_make(const Env* outer);

void env_set(Env* current, MalType symbol, MalType value);

MalType env_get(const Env* current, MalType symbol);
/* Returns NULL if the symbol is not found. */

hashmap env_as_map(const Env* current);
// For debugging.

#endif
