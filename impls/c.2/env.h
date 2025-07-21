#ifndef _MAL_ENV_H
#define _MAL_ENV_H

#include "types.h"

//  types.h defines Env as struct Env_s.

Env* env_make(const Env* outer);

void env_set(Env* current, const char* symbol, MalType value);
/* can be called at most max times. */

MalType env_get(const Env* current, const char* symbol);
/* Returns NULL if the symbol is not found. */

#endif
