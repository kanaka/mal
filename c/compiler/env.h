#ifndef __MAL_ENV__
#define __MAL_ENV__

#include <stdarg.h>
#include <stddef.h>

#include "hashmap.h"
#include "types.h"

#define env_get_bubble_error(env, key) ({ MalType *v = env_get((env), (key)); if (is_error(v)) { return v; }; v; })

MalEnv* build_top_env();
MalEnv* build_env(MalEnv *outer);
MalType* env_get(MalEnv *env, char *key);
MalType* env_set(MalEnv *env, char *key, MalType *val);
void env_delete(MalEnv *env, char *key);
MalEnv* env_find(MalEnv *env, char *key);
void inspect_env(MalEnv *env);

#endif
