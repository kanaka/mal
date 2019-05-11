#include <gc.h>
#include <stdio.h>
#include <stdlib.h>

#include "env.h"
#include "hashmap.h"
#include "printer.h"
#include "types.h"
#include "util.h"

MalEnv* build_top_env() {
  MalEnv *top_env = build_env(NULL);
  top_env->num = 0;
  return top_env;
}

MalEnv* build_env(MalEnv *outer) {
  MalEnv *env = GC_MALLOC(sizeof(MalEnv));
  env->outer = outer;
  hashmap_init(&env->data, hashmap_hash_string, hashmap_compare_string, 100);
  hashmap_set_key_alloc_funcs(&env->data, hashmap_alloc_key_string, NULL);
  return env;
}

MalType* env_get(MalEnv *env, char *key) {
  env = env_find(env, key);
  if (!env) {
    return mal_error(mal_sprintf("'%s' not found", key));
  }
  MalType *val = hashmap_get(&env->data, key);
  if (val) {
    return val;
  } else {
    return mal_error(mal_sprintf("'%s' not found", key));
  }
}

MalType* env_set(MalEnv *env, char *key, MalType *val) {
  if (is_blank_line(val)) return val;
  hashmap_remove(&env->data, key);
  hashmap_put(&env->data, key, val);
  return val;
}

void env_delete(MalEnv *env, char *key) {
  hashmap_remove(&env->data, key);
}

MalEnv* env_find(MalEnv *env, char *key) {
  if (hashmap_get(&env->data, key)) {
    return env;
  } else if (env->outer) {
    return env_find(env->outer, key);
  } else {
    return NULL;
  }
}
