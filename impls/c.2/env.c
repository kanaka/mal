#include <stdio.h>
#include <gc.h>

#include "libs/hashmap/hashmap.h"
#include "types.h"
#include "env.h"
#include "reader.h"

/* Note: caller must make sure enough exprs to match symbols */
Env* env_make(Env* outer, list symbol_list, list exprs_list, MalType* more_symbol) {

  Env*  env = GC_MALLOC(sizeof(*env));
  env->outer = outer;
  env->data = NULL;

  while (symbol_list) {

    env = env_set(env, symbol_list->data, exprs_list->data);

    symbol_list = symbol_list->next;
    exprs_list = exprs_list->next;
  }

  /* set the 'more' symbol if there is one */
  if (more_symbol) {
    env = env_set(env, more_symbol, make_list(exprs_list));
  }
  return env;
}

Env* env_set(Env* current, MalType* symbol, MalType* value) {

  current->data = hashmap_put(current->data, symbol->value.mal_symbol, value);
  return current;
}

Env* env_find(Env* current, MalType* symbol) {

  MalType* val = hashmap_get(current->data, symbol->value.mal_symbol);

  if (val) {
    return current;
  }
  else if (current->outer) {
    return env_find(current->outer, symbol);
  }
  else {
    return NULL; /* not found */
  }
}

MalType* env_get(Env* current, MalType* symbol) {

  Env* env = env_find(current, symbol);

  if (env) {
    return hashmap_get(env->data, symbol->value.mal_symbol);
  }
  else {
    return make_error_fmt("'%s' not found", symbol->value.mal_symbol);
  }
}

Env* env_set_C_fn(Env* current, char* symbol_name, MalType*(*fn)(list)) {

  return env_set(current, make_symbol(symbol_name), make_function(fn));
}
