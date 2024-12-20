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

    env_set(env, ((MalType*)symbol_list->data)->value.mal_symbol, exprs_list->data);

    symbol_list = symbol_list->next;
    exprs_list = exprs_list->next;
  }

  /* set the 'more' symbol if there is one */
  if (more_symbol) {
    env_set(env, more_symbol->value.mal_symbol, make_list(exprs_list));
  }
  return env;
}

void env_set(Env* current, char* symbol, MalType* value) {

  current->data = hashmap_put(current->data, symbol, value);

}

MalType* env_get(Env* current, char* symbol) {

  MalType* val = hashmap_get(current->data, symbol);

  if (val) {
    return val;
  }
  else if (current->outer) {
    return env_get(current->outer, symbol);
  }
  else {
    return NULL; /* not found */
  }
}
