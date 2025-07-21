#include <assert.h>
#include <string.h>

#include <gc.h>

#include "env.h"

// Binary trees here are much easyer than for hash maps because there
// is one key type and we can update the structure in place.
// This costs less than a hashmap for little dictionnaries,
// while helping a lot for the REPL environment.

struct bind_s {
  const char* key;
  MalType value;
  struct bind_s* left;
  struct bind_s* right;
};

struct Env_s {
  const Env*     outer;
  struct bind_s* data;
};

Env* env_make(const Env* outer) {
  struct Env_s* env = GC_MALLOC(sizeof(*env));
  env->outer = outer;
  env->data = NULL;
  return env;
}

void env_set(Env* current, const char* symbol, MalType value) {

  struct bind_s** p = &current->data;
  while(*p) {
    int cmp = strcmp(symbol, (*p)->key);
    if(cmp == 0) {
      (*p)->value = value;;
      return;
    }
    p = cmp < 0 ? &((*p)->left) : &((*p)->right);
  }
  *p = GC_MALLOC(sizeof(**p));
  **p = (struct bind_s){ .key=symbol, .value=value, .left=NULL, .right=NULL};
}

MalType env_get(const Env* current, const char* symbol) {
  do {
    const struct bind_s* p = current->data;
    while(p != NULL) {
      int cmp = strcmp(symbol, p->key);
      if(cmp == 0)
        return p->value;
      p = cmp < 0 ? p->left : p->right;
    }
  } while((current = current->outer));
  return NULL;
}
