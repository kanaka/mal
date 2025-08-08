#include <gc.h>

#include "env.h"
#include "hashmap.h"

struct Env_s {
  const Env*     outer;
  struct map*    data;
};

Env* env_make(const Env* outer) {
  struct Env_s* env = GC_MALLOC(sizeof(*env));
  env->outer = outer;
  env->data = map_empty();
  return env;
}

inline void env_set(Env* current, MalType symbol, MalType value) {
  current->data = hashmap_put(current->data, symbol, value);
}

MalType env_get(const Env* current, MalType symbol) {
  do {
    MalType value = hashmap_get(current->data, symbol);
    if (value) {
      return value;
    }
  } while((current = current->outer));
  return NULL;
}

hashmap env_as_map(const Env* current) {
  return current->data;
}
