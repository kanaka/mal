#include <stdarg.h>
#include <stdio.h>

#include <gc.h>

#include "types.h"

struct MalType_s THE_NIL = { MALTYPE_NIL, &THE_NIL, {0}};
struct MalType_s THE_TRUE = { MALTYPE_TRUE, &THE_NIL, {0}};
struct MalType_s THE_FALSE = { MALTYPE_FALSE, &THE_NIL, {0}};

#define generic_is(name, mask)         \
  inline int is_##name(MalType val) { \
    return val->type & (mask);         \
  }
generic_is(sequential, MALTYPE_LIST | MALTYPE_VECTOR)
generic_is(list, MALTYPE_LIST);
generic_is(vector, MALTYPE_VECTOR);
generic_is(hashmap, MALTYPE_HASHMAP);
generic_is(nil, MALTYPE_NIL);
generic_is(string, MALTYPE_STRING);
generic_is(false, MALTYPE_FALSE);
generic_is(symbol, MALTYPE_SYMBOL);
generic_is(keyword, MALTYPE_KEYWORD);
generic_is(error, MALTYPE_ERROR);
generic_is(callable, MALTYPE_CLOSURE | MALTYPE_FUNCTION | MALTYPE_MACRO);
generic_is(function, MALTYPE_FUNCTION);
generic_is(closure, MALTYPE_CLOSURE);
generic_is(macro, MALTYPE_MACRO);

#define generic_make(name, ctype, mtype, mfield)             \
  MalType name(ctype value) {                                \
    struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val)); \
    *mal_val = (struct MalType_s){                           \
      .type     = mtype,                                     \
      .metadata = &THE_NIL,                                  \
      .value    = { .mal_##mfield = value },                 \
    };                                                       \
    return mal_val;                                          \
  }
generic_make(make_symbol,   const char*, MALTYPE_SYMBOL,   string)
generic_make(make_integer,  long,        MALTYPE_INTEGER,  integer)
generic_make(make_float,    double,      MALTYPE_FLOAT,    float)
generic_make(make_keyword,  const char*, MALTYPE_KEYWORD,  string)
generic_make(make_string,   const char*, MALTYPE_STRING,   string)
generic_make(make_list,     list,        MALTYPE_LIST,     list)
generic_make(make_vector,   list,        MALTYPE_VECTOR,   list)
generic_make(make_hashmap,  list,        MALTYPE_HASHMAP,  list)
generic_make(make_function, function_t,  MALTYPE_FUNCTION, function)
generic_make(wrap_error,    MalType,     MALTYPE_ERROR,    error);
generic_make(make_macro,    MalClosure,  MALTYPE_MACRO,    closure)

MalType make_atom(MalType value) {

  MalType* atm_val = GC_MALLOC(sizeof(*atm_val));
  *atm_val = value;

  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_ATOM;
  mal_val->value.mal_atom = atm_val;
  mal_val->metadata = &THE_NIL;

  return mal_val;
}

MalType make_closure(const Env* env, size_t param_len,
                     const char** parameters,
                     MalType definition, const char* more_symbol) {

  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  mal_val->type = MALTYPE_CLOSURE;
  mal_val->metadata = &THE_NIL;

  /* Allocate memory for embedded struct */
  struct MalClosure_s* mc = GC_MALLOC(sizeof(*mc));
  mc->env = env;
  mc->param_len = param_len;
  mc->parameters = parameters;
  mc->definition = definition;
  mc->more_symbol = more_symbol;

  mal_val->value.mal_closure = mc;
  return mal_val;
}

inline MalType make_true() {
  return &THE_TRUE;
}

inline MalType make_false() {
  return &THE_FALSE;
}

inline MalType make_nil() {
  return &THE_NIL;
}

MalType with_meta(MalType data, MalType meta) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){
    .type     = data->type,
    .metadata = meta,
    .value    = data->value,
  };
  return mal_val;
}
