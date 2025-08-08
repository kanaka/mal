#include <assert.h>
#ifdef DEBUG_HASH
#  include "stdio.h"
#endif
#include <string.h>

#include <gc.h>

#include "types.h"
#include "vector.h"
#include "hashmap.h"
#include "linked_list.h"
#ifdef DEBUG_HASH
#  include "printer.h"
#endif

struct MalType_s {
  enum mal_type_t type;

  union MalType_u {
    long mal_integer;
    double mal_float;
    struct {
      const char* s;
      size_t      hash;
    } mal_string;
    struct {
      list    l;
      MalType meta;
    } mal_list;
    struct {
      vector_t v;
      MalType  meta;;
    } mal_vector;
    struct {
      hashmap m;
      MalType meta;
    } mal_hashmap;
    struct {
      function_t f;
      MalType    meta;
    } mal_function;
    struct {
      struct MalClosure_s c;
      MalType meta;
    } mal_closure;
    MalType* mal_atom;          // This pointer allows mutability.
  } value;
};

struct MalType_s THE_NIL = { MALTYPE_NIL, {0}};
struct MalType_s THE_TRUE = { MALTYPE_TRUE, {0}};
struct MalType_s THE_FALSE = { MALTYPE_FALSE, {0}};

int is_nil(MalType val) { return val == &THE_NIL; }
int is_false(MalType val) { return val == &THE_FALSE; }
int is_true(MalType val) { return val == &THE_TRUE; }

inline int is_integer(MalType val, long* result) {
  int ok = val->type & MALTYPE_INTEGER;
  if (ok) *result = val->value.mal_integer;
  return ok;
}
MalType make_integer(long value) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_INTEGER, {.mal_integer=value}};
  return mal_val;
}

inline int is_float(MalType val, double* result) {
  int ok = val->type & MALTYPE_FLOAT;
  if (ok) *result = val->value.mal_float;
  return ok;
}
MalType make_float(double value) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_FLOAT, {.mal_float=value}};
  return mal_val;
}

size_t hash(const char* s) {
# ifdef DEBUG_HASH
  printf("HASH %s\n", s);
# endif
  size_t h = 0;
  // 8 characters are sufficient to ensure distinct hashes for
  // "keyword" and "keyword?".
  for (size_t i = 0; i < 8; i++) {
    unsigned char c = s[i];
    if (!c) break;
    h = h << 1 ^ c;
#   ifdef DEBUG_HASH
    printf("HASH  %c %08b %064lb\n", c, c, h);
#   endif
  }
  return h;
}

inline const char* is_string(MalType val) {
  return val->type & MALTYPE_STRING ? val->value.mal_string.s : NULL;
}
MalType make_string(const char*  value) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_STRING, {.mal_string={value, hash(value)}}};
  return mal_val;
}

inline const char* is_keyword(MalType val) {
  return val->type & MALTYPE_KEYWORD ? val->value.mal_string.s : NULL;
}
MalType make_keyword(const char*  value) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_KEYWORD, {.mal_string={value, hash(value)}}};
  return mal_val;
}

inline const char* is_symbol(MalType val) {
  return val->type & MALTYPE_SYMBOL ? val->value.mal_string.s : NULL;
}
MalType make_symbol(const char*  value) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_SYMBOL, {.mal_string={value, hash(value)}}};
  return mal_val;
}

inline int is_list(MalType val, list* result) {
  int ok = val->type & MALTYPE_LIST;
  if (ok) *result = val->value.mal_list.l;
  return ok;
}
MalType make_list_m(list value, MalType metadata) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_LIST, {.mal_list={value, metadata}}};
  return mal_val;
}
inline MalType make_list(list value) {
  return make_list_m(value, &THE_NIL);
}

inline vector_t is_vector(MalType val) {
  return val->type & MALTYPE_VECTOR ? val->value.mal_vector.v : NULL;
}
MalType make_vector_m(vector_t value, MalType metadata) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_VECTOR, {.mal_vector={value, metadata}}};
  return mal_val;
}
inline MalType make_vector(vector_t value) {
  return make_vector_m(value, &THE_NIL);
}

inline hashmap is_hashmap(MalType val) {
  return val->type & MALTYPE_HASHMAP ? val->value.mal_hashmap.m : NULL;
}
MalType make_hashmap_m(hashmap value, MalType metadata) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_HASHMAP, {.mal_hashmap={value, metadata}}};
  return mal_val;
}
inline MalType make_hashmap(hashmap value) {
  return make_hashmap_m(value, &THE_NIL);
}

inline function_t is_function(MalType val) {
  return val->type & MALTYPE_FUNCTION ? val->value.mal_function.f : NULL;
}
MalType make_function_m(function_t value, MalType metadata) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_FUNCTION, {.mal_function={value, metadata}}};
  return mal_val;
}
inline MalType make_function(function_t value) {
  return make_function_m(value, &THE_NIL);
}

inline MalClosure is_closure(MalType val) {
  return val->type & MALTYPE_CLOSURE ? &val->value.mal_closure.c : NULL;
}
MalType make_closure_m(const Env* env, list fnstar_args, MalType metadata) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_CLOSURE, {.mal_closure={{env, fnstar_args}, metadata}}};
  return mal_val;
}
inline MalType make_closure(const Env* env, list fnstar_args) {
  return make_closure_m(env, fnstar_args, &THE_NIL);
}

inline MalClosure is_macro(MalType val) {
  return val->type & MALTYPE_MACRO ? &val->value.mal_closure.c : NULL;
}
MalType make_macro(const Env* env, list fnstar_args) {
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_MACRO, {.mal_closure={{env, fnstar_args}, &THE_NIL}}};
  return mal_val;
}

inline MalType* is_atom(MalType val) {
  return val->type & MALTYPE_ATOM ? val->value.mal_atom : NULL;
}
MalType make_atom(MalType value) {
  MalType* mal_atom = GC_MALLOC(sizeof(*mal_atom));
  *mal_atom = value;
  struct MalType_s* mal_val = GC_MALLOC(sizeof(*mal_val));
  *mal_val = (struct MalType_s){MALTYPE_ATOM, {.mal_atom=mal_atom}};
  return mal_val;
}

MalType meta(MalType form) {
  switch (form->type) {
  case MALTYPE_LIST    : return form->value.mal_list    .meta;
  case MALTYPE_VECTOR  : return form->value.mal_vector  .meta;
  case MALTYPE_HASHMAP : return form->value.mal_hashmap .meta;
  case MALTYPE_FUNCTION: return form->value.mal_function.meta;
  case MALTYPE_CLOSURE : return form->value.mal_closure .meta;
  default: return &THE_NIL;
  }
}

inline enum mal_type_t type(MalType val) {
  return val->type;
}

inline size_t get_hash(MalType form) {
  assert(form->type & (MALTYPE_KEYWORD | MALTYPE_STRING | MALTYPE_SYMBOL));
  return form->value.mal_string.hash;
}

void* mal_type_value_address(MalType form) {
  switch (form->type) {
  case MALTYPE_NIL:
  case MALTYPE_INTEGER:
    return (void*)&form->value.mal_integer;
  case MALTYPE_STRING:
    return (void*)&form->value.mal_string.s;
  case MALTYPE_FLOAT:
    return (void*)&form->value.mal_integer;
  default:
    assert(false);
    return NULL; // silent a warning when NDEBUG.
  }
}

bool equal_forms(MalType first, MalType second) {
  // Compare strings as soon as possible because EVAL, map_get and
  // env_get call this function often.  Conclude early if the hashes
  // do not match.

  if (first->type & (MALTYPE_LIST | MALTYPE_VECTOR)) {
    if (second->type & ~ (MALTYPE_LIST | MALTYPE_VECTOR)) return false;
    seq_cursor c2 = seq_iter(second);
    for (seq_cursor c1 = seq_iter(first);
         seq_cont(first, c1);
         c1 = seq_next(first, c1)) {
      if (!seq_cont(second, c2)
          || !equal_forms(seq_item(first, c1), seq_item(second, c2)))
        return false;
      c2 = seq_next(second, c2);
    }
    return !seq_cont(second, c2);
  }

  if (first->type != second->type) return false;

  if (first->type & (MALTYPE_KEYWORD | MALTYPE_STRING | MALTYPE_SYMBOL)) {
    return (first->value.mal_string.hash == second->value.mal_string.hash)
      && !strcmp(first->value.mal_string.s, second->value.mal_string.s);
  }

  if (first->type & (MALTYPE_NIL | MALTYPE_FALSE | MALTYPE_TRUE)) return true;

  if (first->type == MALTYPE_INTEGER) {
    return first->value.mal_integer == second->value.mal_integer;
  }
  if (first->type == MALTYPE_FLOAT) {
    return first->value.mal_float == second->value.mal_float;
  }
  if (first->type == MALTYPE_HASHMAP) {
    hashmap m1 = first->value.mal_hashmap.m;
    hashmap m2 = second->value.mal_hashmap.m;
    if (map_count(m1) != map_count(m2))
      return false;
    for (map_cursor c = map_iter(m1); map_cont(m1, c); c = map_next(m1, c)) {
      MalType val2 = hashmap_get(m2, map_key(m1, c));
        if (!val2 || !equal_forms(map_val(m1, c), val2))
          return false;
      }
    return true;
  }
  return false;
}

MalType SYMBOL_AMPERSAND;
MalType SYMBOL_CATCH;
MalType SYMBOL_CONCAT;
MalType SYMBOL_CONS;
MalType SYMBOL_DEBUG_EVAL;
MalType SYMBOL_DEF;
MalType SYMBOL_DEFMACRO;
MalType SYMBOL_DEREF;
MalType SYMBOL_DO;
MalType SYMBOL_FN;
MalType SYMBOL_IF;
MalType SYMBOL_LET;
MalType SYMBOL_QUASIQUOTE;
MalType SYMBOL_QUOTE;
MalType SYMBOL_SPLICE_UNQUOTE;
MalType SYMBOL_TRY;
MalType SYMBOL_UNQUOTE;
MalType SYMBOL_VEC;
MalType SYMBOL_WITH_META;

void types_init() {
  SYMBOL_AMPERSAND      = make_symbol("&");
  SYMBOL_CATCH          = make_symbol("catch*");
  SYMBOL_CONCAT         = make_symbol("concat");
  SYMBOL_CONCAT         = make_symbol("concat");
  SYMBOL_CONS           = make_symbol("cons");
  SYMBOL_DEBUG_EVAL     = make_symbol("DEBUG-EVAL");
  SYMBOL_DEF            = make_symbol("def!");
  SYMBOL_DEFMACRO       = make_symbol("defmacro!");
  SYMBOL_DEREF          = make_symbol("deref");
  SYMBOL_DO             = make_symbol("do");
  SYMBOL_FN             = make_symbol("fn*");
  SYMBOL_IF             = make_symbol("if");
  SYMBOL_LET            = make_symbol("let*");
  SYMBOL_QUASIQUOTE     = make_symbol("quasiquote");
  SYMBOL_QUOTE          = make_symbol("quote");
  SYMBOL_SPLICE_UNQUOTE = make_symbol("splice-unquote");
  SYMBOL_TRY            = make_symbol("try*");
  SYMBOL_UNQUOTE        = make_symbol("unquote");
  SYMBOL_VEC            = make_symbol("vec");
  SYMBOL_WITH_META      = make_symbol("with-meta");
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
