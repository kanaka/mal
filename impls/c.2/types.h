#ifndef _MAL_TYPES_H
#define _MAL_TYPES_H

#include <stdbool.h>

// The order must match the one in printer.c.
enum mal_type_t {
  MALTYPE_SYMBOL   = 1 <<  0,
  MALTYPE_KEYWORD  = 1 <<  1,
  MALTYPE_INTEGER  = 1 <<  2,
  MALTYPE_FLOAT    = 1 <<  3,
  MALTYPE_STRING   = 1 <<  4,
  MALTYPE_TRUE     = 1 <<  5,
  MALTYPE_FALSE    = 1 <<  6,
  MALTYPE_NIL      = 1 <<  7,
  MALTYPE_LIST     = 1 <<  8,
  MALTYPE_VECTOR   = 1 <<  9,
  MALTYPE_HASHMAP  = 1 << 10,
  MALTYPE_FUNCTION = 1 << 11,
  MALTYPE_CLOSURE  = 1 << 12,
  MALTYPE_ATOM     = 1 << 13,
  MALTYPE_MACRO    = 1 << 14,
};

typedef struct MalType_s* MalType;
typedef const struct MalClosure_s* MalClosure;
typedef struct pair_s* list; // mutable for appends
typedef MalType(*function_t)(list);
typedef struct Env_s Env;
typedef const struct map* hashmap;
typedef const struct vector* vector_t;

struct MalClosure_s {

  const Env* env;
  list fnstar_args; // (parameters body)
  // parameters is a list or vector of symbols
  // If "&" is present, it stands right before the last symbol.

};

MalType make_symbol(const char* value);
MalType make_integer(long value);
MalType make_float(double value);
MalType make_keyword(const char* value);
MalType make_string(const char* value);
MalType make_list(list value);
MalType make_list_m(list value, MalType meta);
MalType make_vector(vector_t value);
MalType make_vector_m(vector_t value, MalType meta);
MalType make_hashmap(hashmap value);
MalType make_hashmap_m(hashmap value, MalType meta);
MalType make_true();
MalType make_false();
MalType make_nil();
MalType make_atom(MalType value);
MalType make_function_m(function_t value, MalType meta);
MalType make_function(function_t value);
MalType make_closure_m(const Env* env, list fnstar_args, MalType meta);
MalType make_closure(const Env* env, list fnstar_args);
MalType make_macro(const Env* env, list fnstar_args);

// A NULL result means that the type differs, except for lists.
int is_list(MalType val, list*);
vector_t is_vector(MalType val);
hashmap is_hashmap(MalType val);
int is_nil(MalType val);
const char* is_string(MalType val);
int is_false(MalType val);
const char* is_symbol(MalType val);
const char* is_keyword(MalType val);
function_t is_function(MalType val);
MalClosure is_closure(MalType val);
MalClosure is_macro(MalType val);
int is_integer(MalType val, long*);
int is_float(MalType val, double*);
MalType* is_atom(MalType val);
int is_true(MalType val);


enum mal_type_t type(MalType);
MalType meta(MalType);      // Returns nil for types without metadata.
size_t get_hash(MalType); // Crashes for types without hash.

// These parts could be implemented outside types, but improve
// readability in core, hashmap and steps.

// This also improves efficiency because
// a lost of allocations of the same symbol are avoided
// amost symbol comparisons in EVAL will only need the precomputed hash.
bool equal_forms(MalType, MalType);
extern MalType SYMBOL_AMPERSAND;
extern MalType SYMBOL_CATCH;
extern MalType SYMBOL_CONCAT;
extern MalType SYMBOL_CONS;
extern MalType SYMBOL_DEBUG_EVAL;
extern MalType SYMBOL_DEF;
extern MalType SYMBOL_DEFMACRO;
extern MalType SYMBOL_DEREF;
extern MalType SYMBOL_DO;
extern MalType SYMBOL_FN;
extern MalType SYMBOL_IF;
extern MalType SYMBOL_LET;
extern MalType SYMBOL_QUASIQUOTE;
extern MalType SYMBOL_QUOTE;
extern MalType SYMBOL_SPLICE_UNQUOTE;
extern MalType SYMBOL_TRY;
extern MalType SYMBOL_UNQUOTE;
extern MalType SYMBOL_VEC;
extern MalType SYMBOL_WITH_META;

void types_init();

//  Evil trick for FFI.
//  Should at least be const void*.
void* mal_type_value_address(MalType);

#endif
