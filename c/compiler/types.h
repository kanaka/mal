#ifndef __MAL_TYPES__
#define __MAL_TYPES__

#include <gc.h>
#include <stddef.h>

#include "hashmap.h"

typedef struct MalType MalType;
typedef struct MalEnv MalEnv;

struct MalEnv {
  size_t num;
  struct hashmap data;
  MalEnv *outer;
};

enum MalTypeType {
  MAL_NIL_TYPE,
  MAL_TRUE_TYPE,
  MAL_FALSE_TYPE,
  MAL_EMPTY_TYPE,
  MAL_CONS_TYPE,
  MAL_KEYWORD_TYPE,
  MAL_NUMBER_TYPE,
  MAL_SYMBOL_TYPE,
  MAL_VECTOR_TYPE,
  MAL_HASHMAP_TYPE,
  MAL_STRING_TYPE,
  MAL_LAMBDA_TYPE,
  MAL_CONTINUATION_TYPE,
  MAL_ATOM_TYPE,
  MAL_BLANK_LINE_TYPE,
  MAL_ERROR_TYPE
};

struct MalType {
  enum MalTypeType type;

  union {
    long long number;
    char *symbol;
    char *keyword;
    struct hashmap hashmap;
    MalType *atom_val;
    MalType *error_val;

    // MAL_CONS_TYPE
    struct {
      MalType *car;
      MalType *cdr;
    };

    // MAL_VECTOR_TYPE
    struct {
      size_t vec_len;
      size_t vec_cap;
      MalType **vec;
    };

    // MAL_STRING_TYPE
    struct {
      size_t str_len;
      size_t str_cap;
      char *str;
    };

    // MAL_LAMBDA_TYPE, MAL_CONTINUATION_TYPE
    struct {
      MalType* (*fn)(MalEnv *env, size_t argc, MalType **args);
      char *function_name;
      MalEnv *env;
      size_t argc;
      MalType **args;
    };
  };

  int is_macro;
  MalType *meta;
};

#define is_primitive(val) ((val)->type == MAL_NIL_TYPE || \
                           (val)->type == MAL_FALSE_TYPE || \
                           (val)->type == MAL_TRUE_TYPE || \
                           (val)->type == MAL_EMPTY_TYPE || \
                           (val)->type == MAL_KEYWORD_TYPE || \
                           (val)->type == MAL_NUMBER_TYPE || \
                           (val)->type == MAL_SYMBOL_TYPE || \
                           (val)->type == MAL_STRING_TYPE)

MalType* mal_alloc();

MalType* mal_nil();
int is_nil(MalType *val);

MalType* mal_empty();
int is_empty(MalType *val);

MalType* mal_true();
MalType* mal_false();

#define is_true(val) ((val)->type == MAL_TRUE_TYPE)
#define is_false(val) ((val)->type == MAL_FALSE_TYPE)
#define is_truthy(val) ((val)->type != MAL_NIL_TYPE && (val)->type != MAL_FALSE_TYPE)
#define is_falsey(val) ((val)->type == MAL_NIL_TYPE || (val)->type == MAL_FALSE_TYPE)

MalType* mal_cons(MalType *car, MalType *cdr);
#define is_cons(val) ((val)->type == MAL_CONS_TYPE)
MalType* mal_car(MalType *val);
MalType* mal_cdr(MalType *val);

size_t mal_list_len(MalType *val);
MalType* mal_list_ref(MalType *val, size_t index);

MalType* mal_vector();
#define is_vector(val) ((val)->type == MAL_VECTOR_TYPE)
size_t mal_vector_len(MalType *vector);
MalType* mal_vector_ref(MalType *val, size_t index);
void mal_vector_push(MalType *vector, MalType *value);
MalType* mal_vector_to_list(MalType *val);
MalType* mal_vector_range(MalType *vec, int start, int stop_exclusive);

MalType* mal_car2(MalType *val);
MalType* mal_cdr2(MalType *val);

MalType* mal_hashmap();
#define is_hashmap(val) ((val)->type == MAL_HASHMAP_TYPE)
MalType* mal_hashmap_get(MalType *map, MalType *key);
void mal_hashmap_put(MalType *map, MalType *key, MalType *val);
void mal_hashmap_remove(MalType *map, MalType *key);
size_t mal_hashmap_size(MalType *map);
MalType* mal_hashmap_keys_to_vector(MalType *map);

MalType* mal_string(char *str);
#define is_string(val) ((val)->type == MAL_STRING_TYPE)
void mal_grow_string(MalType *val, size_t capacity);
void mal_grow_string_at_least(MalType *val, size_t min_capacity);
void mal_string_append(MalType *val, char *str);
void mal_string_append_mal_string(MalType *val, MalType *str);
void mal_string_append_char(MalType *val, char c);
void mal_string_append_long_long(MalType *val, long long n);
MalType* mal_string_replace(MalType *val, char *find, char *replace);
MalType* mal_string_replace_all(MalType *orig, char *find, char *replace);
MalType* mal_string_to_list(MalType *orig);

MalType* mal_keyword(char *name);
#define is_keyword(val) ((val)->type == MAL_KEYWORD_TYPE)

MalType* mal_number(long long number);
#define is_number(val) ((val)->type == MAL_NUMBER_TYPE)

MalType* mal_symbol(char *name);
#define is_symbol(val) ((val)->type == MAL_SYMBOL_TYPE)

MalType* mal_closure(MalType* (*lambda)(MalEnv *env, size_t argc, MalType **args), MalEnv *env);
MalType* mal_builtin_function(MalType* (*lambda)(MalEnv *env, size_t argc, MalType **args), char *function_name, MalEnv *env);
MalType* mal_continuation(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env, size_t argc, MalType **args);
MalType* mal_continuation_0(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env);
MalType* mal_continuation_1(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env, MalType *arg);
#define is_lambda(val) ((val)->type == MAL_LAMBDA_TYPE)
#define is_macro(val) ((val)->is_macro)
#define is_builtin_function(val) ((val)->type == MAL_LAMBDA_TYPE && (val)->function_name)

MalType* mal_atom(MalType *inner_val);
#define is_atom(val) ((val)->type == MAL_ATOM_TYPE)

MalType* mal_blank_line();
#define is_blank_line(val) ((val)->type == MAL_BLANK_LINE_TYPE)

MalType* mal_error();
#define is_error(val) ((val)->type == MAL_ERROR_TYPE)
#define bubble_if_error(val) ({ MalType *v = (val); if (is_error(v)) { return (v); }; v; })

struct list_or_vector_iter {
  enum { LIST_ITER, VECTOR_ITER } type;
  MalType *cell;
  size_t len;
  size_t i;
};

size_t list_or_vector_len(MalType *obj);
struct list_or_vector_iter* list_or_vector_iter(MalType *obj);
struct list_or_vector_iter* list_or_vector_iter_next(struct list_or_vector_iter *iter);
int list_or_vector_iter_is_last(struct list_or_vector_iter *iter);
MalType* list_or_vector_iter_get_obj(struct list_or_vector_iter *iter);
int list_or_vector_index_of(MalType *list, MalType *val);

MalType* mal_sprintf(char *format, ...);

#define is_list_like(obj) ((obj)->type == MAL_EMPTY_TYPE || (obj)->type == MAL_CONS_TYPE || (obj)->type == MAL_VECTOR_TYPE)
#define is_pair(obj) ((obj)->type == MAL_CONS_TYPE || ((obj)->type == MAL_VECTOR_TYPE && (obj)->vec_len > 0))

int is_equal(MalType *arg1, MalType *arg2);
int hashmap_is_equal(MalType *arg1, MalType *arg2);
int list_or_vector_is_equal(MalType *arg1, MalType *arg2);

struct codegen {
  MalType *top;
  MalType *decl;
  MalType *body;
};

MalType* trampoline(MalType *result);

#endif
