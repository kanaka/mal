#ifndef _MAL_TYPES_H
#define _MAL_TYPES_H

#include <stdbool.h>
#include <stddef.h>

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
  MALTYPE_ERROR    = 1 << 13,
  MALTYPE_ATOM     = 1 << 14,
  MALTYPE_MACRO    = 1 << 15,
};

typedef const struct MalType_s* MalType;
typedef const struct MalClosure_s* MalClosure;
typedef struct pair_s* list;
typedef MalType(*function_t)(list);
typedef struct Env_s Env;

union MalType_u {

    long mal_integer;
    double mal_float;
    const char* mal_string;
    list mal_list;
    /* vector mal_vector;  TODO: implement a real vector */
    /* hashmap mal_hashmap; TODO: implement a real hashmap */
    function_t mal_function;
    MalClosure mal_closure;
    MalType* mal_atom;
    MalType mal_error;

};
struct MalType_s {
  enum mal_type_t type;
  MalType metadata;
  union MalType_u value;
};

struct MalClosure_s {

  const Env* env;
  size_t param_len;
  const char** parameters;
  const char* more_symbol;
  MalType definition;

};

MalType make_symbol(const char* value);
MalType make_integer(long value);
MalType make_float(double value);
MalType make_keyword(const char* value);
MalType make_string(const char* value);
MalType make_list(list value);
MalType make_vector(list value);
MalType make_hashmap(list value);
MalType make_true();
MalType make_false();
MalType make_nil();
MalType make_atom(MalType value);
#define make_error_fmt(fmt, ...)                                        \
  wrap_error(make_string(mal_printf("%s: " fmt, __func__, ## __VA_ARGS__)));
MalType wrap_error(MalType value);;
MalType make_function(function_t value);
MalType make_closure(const Env* env, size_t param_len,
                     const char** parameters,
                     MalType definition, const char* more_symbol);
MalType make_macro(MalClosure value);
MalType with_meta(MalType data, MalType meta);

int is_sequential(MalType val);
int is_list(MalType val);
int is_vector(MalType val);
int is_hashmap(MalType val);
int is_nil(MalType val);
int is_string(MalType val);
int is_false(MalType val);
int is_symbol(MalType val);
int is_keyword(MalType val);
int is_error(MalType val);
int is_callable(MalType val);
int is_function(MalType val);
int is_closure(MalType val);
int is_macro(MalType val);


//  Return an exception if the list is empty.
//  Else, return the first element and move it forward.
//  Mutates the argument.
#define eat_argument(args)                        \
  ({                                              \
    if(!(args))                                   \
      return make_error_fmt("too_few_arguments"); \
    MalType _tmp = args->data;                    \
    args = args->next;                            \
    _tmp;                                         \
  })

#define check_empty(lst)                                            \
  if(lst)                                                           \
    return make_error_fmt("unexpected trailing arguments: % N", lst);

#define bad_type(form, expected)                                        \
  make_error_fmt("expected %s, got: %M", expected, form);

#define check_type(form, mask, expected)                   \
  if(form->type & ~(mask)) return bad_type(form, expected)

//  Beware that this evaluates the argument twice.
#define as_string(f)                                                 \
  ({ check_type(f, MALTYPE_STRING,  "a string");   f->value.mal_string;  })
#define as_symbol(f)                                                  \
  ({ check_type(f, MALTYPE_SYMBOL,  "a symbol");   f->value.mal_string;  })
#define as_list(f)                                                    \
  ({ check_type(f, MALTYPE_LIST,    "a list");     f->value.mal_list;    })
#define as_map(f)                                                     \
  ({ check_type(f, MALTYPE_HASHMAP, "a map");      f->value.mal_list   ; })
#define as_closure(f)                                                 \
  ({ check_type(f, MALTYPE_CLOSURE, "a closure");  f->value.mal_closure; })
#define as_integer(f)                                                 \
  ({ check_type(f, MALTYPE_INTEGER, "an integer"); f->value.mal_integer; })
#define as_atom(f)                                                    \
  ({ check_type(f, MALTYPE_ATOM,    "an atom");    f->value.mal_atom;    })
#define as_sequence(f)                                                \
  ({ check_type(f, MALTYPE_LIST | MALTYPE_VECTOR, "a sequence");      \
    f->value.mal_list; })

#endif
