#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>

#include <editline/readline.h>
#include <editline/history.h>
#include <gc.h>

/* only needed for ffi */
#ifdef WITH_FFI
#include <dlfcn.h>
#include <ffi.h>
#endif

#include "libs/hashmap/hashmap.h"
#include "core.h"
#include "printer.h"
#include "reader.h"

/* forward references to main file */
MalType apply(MalType fn, list args);

// Helper functions

bool equal_lists(list, list);
bool equal_hashmaps(list, list);
bool equal_forms(MalType, MalType);
MalType make_boolean(bool);

/* core ns functions */
MalType mal_add(list);
MalType mal_sub(list);
MalType mal_mul(list);
MalType mal_div(list);

MalType mal_prn(list);
MalType mal_println(list);
MalType mal_pr_str(list);
MalType mal_str(list);
MalType mal_read_string(list);
MalType mal_slurp(list);

MalType mal_list_questionmark(list);
MalType mal_empty_questionmark(list);
MalType mal_count(list);
MalType mal_cons(list);
MalType mal_concat(list);
MalType mal_nth(list);
MalType mal_first(list);
MalType mal_rest(list);

MalType mal_equals(list);
MalType mal_lessthan(list);
MalType mal_lessthanorequalto(list);
MalType mal_greaterthan(list);
MalType mal_greaterthanorequalto(list);

MalType mal_atom(list);
MalType mal_atom_questionmark(list);
MalType mal_deref(list);
MalType mal_reset_bang(list);
MalType mal_swap_bang(list);

MalType mal_throw(list);
MalType mal_apply(list);
MalType mal_map(list);

MalType mal_nil_questionmark(list);
MalType mal_true_questionmark(list);
MalType mal_false_questionmark(list);
MalType mal_symbol_questionmark(list);
MalType mal_keyword_questionmark(list);
MalType mal_symbol(list);
MalType mal_keyword(list);

MalType mal_vec(list);
MalType mal_vector_questionmark(list);
MalType mal_sequential_questionmark(list);
MalType mal_map_questionmark(list);
MalType mal_assoc(list);
MalType mal_dissoc(list);
MalType mal_get(list);
MalType mal_contains_questionmark(list);
MalType mal_keys(list);
MalType mal_vals(list);
MalType mal_string_questionmark(list);
MalType mal_number_questionmark(list);
MalType mal_fn_questionmark(list);
MalType mal_macro_questionmark(list);

MalType mal_time_ms(list);
MalType mal_conj(list);
MalType mal_seq(list);
MalType mal_meta(list);
MalType mal_with_meta(list);

MalType mal_readline(list);

/* only needed for ffi */
#ifdef WITH_FFI
MalType mal_dot(list);
#endif

struct ns_s THE_CORE_NS[] = {

  /* arithmetic */
  { "+", mal_add },
  { "-", mal_sub },
  { "*", mal_mul },
  { "/", mal_div },

  /* strings */
  { "prn", mal_prn },
  { "pr-str", mal_pr_str },
  { "str", mal_str },
  { "println", mal_println },
  { "read-string", mal_read_string },

  /* files */
  { "slurp", mal_slurp },

  /* lists */
  { "list", make_list },
  { "empty?", mal_empty_questionmark },
  { "count", mal_count },
  { "cons", mal_cons },
  { "concat", mal_concat },
  { "nth", mal_nth },
  { "first", mal_first },
  { "rest", mal_rest },

  /* predicates */
  { "=", mal_equals },
  { "<", mal_lessthan },
  { "<=", mal_lessthanorequalto },
  { ">", mal_greaterthan },
  { ">=", mal_greaterthanorequalto },

  { "list?", mal_list_questionmark },
  { "nil?", mal_nil_questionmark },
  { "true?", mal_true_questionmark },
  { "false?", mal_false_questionmark },
  { "symbol?", mal_symbol_questionmark },
  { "keyword?", mal_keyword_questionmark },
  { "vector?", mal_vector_questionmark },
  { "sequential?", mal_sequential_questionmark },
  { "map?", mal_map_questionmark },
  { "string?", mal_string_questionmark },
  { "number?", mal_number_questionmark },
  { "fn?", mal_fn_questionmark },
  { "macro?", mal_macro_questionmark },

  /* atoms */
  { "atom", mal_atom },
  { "atom?", mal_atom_questionmark },
  { "deref", mal_deref },
  { "reset!", mal_reset_bang },
  { "swap!", mal_swap_bang },

  /* other */
  { "throw", mal_throw },
  { "apply", mal_apply },
  { "map", mal_map },

  { "symbol", mal_symbol },
  { "keyword", mal_keyword },
  { "vec", mal_vec },
  { "vector", make_vector },
  { "hash-map", mal_hash_map },

  /* hash-maps */
  { "contains?", mal_contains_questionmark },
  { "assoc", mal_assoc },
  { "dissoc", mal_dissoc },
  { "get", mal_get },
  { "keys", mal_keys },
  { "vals", mal_vals },

  /* misc */
  { "time-ms", mal_time_ms },
  { "conj", mal_conj },
  { "seq", mal_seq },
  { "meta", mal_meta },
  { "with-meta", mal_with_meta },

  { "readline", mal_readline },

  /* only needed for ffi */
  #ifdef WITH_FFI
  { ".", mal_dot },
  #endif
};

void ns_make_core(ns* core, size_t* size) {
  *core = THE_CORE_NS;
  *size = sizeof(THE_CORE_NS) / sizeof(struct ns_s);
}

/* core function definitons */

#define generic_arithmetic(name, op, iconst, fconst)                   \
  MalType name(list args) {                                            \
    MalType a1 = eat_argument(args);                                   \
    MalType a2 = eat_argument(args);                                   \
    check_empty(args);                                                 \
    switch(a1->type) {                                                 \
    case MALTYPE_INTEGER:                                              \
      switch(a2->type) {                                               \
      case MALTYPE_INTEGER:                                            \
        return iconst(a1->value.mal_integer op a2->value.mal_integer); \
      case MALTYPE_FLOAT:                                              \
        return fconst(a1->value.mal_integer op a2->value.mal_float);   \
      default: return bad_type(a2, "a number");                        \
      }                                                                \
    case MALTYPE_FLOAT:                                                \
      switch(a2->type) {                                               \
      case MALTYPE_INTEGER:                                            \
        return fconst(a1->value.mal_float   op a2->value.mal_integer); \
      case MALTYPE_FLOAT:                                              \
        return fconst(a1->value.mal_float   op a2->value.mal_float);   \
      default: return bad_type(a2, "a number");                        \
      }                                                                \
    default: return bad_type(a1, "a number");                          \
    }                                                                  \
  }
generic_arithmetic(mal_add,                  +,  make_integer, make_float)
generic_arithmetic(mal_sub,                  -,  make_integer, make_float)
generic_arithmetic(mal_mul,                  *,  make_integer, make_float)
generic_arithmetic(mal_div,                  /,  make_integer, make_float)
generic_arithmetic(mal_lessthan,             <,  make_boolean, make_boolean)
generic_arithmetic(mal_lessthanorequalto,    <=, make_boolean, make_boolean)
generic_arithmetic(mal_greaterthan,          >,  make_boolean, make_boolean)
generic_arithmetic(mal_greaterthanorequalto, >=, make_boolean, make_boolean)

#define generic_type_predicate(name, mask)   \
  MalType name(list args) {                  \
    MalType val = eat_argument(args);        \
    check_empty(args);                       \
    return make_boolean(val->type & (mask)); \
  }
generic_type_predicate(mal_list_questionmark, MALTYPE_LIST)
generic_type_predicate(mal_atom_questionmark, MALTYPE_ATOM)
generic_type_predicate(mal_nil_questionmark, MALTYPE_NIL)
generic_type_predicate(mal_true_questionmark, MALTYPE_TRUE)
generic_type_predicate(mal_false_questionmark, MALTYPE_FALSE)
generic_type_predicate(mal_symbol_questionmark, MALTYPE_SYMBOL)
generic_type_predicate(mal_keyword_questionmark, MALTYPE_KEYWORD)
generic_type_predicate(mal_vector_questionmark, MALTYPE_VECTOR)
generic_type_predicate(mal_sequential_questionmark, MALTYPE_LIST | MALTYPE_VECTOR)
generic_type_predicate(mal_map_questionmark, MALTYPE_HASHMAP)
generic_type_predicate(mal_string_questionmark, MALTYPE_STRING)
generic_type_predicate(mal_number_questionmark, MALTYPE_FLOAT | MALTYPE_INTEGER)
generic_type_predicate(mal_fn_questionmark, MALTYPE_CLOSURE | MALTYPE_FUNCTION)
generic_type_predicate(mal_macro_questionmark, MALTYPE_MACRO)

MalType mal_equals(list args) {
  /* Accepts any type of arguments */

  MalType first_val = eat_argument(args);
  MalType second_val = eat_argument(args);
  check_empty(args);
  return make_boolean(equal_forms(first_val, second_val));
}

bool equal_forms(MalType first_val, MalType second_val) {

    switch(first_val->type) {

    case MALTYPE_LIST:
    case MALTYPE_VECTOR:

      return (second_val->type & (MALTYPE_LIST | MALTYPE_VECTOR))
        && equal_lists(first_val->value.mal_list, second_val->value.mal_list);

    case MALTYPE_INTEGER:

      return (second_val->type == MALTYPE_INTEGER)
        && (first_val->value.mal_integer == second_val->value.mal_integer);

    case MALTYPE_FLOAT:

      return (second_val->type == MALTYPE_FLOAT)
        && (first_val->value.mal_float == second_val->value.mal_float);

    case MALTYPE_SYMBOL:
    case MALTYPE_STRING:
    case MALTYPE_KEYWORD:
      return (first_val->type == second_val->type)
        && !strcmp(first_val->value.mal_string, second_val->value.mal_string);

    case MALTYPE_HASHMAP:
      return (second_val->type == MALTYPE_HASHMAP)
        && equal_hashmaps(first_val->value.mal_list, second_val->value.mal_list);

    case MALTYPE_TRUE:
    case MALTYPE_FALSE:
    case MALTYPE_NIL:

      return first_val == second_val;

    default:

      return false;
    }
}

MalType mal_nth(list args) {

  MalType lst = eat_argument(args);
  MalType n = eat_argument(args);
  check_empty(args);

  list l = as_sequence(lst);
  long idx = as_integer(n);
  if(0 <= idx) {
    while(l) {
      if(!idx)
        return l->data;
      l = l->next;
      idx--;
    }
  }
  return make_error_fmt("index %M out of bounds for: %M", n, lst);
}

MalType mal_first(list args) {

  MalType lst = eat_argument(args);
  check_empty(args);

  if(is_nil(lst)) {
    return make_nil();
  }
  else if(!is_sequential(lst)) {
    return bad_type(lst, "a list, vector or nil");
  }

  list result = lst->value.mal_list;

  if (result) {
    return result->data;
  }
  else {
    return make_nil();
  }
}

MalType mal_rest(list args) {

  MalType lst = eat_argument(args);
  check_empty(args);

  if(is_nil(lst)) {
    return make_list(NULL);
  }
  else if(!is_sequential(lst)) {
    return bad_type(lst, "a list, vector or nil");
  }

  list result = lst->value.mal_list;
  if (result) {
    result = result->next;
  }
  return make_list(result);
}


MalType mal_cons(list args) {

  MalType a1 = eat_argument(args);
  MalType lst = eat_argument(args);
  check_empty(args);

  if (is_sequential(lst)) {
    return make_list(list_push(lst->value.mal_list, a1));
  }
  else if (is_nil(lst)) {
    return make_list(list_push(NULL, a1));
  }
  else {
    return bad_type(lst, "a list, vector or nil");
  }
}

MalType mal_concat(list args) {

  //  Could reuse the last if it is not nil...

  list new_list = NULL;
  list* new_list_last = &new_list;
  while (args) {

    MalType val = args->data;

    /* skip nils */
    if (is_nil(val)) {
    }
    /* concatenate lists and vectors */
    else if (is_sequential(val)) {
      for(list lst=val->value.mal_list; lst; lst=lst->next) {
        *new_list_last = list_push(NULL, lst->data);
        new_list_last = &(*new_list_last)->next;
      }
    }
    /* raise an error for any non-sequence types */
    else {
      return bad_type(val, "a list, vector or nil");
    }
    args = args->next;
  }
  return make_list(new_list);
}

MalType mal_count(list args) {

  MalType val = eat_argument(args);
  check_empty(args);

  if(is_nil(val)) {
    return make_integer(0);
  }
  else if (!is_sequential(val)) {
    return bad_type(val, "a list, vector or nil");
  }
  return make_integer(list_count(val->value.mal_list));
}

MalType mal_empty_questionmark(list args) {

  MalType val = eat_argument(args);
  check_empty(args);

  return make_boolean(!as_sequence(val));
}

MalType mal_pr_str(list args) {
  /* Accepts any number and type of arguments */
  return make_string(mal_printf("% N", args));
}

MalType mal_str(list args) {
  /* Accepts any number and type of arguments */
  return make_string(mal_printf("%#N", args));
}

MalType mal_prn(list args) {
  /* Accepts any number and type of arguments */
  printf("% N\n", args);
  return make_nil();
}

MalType mal_println(list args) {
  /* Accepts any number and type of arguments */
  printf("% #N\n", args);
  return make_nil();
}

MalType mal_read_string(list args) {

  MalType val = eat_argument(args);
  check_empty(args);
  return read_str(as_string(val));
}

MalType mal_slurp(list args) {

  MalType a1 = eat_argument(args);
  check_empty(args);

  const char* filename = as_string(a1);

  FILE* file = fopen(filename, "rb");

  if (!file){
    return make_error_fmt("file not found '%s'", filename);
  }

  fseek(file, 0, SEEK_END);
  size_t file_length = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buffer = (char*)GC_MALLOC(sizeof(*buffer) * file_length + 1);
  if (file_length != fread(buffer, sizeof(*buffer), file_length, file)) {
    return make_error_fmt("failed to read file '%s'", filename);
  }

  fclose(file);

  buffer[file_length] = '\0';
  return make_string(buffer);
}

MalType mal_atom(list args) {
  MalType val = eat_argument(args);
  check_empty(args);
  return make_atom(val);
}
MalType mal_deref(list args) {
  MalType val = eat_argument(args);
  check_empty(args);
  return *as_atom(val);
}
MalType mal_reset_bang(list args) {
  MalType a1 = eat_argument(args);
  MalType a2 = eat_argument(args);
  check_empty(args);
  *as_atom(a1) = a2;
  return a2;
}
MalType mal_swap_bang(list args) {
  MalType a1 = eat_argument(args);
  MalType fn = eat_argument(args);
  MalType* atm = as_atom(a1);
  list fn_args = list_push(args, *atm);
  MalType result = apply(fn, fn_args);

  if (is_error(result)) {
    return result;
  }
  else {
    *atm = result;
    return result;
  }
}

MalType mal_throw(list args) {
  MalType val = eat_argument(args);
  check_empty(args);

  /* re-throw an existing exception */
  assert(!is_error(val));
  /* create a new exception */
    return wrap_error(val);
}

MalType mal_apply(list args) {

  MalType func = eat_argument(args);
  if(!args) {
    return make_error_fmt("expected at least two arguments");
  }

  /* assemble loose arguments */
  list lst = NULL;
  list* lst_last = &lst;
  while(args->next) {
    *lst_last = list_push(NULL, args->data);
    lst_last = &(*lst_last)->next;
    args = args->next;
  }

  MalType final = args->data;

  *lst_last = as_sequence(final);

  return apply(func, lst);
}

MalType mal_map(list args) {

  MalType func = eat_argument(args);
  MalType arg = eat_argument(args);
  check_empty(args);

  if (!is_callable(func)) {
    //  This check is not redundant when arg is empty.
    return bad_type(func, "a closure, function or macro");
  }

  list arg_list = as_sequence(arg);
  list result_list = NULL;
  list* result_list_last = &result_list;

  while(arg_list) {

    MalType result = apply(func, list_push(NULL, arg_list->data));

    /* early return if error */
    if (is_error(result)) {
      return result;
    }
    else {
      *result_list_last = list_push(NULL, result);
      result_list_last = &(*result_list_last)->next;

    }
    arg_list = arg_list->next;
  }
  return make_list(result_list);
}

MalType mal_symbol(list args) {
  MalType val = eat_argument(args);
  check_empty(args);

  return make_symbol(as_string(val));
}

MalType mal_keyword(list args) {

  MalType val = eat_argument(args);
  check_empty(args);

  if (!is_string(val) && !is_keyword(val)) {
    return bad_type(val, "a keyword or string");
  }
  else {
    return make_keyword(val->value.mal_string);
  }
}

MalType mal_vec(list args) {

  /* Accepts a single argument */

  MalType val = eat_argument(args);
  check_empty(args);

  if(!is_vector(val) && !is_list(val)) {
    return bad_type(val, "a list or vector");
  }

  MalType new_val = make_vector(val->value.mal_list);

  return new_val;
}

MalType mal_get(list args) {
  /* TODO: implement a proper hashmap */

  MalType map = eat_argument(args);
  MalType key = eat_argument(args);
  check_empty(args);

  check_type(key, MALTYPE_KEYWORD | MALTYPE_STRING, "a keyword or string");

  if(is_nil(map)) {
    return make_nil();
  }
  else if(!is_hashmap(map)) {
    return bad_type(map, "a map or nil");
  }

  MalType result = hashmap_get(map->value.mal_list, key);

  if (!result) {
    return make_nil();
  }

  return result;
}

MalType mal_contains_questionmark(list args) {

  MalType map = eat_argument(args);
  MalType key = eat_argument(args);
  check_empty(args);

  check_type(key, MALTYPE_KEYWORD | MALTYPE_STRING, "a keyword or string");

  if(is_nil(map)) {
    return make_nil();
  }
  if (!is_hashmap(map)) {
    return bad_type(map, "a map or nil");
  }

  MalType result = hashmap_get(map->value.mal_list, key);

  if (!result) {
    return make_false();
  }
  else {
    return make_true();
  }
}

MalType mal_assoc(list args) {
  MalType map = eat_argument(args);
  return map_assoc(as_map(map), args);
}

MalType mal_dissoc(list args) {

  MalType map = eat_argument(args);
  for(list p=args; p; p=p->next) {
    check_type(p->data, MALTYPE_KEYWORD | MALTYPE_STRING,
               "a keyword or string");
  }

  list source_list = as_map(map);
  list new_list = NULL;

  while(source_list) {

    list dis_args = args;
    long dis = 0;


    while(dis_args) {

      if(equal_forms(source_list->data, dis_args->data)) {
        dis = 1;
        break;
      }
      dis_args = dis_args->next;
    }

    if (!dis) {
      new_list = list_push(new_list, source_list->next->data);
      new_list = list_push(new_list, source_list->data);
    }
    source_list = source_list->next->next;
  }

  return make_hashmap(new_list);
}


 MalType mal_keys(list args) {

  MalType map = eat_argument(args);
  check_empty(args);

  list lst = as_map(map);

  list result = NULL;
  while(lst) {

    result = list_push(result, lst->data);
    lst = lst->next->next;
  }
  return make_list(result);
}

MalType mal_vals(list args) {

  MalType map = eat_argument(args);
  check_empty(args);

  list lst = as_map(map);

  list result = NULL;
  while(lst) {

    result = list_push(result, lst->next->data);
    lst=lst->next->next;
  }
  return make_list(result);
}

MalType mal_time_ms(list args) {
  check_empty(args);

  struct timeval tv;
  gettimeofday(&tv, NULL);
  long ms = tv.tv_sec * 1000 + tv.tv_usec/1000.0 + 0.5;

  return make_float(ms);
}


MalType mal_conj(list args) {

  MalType lst = eat_argument(args);

  list rest = args;

  if (is_list(lst)) {

    list new_lst = lst->value.mal_list;

    while(rest) {
      new_lst = list_push(new_lst, rest->data);
      rest = rest->next;
    }
    return make_list(new_lst);
  }
  else if(is_vector(lst)) {

    list new_lst = NULL;
    list* new_lst_last = &new_lst;
    for(list v=lst->value.mal_list; v; v=v->next) {
      *new_lst_last = list_push(NULL, v->data);
      new_lst_last = &(*new_lst_last)->next;
    }
    *new_lst_last = rest;
    return make_vector(new_lst);
  }
  else {
    return bad_type(lst, "a list or vector");
  }
}

MalType mal_seq(list args) {

  MalType val = eat_argument(args);
  check_empty(args);

  if (is_sequential(val)) {

    /* empty list or vector */
    if (!val->value.mal_list) {
      return make_nil();
    }
    else {
      return make_list(val->value.mal_list);
    }
  }
  else if (is_string(val)) {

    const char* ch = val->value.mal_string;

    /* empty string */
    if (*ch == '\0') {
      return make_nil();
    }
    else {

      list lst = NULL;
      list* lst_last = &lst;

      while(*ch != '\0') {
        char* new_ch = GC_MALLOC(2);
        new_ch[0] = *ch;
        new_ch[1] = 0;

        *lst_last = list_push(NULL, make_string(new_ch));
        lst_last = &(*lst_last)->next;
        ch++;
      }
      return make_list(lst);
    }
  }
  else if (is_nil(val)) {
    return make_nil();
  }
  else {
    return bad_type(val, "nil, a list, vector or string");
  }
}

MalType mal_meta(list args) {

  MalType val = eat_argument(args);
  check_empty(args);

  if (!is_sequential(val) && !is_hashmap(val) && !is_callable(val)) {
    return bad_type(val, "a sequence, map or callable");
  }
    return val->metadata;
}

MalType mal_with_meta(list args) {

  MalType val = eat_argument(args);
  MalType metadata = eat_argument(args);
  check_empty(args);

  if (!is_sequential(val) && !is_hashmap(val) && !is_callable(val)) {
    return bad_type(val, "a sequence, map or callable");
  }

  MalType new_val = with_meta(val, metadata);

  return new_val;
}

MalType mal_readline(list args) {
  MalType a1 = eat_argument(args);
  check_empty(args);

  char* str = readline(as_string(a1));
  if(!str)
    return make_nil();
  add_history(str);
  /* Copy the input into an area managed by libgc. */
  size_t n = strlen(str) + 1;
  char* result = GC_MALLOC(n);
  memcpy(result, str, n);
  free(str);
  return make_string(result);
}


/* helper functions */

inline MalType make_boolean(bool x) {
  return x ? make_true() : make_false();
}

bool equal_lists(list first, list second) {

  while(first != NULL) {
    if(second == NULL || !equal_forms(first->data, second->data))
      return false;
    first = first->next;
    second = second->next;
  }
  return second == NULL;
}

bool equal_hashmaps(list first, list second) {

  //  Check that the lists have the same count.
  if(list_count(first) != list_count(second))
    return false;
  while(first) {
    MalType val = hashmap_get(second, first->data);
    first = first->next;
    if(!val || !equal_forms(first->data, val)) {
      return false;
    }
    first = first->next;
  }
  return true;
}


#ifdef WITH_FFI
MalType mal_dot(list args) {

  /* (. "lib" "return type" "function" "arg1 type" "arg 1" ...) */

  MalType lib_name = eat_argument(args);

  const char* lib_name_str = NULL;
  if (is_string(lib_name)) {
    lib_name_str = lib_name->value.mal_string;
  }
  else if (!is_nil(lib_name)) {
    return bad_type(lib_name, "a string or nil");
  }

  MalType return_type = eat_argument(args);

  const char* return_type_str = as_string(return_type);

  MalType fn_name = eat_argument(args);

  const char* fn_name_str = as_string(fn_name);

  int args_count = list_count(args);

  if (args_count % 2 == 1) {
    return make_error_fmt("expected even number of argument types and values");
  }

  list arg_types_list = NULL;
  list* arg_types_list_last = &arg_types_list;
  list arg_vals_list = NULL;
  list* arg_vals_list_last = &arg_vals_list;

  while(args) {

    MalType val_type = (MalType)args->data;
    args = args->next;
    MalType val = eat_argument(args);  // check that the argument is present

    as_string(val_type);        // Just for the type check.

    *arg_types_list_last = list_push(NULL, val_type);
    arg_types_list_last = &(*arg_types_list_last)->next;
    *arg_vals_list_last = list_push(NULL, val);
    arg_vals_list_last = &(*arg_vals_list_last)->next;
  }

  /* open a shared library dynamically and get hold of a function */
  void* lib_handle = dlopen(lib_name_str, RTLD_LAZY);

  if (!lib_handle) {
    return make_error_fmt("%s", dlerror());
  }

  void* fn = dlsym(lib_handle, fn_name_str);

  char* error;
  if ((error = dlerror()) != NULL) {
    return make_error_fmt("dlsym could not get handle to function '%s': %s",
          fn_name_str, error);
  }

  /* use libffi to call function */

  ffi_cif cif;
  ffi_type* ret_type;
  ffi_type* arg_types[20];
  void* arg_vals[20];
  ffi_status status;
  ffi_type* ffi_get_type(const char *type, MalType* err);

  MalType mal_err = make_nil();

  /* set return type */
  MalType make_type(const char *type);
  MalType immutable_retval = make_type(return_type_str);
  struct MalType_s* retval = GC_MALLOC(sizeof(*retval));
  *retval = *immutable_retval;;

  ret_type = ffi_get_type(return_type_str, &mal_err);
  if(is_error(mal_err)) { return mal_err; }

  int arg_count = list_count(arg_types_list);

  /* Set the argument types and values */
  for (int i = 0; i < arg_count; i++) {

    MalType val_type = (MalType)arg_types_list->data;
    arg_types[i] = ffi_get_type(as_string(val_type), &mal_err);
    if (is_error(mal_err)) { return mal_err; }

    MalType val = (MalType)arg_vals_list->data;
    arg_vals[i] = (void*)(&(val->value));

    arg_types_list = arg_types_list->next;
    arg_vals_list = arg_vals_list->next;
  }

  /* perform the call */
  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_count, ret_type, arg_types);

  if (status != FFI_OK) {
    return make_error_fmt("call to ffi_prep_cif failed with code: %d", status);
  }

  ffi_call(&cif, FFI_FN(fn), &retval->value, arg_vals);

  /* close the library */
  dlclose(lib_handle);

  if (ret_type == &ffi_type_void) {
    return make_nil();
  } else {
    return retval;
  }
}

/* helper function for ffi */
ffi_type* ffi_get_type(const char *type, MalType* err) {

  if ((strcmp("void", type) == 0)) {

    return &ffi_type_void;
  }
  else if ((strcmp("string", type) == 0) ||
           (strcmp("char*", type) == 0) ||
           (strcmp("char *", type) == 0)) {

    return &ffi_type_pointer;
  }
  else if ((strcmp("integer", type) == 0) ||
           (strcmp("int64", type) == 0)) {

    return &ffi_type_sint64;
  }
  else if ((strcmp("int32", type) == 0)) {

    return &ffi_type_sint32;
  }
  else if (strcmp("double", type) == 0) {

    return &ffi_type_double;
  }
  else if (strcmp("float", type) == 0) {
    return &ffi_type_float;
  }
  else {
    *err = make_error_fmt("type not recognised '%s'", type);
    return NULL;
  }
}

/* helper function for ffi */
MalType make_type(const char *type) {

  if ((strcmp("void", type) == 0)) {

    return make_nil();
  }
  else if ((strcmp("string", type) == 0) ||
           (strcmp("char*", type) == 0) ||
           (strcmp("char *", type) == 0)) {

    return make_string("");
  }
  else if ((strcmp("integer", type) == 0) ||
           (strcmp("int64", type) == 0)) {

    return make_integer(0);
  }
  else if ((strcmp("int32", type) == 0)) {
    return make_integer(0);
  }
  else if (strcmp("double", type) == 0) {

    return make_float(0);
  }
  else if (strcmp("float", type) == 0) {

    return make_float(0);
  }
  else {
    return make_error_fmt("type not supported '%s'", type);
  }
}
#endif
