#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <gc.h>

/* only needed for ffi */
#ifdef WITH_FFI
#include <dlfcn.h>
#include <ffi.h>
#endif

#include "hashmap.h"
#include "core.h"
#include "printer.h"
#include "reader.h"
#include "error.h"
#include "linked_list.h"
#include "readline.h"
#include "vector.h"

/* forward references to main file */
MalType apply(MalType fn, list args);

// Helper functions

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
MalType mal_vector(list);
MalType mal_vector_questionmark(list);
MalType mal_sequential_questionmark(list);
MalType mal_hash_map(list);
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
  { "vector", mal_vector },
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
    explode2(#op, args, a1, a2);                                       \
    long i1, i2;                                                       \
    double f1, f2;                                                     \
    if (is_integer(a1, &i1)) {                                         \
      if (is_integer(a2, &i2)) return iconst(i1 op i2);                \
      if (is_float  (a2, &f2)) return fconst(i1 op f2);                \
      bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a2);              \
    }                                                                  \
    if (is_float(a1, &f1)) {                                           \
      if (is_integer(a2, &i2)) return iconst(f1 op i2);                \
      if (is_float  (a2, &f2)) return fconst(f1 op f2);                \
      bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a2);              \
    }                                                                  \
    bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a1);                \
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
  MalType mal_##name##_questionmark(list args) { \
    explode1(#name "?", args, val);          \
    return make_boolean(type(val) & (mask)); \
  }
generic_type_predicate(list, MALTYPE_LIST)
generic_type_predicate(atom, MALTYPE_ATOM)
generic_type_predicate(nil, MALTYPE_NIL)
generic_type_predicate(true, MALTYPE_TRUE)
generic_type_predicate(false, MALTYPE_FALSE)
generic_type_predicate(symbol, MALTYPE_SYMBOL)
generic_type_predicate(keyword, MALTYPE_KEYWORD)
generic_type_predicate(vector, MALTYPE_VECTOR)
generic_type_predicate(sequential, MALTYPE_LIST | MALTYPE_VECTOR)
generic_type_predicate(map, MALTYPE_HASHMAP)
generic_type_predicate(string, MALTYPE_STRING)
generic_type_predicate(number, MALTYPE_FLOAT | MALTYPE_INTEGER)
generic_type_predicate(fn, MALTYPE_CLOSURE | MALTYPE_FUNCTION)
generic_type_predicate(macro, MALTYPE_MACRO)

MalType mal_equals(list args) {
  /* Accepts any type of arguments */

  explode2("=", args, first_val, second_val);
  return make_boolean(equal_forms(first_val, second_val));
}

MalType mal_nth(list args) {

  explode2("nth", args, lst, n);

  vector_t v;
  list l;
  long idx;
  if (!is_integer(n, &idx)) {
    bad_type("nth", MALTYPE_INTEGER, n);
  }
  if(idx < 0) {
    make_error("'nth': negative index: %d", idx);
  }
  if (is_list(lst, &l)) {
    while(l) {
      if(!idx)
        return l->data;
      l = l->next;
      idx--;
    }
  }
  else if ((v = is_vector(lst))) {
    if ((size_t)idx < v->count) {
      return v->nth[idx];
    }
  } else {
    bad_type("nth", MALTYPE_LIST | MALTYPE_VECTOR, lst);
  }
  make_error("'nth': index %M out of bounds for: %M", n, lst);
}

MalType mal_first(list args) {

  explode1("first", args, lst);

  list result;
  vector_t v;
  if(is_nil(lst)) {
    return make_nil();
  }
  else if ((v = is_vector(lst))) {
    return v->count ? v->nth[0] : make_nil();
  }
  else if (!is_list(lst, &result)) {
    bad_type("first", MALTYPE_LIST | MALTYPE_VECTOR | MALTYPE_NIL, lst);
  }

  if (result) {
    return result->data;
  }
  else {
    return make_nil();
  }
}

MalType mal_rest(list args) {

  explode1("rest", args, lst);

  list result = NULL;
  vector_t v;
  if(is_nil(lst)) {
    return make_list(NULL);
  }
  else if ((v = is_vector(lst))) {
    for (size_t i = v->count; 1 < i--; ) {
      result = list_push(result, v->nth[i]);
    }
    return make_list(result);
  }
  else if (!is_list(lst, &result)) {
    bad_type("rest", MALTYPE_LIST | MALTYPE_VECTOR | MALTYPE_NIL, lst);
  }

  if (result) {
    result = result->next;
  }
  return make_list(result);
}


MalType mal_cons(list args) {

  explode2("cons", args, a1, lst);

  list result = NULL;
  vector_t v;
  if ((v = is_vector(lst))) {
    for (size_t i = v->count; i--; ) {
      result = list_push(result, v->nth[i]);
    }
  }
  else if (is_list(lst, &result)) {
  }
  else if (is_nil(lst)) {
  }
  else {
    bad_type("cons", MALTYPE_LIST | MALTYPE_VECTOR | MALTYPE_NIL, lst);
  }
  return make_list(list_push(result, a1));
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
    else if (type(val) & (MALTYPE_LIST | MALTYPE_VECTOR)) {
      for (seq_cursor lst = seq_iter(val); seq_cont(val, lst); lst = seq_next(val, lst)) {
        *new_list_last = list_push(NULL, seq_item(val, lst));
        new_list_last = &(*new_list_last)->next;
      }
    }
    /* raise an error for any non-sequence types */
    else {
      bad_type("concat", MALTYPE_NIL | MALTYPE_LIST | MALTYPE_VECTOR, val);
    }
    args = args->next;
  }
  return make_list(new_list);
}

MalType mal_count(list args) {

  explode1("count", args, val);

  vector_t v;
  list mal_list;
  if(is_nil(val)) {
    return make_integer(0);
  }
  else if ((v = is_vector(val))) {
    return make_integer(v->count);
  }
  else if (!is_list(val, &mal_list)) {
    bad_type("count", MALTYPE_LIST | MALTYPE_NIL | MALTYPE_VECTOR, val);
  }
  return make_integer((long)list_count(mal_list));
}

MalType mal_empty_questionmark(list args) {

  explode1("empty?", args, val);

  vector_t v;
  list l;
  if ((v = is_vector(val))) {
    return make_boolean(!v->count);
  }
  else if (is_list(val, &l)) {
    return make_boolean(!l);
  }
  else {
    bad_type("empty?", MALTYPE_LIST | MALTYPE_VECTOR, val);
  }
}

MalType mal_pr_str(list args) {
  /* Accepts any number and type of arguments */
  return make_string(mal_printf("%N", args));
}

MalType mal_str(list args) {
  /* Accepts any number and type of arguments */
  return make_string(mal_printf("%# N", args));
}

MalType mal_prn(list args) {
  /* Accepts any number and type of arguments */
  printf("%N\n", args);
  return make_nil();
}

MalType mal_println(list args) {
  /* Accepts any number and type of arguments */
  printf("%#N\n", args);
  return make_nil();
}

MalType mal_read_string(list args) {

  explode1("read-string", args, val);

  const char* s = is_string(val);
  if (!s) {
    bad_type("read-string", MALTYPE_STRING, val);
  }
  return read_str(s);
  // Implicit error propagation
}

MalType mal_slurp(list args) {

  explode1("slurp", args, a1);

  const char* filename = is_string(a1);
  if (!filename) {
    bad_type("slurp", MALTYPE_STRING, a1);
  }

  FILE* file = fopen(filename, "rb");

  if (!file){
    make_error("'slurp': file not found '%s'", filename);
  }

  fseek(file, 0, SEEK_END);
  size_t file_length = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buffer = (char*)GC_MALLOC(sizeof(*buffer) * file_length + 1);
  size_t read = fread(buffer, sizeof(*buffer), file_length, file);
  // close before raising an exception
  fclose(file);
  if (file_length != read) {
    make_error("'slurp': failed to read file '%s'", filename);
  }

  buffer[file_length] = '\0';
  return make_string(buffer);
}

MalType mal_atom(list args) {
  explode1("atom", args, val);
  return make_atom(val);
}
MalType mal_deref(list args) {
  explode1("deref", args, val);
  MalType* atm = is_atom(val);
  if (!atm) {
    bad_type("deref", MALTYPE_ATOM, val);
  }
  return *atm;
}
MalType mal_reset_bang(list args) {
  explode2("reset!", args, a1, a2);
  MalType* atm = is_atom(a1);
  if (!atm) {
    bad_type("reset!", MALTYPE_ATOM, a1);
  }
  *atm = a2;
  return a2;
}
MalType mal_swap_bang(list args) {
  if (!args || !args->next) {
    bad_arg_count("swap!", "at least two arguments", args);
  }
  MalType* atm = is_atom(args->data);
  if (!atm) {
    bad_type("swap!", MALTYPE_ATOM, args->data);
  }
  MalType fn = args->next->data;
  check_type("swap!", MALTYPE_CLOSURE | MALTYPE_FUNCTION | MALTYPE_MACRO, fn);
  list fn_args = list_push(args->next->next, *atm);
  MalType result = apply(fn, fn_args);

  if (mal_error) {
    return NULL;
  }
  else {
    *atm = result;
    return result;
  }
}

MalType mal_throw(list args) {
  explode1("throw", args, a1);

  /* re-throw an existing exception */
  assert(!mal_error);
  /* create a new exception */
  mal_error = a1;
  return NULL;
}

MalType mal_apply(list args) {

  if (!args || !args->next) {
    bad_arg_count("apply", "at least two arguments", args);
  }
  MalType func = args->data;
  check_type("apply", MALTYPE_CLOSURE | MALTYPE_FUNCTION | MALTYPE_MACRO, func);
  args = args->next;

  /* assemble loose arguments */
  list lst = NULL;
  list* lst_last = &lst;
  while(args->next) {
    *lst_last = list_push(NULL, args->data);
    lst_last = &(*lst_last)->next;
    args = args->next;
  }

  MalType final = args->data;

  vector_t v = is_vector(final);
  //  Append the elements of the final sequence,
  //  efficiently if it is a list.
  if (v) {
    for (size_t i = v->count; i--; ) {
      *lst_last = list_push(*lst_last, v->nth[i]);
    }
  }
  else if (!is_list(final, lst_last)) {
    bad_type("swap!", MALTYPE_LIST | MALTYPE_VECTOR, final);
  }

  return apply(func, lst);
  // Implicit error propagation
}

MalType mal_map(list args) {

  explode2("map", args, func, arg);

  check_type("map", MALTYPE_CLOSURE | MALTYPE_FUNCTION | MALTYPE_MACRO, func);
    //  This check is not redundant when arg is empty.

  check_type("map", MALTYPE_LIST | MALTYPE_VECTOR, arg);
  seq_cursor arg_list = seq_iter(arg);
  list result_list = NULL;
  list* result_list_last = &result_list;

  while(seq_cont(arg, arg_list)) {

    MalType result = apply(func, list_push(NULL, seq_item(arg, arg_list)));

    /* early return if error */
    if (mal_error) {
      return NULL;
    }
    else {
      *result_list_last = list_push(NULL, result);
      result_list_last = &(*result_list_last)->next;

    }
    arg_list = seq_next(arg, arg_list);
  }
  return make_list(result_list);
}

MalType mal_symbol(list args) {
  explode1("symbol", args, val);

  const char* s = is_string(val);
  if (!s) {
    bad_type("symbol", MALTYPE_STRING, val);
  }
  return make_symbol(s);
}

MalType mal_keyword(list args) {

  explode1("keyword", args, val);

  const char* s;
  if ((s = is_string (val))) {
    return make_keyword(s);
  }
  else if ((s = is_keyword(val))) {
    return val;
  }
  else {
    bad_type("keyword", MALTYPE_KEYWORD | MALTYPE_STRING, val);
  }
}

MalType mal_vector(list args) {
  /* Accepts any number and type of arguments */
  size_t capacity = list_count(args);
  struct vector* v = vector_new(capacity);
  while (args) {
    vector_append(&capacity, &v, args->data);
    args = args->next;
  }
  assert(v->count == capacity);
  return make_vector(v);
}

MalType mal_vec(list args) {

  /* Accepts a single argument */

  explode1("vec", args, val);

  list l;
  vector_t v;
  if ((v = is_vector(val))) {
    return val;
  }
  else if (is_list ( val, &l)) {
    return mal_vector(l);
  }
  else {
    bad_type("vec", MALTYPE_LIST | MALTYPE_VECTOR, val);
  }
}

MalType map_assoc_mutate(const char* context, struct map* new_lst, list args) {
  for (list a = args; a ; a = a->next->next) {
    check_type(context, MALTYPE_KEYWORD | MALTYPE_STRING, a->data);
    if (!a->next) {
      bad_arg_count("assoc", "an even count of key/value pairs", args);
    }
    new_lst = hashmap_put(new_lst, a->data, a->next->data);
  }
  return make_hashmap(new_lst);
}

MalType mal_hash_map(list args) {
  return map_assoc_mutate("hash-map", map_empty(), args);
}

MalType mal_get(list args) {

  explode2("get", args, map, key);

  check_type("get", MALTYPE_KEYWORD | MALTYPE_STRING, key);

  hashmap mal_list;
  if(is_nil(map)) {
    return make_nil();
  }
  else if(!(mal_list = is_hashmap(map))) {
    bad_type("get", MALTYPE_HASHMAP | MALTYPE_NIL, map);
  }

  MalType result = hashmap_get(mal_list, key);

  if (!result) {
    return make_nil();
  }

  return result;
}

MalType mal_contains_questionmark(list args) {

  explode2("contains?", args, map, key);

  check_type("contains?", MALTYPE_KEYWORD | MALTYPE_STRING, key);

  hashmap mal_list;
  if(is_nil(map)) {
    return make_nil();
  }
  if (!(mal_list = is_hashmap(map))) {
    bad_type("contains?", MALTYPE_HASHMAP | MALTYPE_NIL, map);
  }

  MalType result = hashmap_get(mal_list, key);

  return make_boolean(result);
}

MalType mal_assoc(list args) {
  if (!args) {
    bad_arg_count("assoc", "at least one argument", args);
  }
  MalType map = args->data;
  hashmap m = is_hashmap(map);
  if (!m) {
    bad_type("assoc", MALTYPE_HASHMAP, map);
  }
  return map_assoc_mutate("assoc", map_copy(m), args->next);
}

MalType mal_dissoc(list args) {

  if (!args) {
    bad_arg_count("dissoc", "at least one argument", args);
  }
  MalType map = args->data;
  hashmap m = is_hashmap(map);
  if (!m) {
    bad_type("dissoc", MALTYPE_HASHMAP, map);
  }
  struct map* new_list = map_copy(m);

  args = args->next;

    list dis_args = args;

    while(dis_args) {

      check_type("dissoc", MALTYPE_KEYWORD | MALTYPE_STRING, dis_args->data);
      map_dissoc_mutate(new_list, dis_args->data);
      dis_args = dis_args->next;
    }

  return make_hashmap(new_list);
}


MalType mal_keys(list args) {

  explode1("keys", args, map);

  hashmap m = is_hashmap(map);
  if (!m) {
    bad_type("keys", MALTYPE_HASHMAP, map);
  }
  map_cursor lst = map_iter(m);

  list result = NULL;
  while(map_cont(m, lst)) {

      result = list_push(result, map_key(m, lst));
      lst = map_next(m, lst);
  }
  return make_list(result);
}

MalType mal_vals(list args) {

  explode1("vals", args, map);

  hashmap m = is_hashmap(map);
  if (!m) {
    bad_type("vals", MALTYPE_HASHMAP, map);
  }
  map_cursor lst = map_iter(m);

  list result = NULL;
  while(map_cont(m, lst)) {

    result = list_push(result, map_val(m, lst));
    lst = map_next(m, lst);
  }
  return make_list(result);
}

MalType mal_time_ms(list args) {
  explode0("time-ms", args);

  struct timeval tv;
  gettimeofday(&tv, NULL);
  long ms = tv.tv_sec * 1000 + tv.tv_usec/1000.0 + 0.5;

  return make_float(ms);
}


MalType mal_conj(list args) {

  if (!args) {
    bad_arg_count("conj", "at least one argument", args);
  }
  MalType lst = args->data;

  list rest = args->next;

  vector_t src;
  list new_lst;
  if (is_list(lst, &new_lst)) {

    while(rest) {
      new_lst = list_push(new_lst, rest->data);
      rest = rest->next;
    }
    return make_list(new_lst);
  }
  else if ((src = is_vector(lst))) {

    size_t capacity = src->count + list_count(rest);
    struct vector* new_vec = vector_new(capacity);

    for (size_t i = 0; i < src->count; i++) {
      vector_append(&capacity, &new_vec, src->nth[i]);
    }

    while(rest) {
      vector_append(&capacity, &new_vec, rest->data);
      rest = rest->next;
    }
    assert(new_vec->count == capacity);
    return make_vector(new_vec);
  }
  else {
    bad_type("conj", MALTYPE_LIST | MALTYPE_VECTOR, lst);
  }
}

MalType mal_seq(list args) {

  explode1("seq", args, val);

  vector_t v;
  list lst = NULL;
  const char* ch;

  if (is_list(val, &lst)) {
    return lst ? val : make_nil();
  }
  else if ((ch = is_string(val))) {

    /* empty string */
    if (*ch == '\0') {
      return make_nil();
    }
    else {
      for (size_t i = strlen(ch); i--; ) {
        char* new_ch = GC_MALLOC(2);
        *new_ch = ch[i];
        assert(!new_ch[1]);

        lst = list_push(lst, make_string(new_ch));
      }
      return make_list(lst);
    }
  }
  else if ((v = is_vector(val))) {
    for (size_t i = v->count; i--; ) {
      lst = list_push(lst, v->nth[i]);
    }
    return lst ? make_list(lst) : make_nil();
  }
  else if (is_nil(val)) {
    return make_nil();
  }
  else {
    bad_type("seq", MALTYPE_LIST | MALTYPE_VECTOR | MALTYPE_NIL | MALTYPE_STRING, val);
  }
}

MalType mal_meta(list args) {

  explode1("meta", args, val);

  return meta(val);
}

MalType mal_with_meta(list args) {

  explode2("with-meta", args, val, metadata);

  list l;
  if (is_list(val, &l)) return make_list_m(l, metadata);
  vector_t v = is_vector(val);
  if (v) return make_vector_m(v, metadata);
  hashmap m = is_hashmap(val);
  if (m) return make_hashmap_m(m, metadata);
  function_t f = is_function(val);
  if (f) return make_function_m(f, metadata);
  MalClosure c = is_closure(val);
  if (c) return make_closure_m(c->env, c->fnstar_args, metadata);
  bad_type("with-meta",
           MALTYPE_LIST | MALTYPE_VECTOR | MALTYPE_HASHMAP | MALTYPE_FUNCTION | MALTYPE_CLOSURE,
           val);
}

MalType mal_readline(list args) {
  explode1("readline", args, prompt);

  const char* prompt_str = is_string(prompt);
  if (!prompt_str) {
    bad_type("readline", MALTYPE_STRING, prompt);
  }
  const char* str = readline_gc(prompt_str);
  if(!str)
    return make_nil();
  return make_string(str);
}


/* helper functions */

inline MalType make_boolean(bool x) {
  return x ? make_true() : make_false();
}


#ifdef WITH_FFI
struct {
  const char*     c_type;
  enum mal_type_t mal_type;
  ffi_type*       ffit;
} core_ffi_translations[] = {
  { "void",    MALTYPE_NIL,     &ffi_type_void },
  { "string",  MALTYPE_STRING,  &ffi_type_pointer },
  { "char*",   MALTYPE_STRING,  &ffi_type_pointer },
  { "char *",  MALTYPE_STRING,  &ffi_type_pointer },
  { "integer", MALTYPE_INTEGER, &ffi_type_sint64 },
  { "int64",   MALTYPE_INTEGER, &ffi_type_sint64 },
  { "int32",   MALTYPE_INTEGER, &ffi_type_sint32 },
  { "double",  MALTYPE_FLOAT,   &ffi_type_double },
  { "float",   MALTYPE_FLOAT,   &ffi_type_float },
};
size_t core_ffi_find(const char *type) {
  for (size_t i = 0;
       i < sizeof(core_ffi_translations) / sizeof(*core_ffi_translations);
       i++) {
    if (!strcmp(core_ffi_translations[i].c_type, type)) {
      return i;
    }
  }
  make_error("'ffi': unknown type '%s'", type);
}
MalType mal_dot(list args) {

  /* (. "lib" "return type" "function" "arg1 type" "arg 1" ...) */

  list a;
  if (!args || !(a = args->next) || !a->next) {
    bad_arg_count(".", "at least three arguments", args);
  }

  const char* lib_name = is_string(args->data);
  if (!lib_name && !is_nil(args->data)) {
    bad_type(".", MALTYPE_STRING | MALTYPE_NIL, args->data);
  }

  const char* return_type_str = is_string(a->data);
  if (!return_type_str) {
    bad_type(".", MALTYPE_STRING, a->data);
  }
  size_t return_type = core_ffi_find(return_type_str);
  if (mal_error) return NULL;

  a = a->next;

  const char* fn_name = is_string(a->data);
  if (!fn_name) {
    bad_type(".", MALTYPE_STRING, a->data);
  }

  a = a->next;

  int       arg_count = 0;
  ffi_type* arg_types[20];
  void*     arg_vals [20];
  while (a) {
    if (20 <= arg_count) {
      bad_arg_count(".", "less than 20 C arguments", args);
    }
    const char* val_type = is_string(a->data);
    if (!val_type) {
      bad_type(".", MALTYPE_STRING, a->data);
    }
    size_t val_type_index = core_ffi_find(val_type);
    if (mal_error) return NULL;
    arg_types[arg_count] = core_ffi_translations[val_type_index].ffit;

    a = a->next;

    if (!a) {
      bad_arg_count(".", "an even number of argument types and values", args);
    }
    arg_vals[arg_count] = mal_type_value_address(a->data);

    a = a->next;
    arg_count++;
  }

  /* open a shared library dynamically and get hold of a function */
  void* lib_handle = dlopen(lib_name, RTLD_LAZY);

  if (!lib_handle) {
    make_error("'ffi': reports: %s", dlerror());
  }

  void* fn = dlsym(lib_handle, fn_name);

  const char* error = dlerror();
  if (error) {
    make_error("'ffi': dlsym could not get handle to function '%s': %s", fn_name, error);
  }

  /* use libffi to call function */
  /* perform the call */
  ffi_cif cif;
  ffi_status status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_count,
                                   core_ffi_translations[return_type].ffit,
                                   arg_types);
  if (status != FFI_OK) {
    make_error("'ffi': call to ffi_prep_cif failed with code: %d", status);
  }

  /* set return type */
  MalType result;
  switch (core_ffi_translations[return_type].mal_type) {
  case MALTYPE_NIL: {
    char retval;
    ffi_call(&cif, FFI_FN(fn), &retval, arg_vals);
    result = make_nil();
    break;
  }
  case MALTYPE_STRING: {
    char* retval;
    ffi_call(&cif, FFI_FN(fn), &retval, arg_vals);
    result = make_string(retval);
    break;
  }
  case MALTYPE_INTEGER: {
    long retval;
    ffi_call(&cif, FFI_FN(fn), &retval, arg_vals);
    result = make_integer(retval);
    break;
  }
  case MALTYPE_FLOAT: {
    double retval;
    ffi_call(&cif, FFI_FN(fn), &retval, arg_vals);
    result = make_float(retval);
    break;
  }
  default:
    assert(false);
  }

  /* close the library */
  dlclose(lib_handle);

  return result;
}

#endif
