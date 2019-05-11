#include <assert.h>
#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include "env.h"
#include "core.h"
#include "reader.h"
#include "printer.h"
#include "types.h"
#include "util.h"

struct hashmap* core_ns() {
  struct hashmap *ns = GC_MALLOC(sizeof(struct hashmap));
  hashmap_init(ns, hashmap_hash_string, hashmap_compare_string, 100);
  hashmap_set_key_alloc_funcs(ns, hashmap_alloc_key_string, NULL);
  hashmap_put(ns, "+", core_add);
  hashmap_put(ns, "-", core_sub);
  hashmap_put(ns, "*", core_mul);
  hashmap_put(ns, "/", core_div);
  hashmap_put(ns, "count", core_count);
  hashmap_put(ns, "prn", core_prn);
  hashmap_put(ns, "list", core_list);
  hashmap_put(ns, "list?", core_is_list);
  hashmap_put(ns, "empty?", core_is_empty);
  hashmap_put(ns, "=", core_is_equal);
  hashmap_put(ns, ">", core_is_gt);
  hashmap_put(ns, ">=", core_is_gte);
  hashmap_put(ns, "<", core_is_lt);
  hashmap_put(ns, "<=", core_is_lte);
  hashmap_put(ns, "pr-str", core_pr_str);
  hashmap_put(ns, "str", core_str);
  hashmap_put(ns, "println", core_println);
  hashmap_put(ns, "read-string", core_read_string);
  hashmap_put(ns, "slurp", core_slurp);
  hashmap_put(ns, "atom", core_atom);
  hashmap_put(ns, "atom?", core_is_atom);
  hashmap_put(ns, "deref", core_deref);
  hashmap_put(ns, "reset!", core_reset);
  hashmap_put(ns, "swap!", core_swap);
  hashmap_put(ns, "cons", core_cons);
  hashmap_put(ns, "concat", core_concat);
  hashmap_put(ns, "nth", core_nth);
  hashmap_put(ns, "first", core_first);
  hashmap_put(ns, "rest", core_rest);
  hashmap_put(ns, "throw", core_throw);
  hashmap_put(ns, "apply", core_apply);
  hashmap_put(ns, "map", core_map);
  hashmap_put(ns, "nil?", core_is_nil);
  hashmap_put(ns, "true?", core_is_true);
  hashmap_put(ns, "false?", core_is_false);
  hashmap_put(ns, "symbol?", core_is_symbol);
  hashmap_put(ns, "keyword?", core_is_keyword);
  hashmap_put(ns, "vector?", core_is_vector);
  hashmap_put(ns, "map?", core_is_map);
  hashmap_put(ns, "sequential?", core_is_sequential);
  hashmap_put(ns, "symbol", core_symbol);
  hashmap_put(ns, "keyword", core_keyword);
  hashmap_put(ns, "vector", core_vector);
  hashmap_put(ns, "hash-map", core_hash_map);
  hashmap_put(ns, "assoc", core_assoc);
  hashmap_put(ns, "dissoc", core_dissoc);
  hashmap_put(ns, "get", core_get);
  hashmap_put(ns, "contains?", core_contains);
  hashmap_put(ns, "keys", core_keys);
  hashmap_put(ns, "vals", core_vals);
  hashmap_put(ns, "readline", core_readline);
  hashmap_put(ns, "meta", core_meta);
  hashmap_put(ns, "with-meta", core_with_meta);
  hashmap_put(ns, "seq", core_seq);
  hashmap_put(ns, "conj", core_conj);
  hashmap_put(ns, "time-ms", core_time_ms);
  hashmap_put(ns, "string?", core_is_string);
  hashmap_put(ns, "number?", core_is_number);
  hashmap_put(ns, "fn?", core_is_fn);
  hashmap_put(ns, "macro?", core_is_macro);
  return ns;
}

MalType* core_add(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  if (argc == 0) {
    return mal_number(0);
  } else {
    long long result = args[0]->number;
    for (size_t i=1; i<argc; i++) {
      result += args[i]->number;
    }
    return mal_number(result);
  }
}

MalType* core_sub(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  assert(argc > 0);
  long long result = args[0]->number;
  for (size_t i=1; i<argc; i++) {
    result -= args[i]->number;
  }
  return mal_number(result);
}

MalType* core_mul(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  if (argc == 0) {
    return mal_number(1);
  } else {
    long long result = args[0]->number;
    for (size_t i=1; i<argc; i++) {
      result *= args[i]->number;
    }
    return mal_number(result);
  }
}

MalType* core_div(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  assert(argc > 0);
  long long result = args[0]->number;
  for (size_t i=1; i<argc; i++) {
    result /= args[i]->number;
  }
  return mal_number(result);
}

MalType* core_count(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to count");
  MalType *arg = args[0];
  switch (arg->type) {
    case MAL_EMPTY_TYPE:
    case MAL_NIL_TYPE:
      return mal_number(0);
    case MAL_CONS_TYPE:
      return mal_number(mal_list_len(arg));
    case MAL_VECTOR_TYPE:
      return mal_number(mal_vector_len(arg));
    default:
      printf("Object type to count not supported\n");
      return mal_nil();
  }
}

MalType* core_prn(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *out = mal_string("");
  for (size_t i=0; i<argc; i++) {
    mal_string_append(out, pr_str(args[i], 1));
    if (i < argc-1) mal_string_append_char(out, ' ');
  }
  printf("%s\n", out->str);
  return mal_nil();
}

MalType* core_list(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *vec = mal_vector();
  for (size_t i=0; i<argc; i++) {
    mal_vector_push(vec, args[i]);
  }
  return mal_vector_to_list(vec);
}

MalType* core_is_list(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to list?");
  MalType *arg = args[0];
  return is_empty(arg) || is_cons(arg) ? mal_true() : mal_false();
}

MalType* core_is_empty(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to empty?");
  MalType *arg = args[0];
  if (is_empty(arg)) return mal_true();
  if (is_vector(arg) && arg->vec_len == 0) return mal_true();
  return mal_false();
}

MalType* core_is_equal(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 argument to =");
  return is_equal(args[0], args[1]) ? mal_true() : mal_false();
}

MalType* core_is_gt(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 argument to >");
  MalType *arg1 = args[0];
  MalType *arg2 = args[1];
  mal_assert(is_number(arg1) && is_number(arg2), "Both arguments to > must be numbers");
  return arg1->number > arg2->number ? mal_true() : mal_false();
}

MalType* core_is_gte(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 argument to >=");
  MalType *arg1 = args[0];
  MalType *arg2 = args[1];
  mal_assert(is_number(arg1) && is_number(arg2), "Both arguments to >= must be numbers");
  return arg1->number == arg2->number || arg1->number > arg2->number ? mal_true() : mal_false();
}

MalType* core_is_lt(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 argument to <");
  MalType *arg1 = args[0];
  MalType *arg2 = args[1];
  mal_assert(is_number(arg1) && is_number(arg2), "Both arguments to < must be numbers");
  return arg1->number < arg2->number ? mal_true() : mal_false();
}

MalType* core_is_lte(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 argument to <=");
  MalType *arg1 = args[0];
  MalType *arg2 = args[1];
  mal_assert(is_number(arg1) && is_number(arg2), "Both arguments to <= must be numbers");
  return arg1->number == arg2->number || arg1->number < arg2->number ? mal_true() : mal_false();
}

MalType* core_pr_str(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *out = mal_string("");
  for (size_t i=0; i<argc; i++) {
    mal_string_append(out, pr_str(args[i], 1));
    if (i < argc-1) mal_string_append_char(out, ' ');
  }
  return out;
}

MalType* core_str(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *out = mal_string("");
  for (size_t i=0; i<argc; i++) {
    mal_string_append(out, pr_str(args[i], 0));
  }
  return out;
}

MalType* core_println(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *out = mal_string("");
  for (size_t i=0; i<argc; i++) {
    mal_string_append(out, pr_str(args[i], 0));
    if (i < argc-1) mal_string_append_char(out, ' ');
  }
  printf("%s\n", out->str);
  return mal_nil();
}

MalType* core_read_string(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to read-string");
  MalType *code = args[0];
  mal_assert(is_string(code), "read-string expects a string argument");
  return read_str(code->str);
}

MalType* core_slurp(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to slurp");
  MalType *filename = args[0];
  mal_assert(is_string(filename), "slurp expects a string argument");
  return read_file(filename->str);
}

MalType* core_atom(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to atom");
  return mal_atom(args[0]);
}

MalType* core_is_atom(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to atom?");
  return is_atom(args[0]) ? mal_true() : mal_false();
}

MalType* core_deref(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to deref");
  MalType *val = args[0];
  mal_assert(is_atom(val), "deref expects an atom argument");
  return val->atom_val;
}

MalType* core_reset(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to reset!");
  MalType *atom = args[0];
  mal_assert(is_atom(atom), "reset! expects an atom argument");
  MalType *inner_val = args[1];
  atom->atom_val = inner_val;
  return inner_val;
}

MalType* core_swap(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc >= 2, "Expected at least 2 arguments to swap!");
  MalType *atom = args[0];
  mal_assert(is_atom(atom), "swap! expects an atom argument");
  MalType *lambda = args[1];
  mal_assert(is_lambda(lambda), "swap! expects a lambda argument");
  MalType **swap_args = GC_MALLOC(argc * sizeof(MalType*));
  swap_args[0] = atom->atom_val;
  for(size_t i=2; i<argc; i++) {
    swap_args[i - 1] = args[i];
  }
  atom->atom_val = trampoline(mal_continuation(lambda->fn, lambda->env, argc - 1, swap_args));
  return atom->atom_val;
}

MalType* core_cons(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to cons");
  MalType *new_item = args[0];
  if (is_vector(args[1])) {
    MalType *vec = args[1];
    MalType *new_vec = mal_vector();
    mal_vector_push(new_vec, new_item);
    for (size_t i=0; i<mal_vector_len(vec); i++) {
      mal_vector_push(new_vec, vec->vec[i]);
    }
    return mal_vector_to_list(new_vec);
  } else {
    return mal_cons(new_item, args[1]);
  }
}

MalType* core_concat(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *final = mal_vector(), *item;
  struct list_or_vector_iter *iter;
  for (size_t i=0; i<argc; i++) {
    for (iter = list_or_vector_iter(args[i]); iter; iter = list_or_vector_iter_next(iter)) {
      item = list_or_vector_iter_get_obj(iter);
      mal_vector_push(final, item);
    }
  }
  return mal_vector_to_list(final);
}

MalType* core_nth(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to nth");
  MalType *list_or_vector = args[0];
  mal_assert(is_list_like(list_or_vector), "nth expects a list or a vector argument");
  MalType *mal_index = args[1];
  mal_assert(is_number(mal_index), "nth expects a number as the index argument");
  size_t index = (size_t)mal_index->number;
  if (is_vector(list_or_vector)) {
    size_t size = mal_vector_len(list_or_vector);
    if (index >= size) {
      return mal_error(mal_string("nth index out of range"));
    }
    return mal_vector_ref(list_or_vector, index);
  } else {
    size_t size = mal_list_len(list_or_vector);
    if (index >= size) {
      return mal_error(mal_string("nth index out of range"));
    }
    return mal_list_ref(list_or_vector, index);
  }
}

MalType* core_first(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to first");
  MalType *list = args[0];
  mal_assert(is_list_like(list) || is_nil(list), "first expects a list or a vector argument");
  if (is_empty(list) || is_nil(list)) {
    return mal_nil();
  } else if (is_cons(list)) {
    return list->car;
  } else if (list->vec_len > 0) {
    return list->vec[0];
  } else {
    return mal_nil();
  }
}

MalType* core_rest(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to rest");
  MalType *list = args[0];
  mal_assert(is_list_like(list) || is_nil(list), "rest expects a list or a vector argument");
  if (is_empty(list) || is_nil(list)) {
    return mal_empty();
  } else if (is_cons(list)) {
    return list->cdr;
  } else if (list->vec_len > 0) {
    return mal_cdr2(list);
  } else {
    return mal_empty();
  }
}

MalType* core_throw(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to throw");
  MalType *val = args[0];
  return mal_error(val);
}

MalType* core_apply(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc >= 2, "Expected at least 2 arguments to apply");
  MalType *lambda = args[0];
  mal_assert(is_lambda(lambda), "Expected first argument of apply to be a lambda");
  MalType *args_vec = mal_vector();
  struct list_or_vector_iter *iter;
  MalType *arg;
  for (size_t i=1; i<argc; i++) {
    if (is_list_like(args[i])) {
      for (iter = list_or_vector_iter(args[i]); iter; iter = list_or_vector_iter_next(iter)) {
        arg = list_or_vector_iter_get_obj(iter);
        mal_vector_push(args_vec, arg);
      }
    } else {
      mal_vector_push(args_vec, args[i]);
    }
  }
  return trampoline(mal_continuation(lambda->fn, lambda->env, args_vec->vec_len, args_vec->vec));
}

MalType* core_map(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to map");
  MalType *lambda = args[0];
  mal_assert(is_lambda(lambda), "Expected first argument of map to be a lambda");
  MalType *list = args[1];
  mal_assert(is_list_like(list), "Expected second argument of map to be a list or vector");
  MalType *result_vec = mal_vector();
  struct list_or_vector_iter *iter;
  MalType *val;
  for (iter = list_or_vector_iter(list); iter; iter = list_or_vector_iter_next(iter)) {
    val = list_or_vector_iter_get_obj(iter);
    val = trampoline(mal_continuation_1(lambda->fn, lambda->env, val));
    bubble_if_error(val);
    mal_vector_push(result_vec, val);
  }
  return mal_vector_to_list(result_vec);
}

MalType* core_is_nil(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to nil?");
  MalType *val = args[0];
  return is_nil(val) ? mal_true() : mal_false();
}

MalType* core_is_true(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to true?");
  MalType *val = args[0];
  return is_true(val) ? mal_true() : mal_false();
}

MalType* core_is_false(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to false?");
  MalType *val = args[0];
  return is_false(val) ? mal_true() : mal_false();
}

MalType* core_is_symbol(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to symbol?");
  MalType *val = args[0];
  return is_symbol(val) ? mal_true() : mal_false();
}

MalType* core_is_keyword(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to keyword?");
  MalType *val = args[0];
  return is_keyword(val) ? mal_true() : mal_false();
}

MalType* core_is_vector(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to vector?");
  MalType *val = args[0];
  return is_vector(val) ? mal_true() : mal_false();
}

MalType* core_is_map(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to map?");
  MalType *val = args[0];
  return is_hashmap(val) ? mal_true() : mal_false();
}

MalType* core_is_sequential(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to sequential?");
  MalType *val = args[0];
  return is_list_like(val) ? mal_true() : mal_false();
}

MalType* core_symbol(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to symbol function");
  MalType *val = args[0];
  mal_assert(is_string(val), "symbol function expects a string argument");
  return mal_symbol(val->str);
}

MalType* core_keyword(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to keyword function");
  MalType *val = args[0];
  if (is_keyword(val)) {
    return val;
  }
  mal_assert(is_string(val), "keyword function expects a string argument");
  return mal_keyword(val->str);
}

MalType* core_vector(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  MalType *vec = mal_vector();
  for (size_t i=0; i<argc; i++) {
    mal_vector_push(vec, args[i]);
  }
  return vec;
}

MalType* core_hash_map(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc % 2 == 0, "Expected even number of arguments to hash-map");
  MalType *map = mal_hashmap();
  for (size_t i=0; i<argc; i+=2) {
    mal_hashmap_put(map, args[i], args[i+1]);
  }
  return map;
}

MalType* core_assoc(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc % 2 == 1, "Expected odd number of arguments to assoc");
  MalType *map = args[0];
  mal_assert(is_hashmap(map), "Expected first argument to assoc to be a hash-map");
  MalType *new_map = mal_hashmap();
  struct hashmap_iter *iter;
  MalType *key, *val;
  for (iter = hashmap_iter(&map->hashmap); iter; iter = hashmap_iter_next(&map->hashmap, iter)) {
    key = read_str((char*)hashmap_iter_get_key(iter));
    val = (MalType*)hashmap_iter_get_data(iter);
    mal_hashmap_put(new_map, key, val);
  }
  for (size_t i=1; i<argc; i+=2) {
    mal_hashmap_put(new_map, args[i], args[i+1]);
  }
  return new_map;
}

MalType* core_dissoc(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc >= 2, "Expected at least 2 arguments to disassoc");
  MalType *map = args[0];
  mal_assert(is_hashmap(map), "Expected first argument to disassoc to be a hash-map");
  MalType *new_map = mal_hashmap();
  struct hashmap_iter *iter;
  MalType *key, *val;
  int skip;
  for (iter = hashmap_iter(&map->hashmap); iter; iter = hashmap_iter_next(&map->hashmap, iter)) {
    skip = 0;
    key = read_str((char*)hashmap_iter_get_key(iter));
    for (size_t i=1; i<argc; i++) {
      if (is_equal(args[i], key)) skip = 1;
    }
    if (skip) continue;
    val = (MalType*)hashmap_iter_get_data(iter);
    mal_hashmap_put(new_map, key, val);
  }
  return new_map;
}

MalType* core_get(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to get");
  MalType *map = args[0];
  if (is_nil(map)) {
    return mal_nil();
  }
  mal_assert(is_hashmap(map), "Expected first argument to get function to be a hash-map");
  MalType *key = args[1];
  MalType *val = mal_hashmap_get(map, key);
  if (val) {
    return val;
  } else {
    return mal_nil();
  }
}

MalType* core_contains(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to contains?");
  MalType *map = args[0];
  mal_assert(is_hashmap(map), "Expected first argument to contains function to be a hash-map");
  MalType *key = args[1];
  return mal_hashmap_get(map, key) ? mal_true() : mal_false();
}

MalType* core_keys(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to keys");
  MalType *map = args[0];
  mal_assert(is_hashmap(map), "Expected first argument to keys function to be a hash-map");
  MalType *keys_vec = mal_hashmap_keys_to_vector(map);
  return mal_vector_to_list(keys_vec);
}

MalType* core_vals(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to vals");
  MalType *map = args[0];
  mal_assert(is_hashmap(map), "Expected first argument to vals function to be a hash-map");
  MalType *vals_vec = mal_vector();
  struct hashmap_iter *iter;
  MalType *val;
  for (iter = hashmap_iter(&map->hashmap); iter; iter = hashmap_iter_next(&map->hashmap, iter)) {
    val = (MalType*)hashmap_iter_get_data(iter);
    mal_vector_push(vals_vec, val);
  }
  return mal_vector_to_list(vals_vec);
}

MalType* core_readline(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to readline");
  MalType *prompt = args[0];
  mal_assert(is_string(prompt), "Expected first argument to readline to be a string");
  char buffer[1000];
  printf("%s", prompt->str);
  if (fgets(buffer, 1000, stdin) == NULL) {
    return mal_nil();
  } else {
    size_t len = strlen(buffer);
    if (buffer[len-1] == '\n') {
      buffer[len-1] = 0; // strip the newline
    }
    return mal_string(buffer);
  }
}

MalType* core_meta(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to meta");
  MalType *val = args[0];
  if (val->meta) {
    return val->meta;
  } else {
    return mal_nil();
  }
}

MalType* core_with_meta(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 2, "Expected 2 arguments to with-meta");
  MalType *val = args[0];
  MalType *new_val = mal_alloc();
  memcpy(new_val, val, sizeof(MalType));
  new_val->meta = args[1];
  return new_val;
}

MalType* core_seq(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 arguments to seq");
  MalType *val = args[0];
  switch (val->type) {
    case MAL_CONS_TYPE:
      return val;
    case MAL_STRING_TYPE:
      if (val->str_len == 0) {
        return mal_nil();
      } else {
        return mal_string_to_list(val);
      }
    case MAL_VECTOR_TYPE:
      if (val->vec_len == 0) {
        return mal_nil();
      } else {
        return mal_vector_to_list(val);
      }
    default:
      return mal_nil();
	}
}

MalType* core_conj(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc >= 2, "Expected at least 2 arguments to conj");
  MalType *collection = args[0];
  mal_assert(
    is_empty(collection) || is_cons(collection) || is_vector(collection),
    "Expected first argument to conj to be a list or vector"
  );
  if (is_empty(collection) || is_cons(collection)) {
    MalType *node = collection;
    for (size_t i=1; i<argc; i++) {
      node = mal_cons(args[i], node);
    }
    return node;
  } else {
    MalType *vec = mal_vector();
    for (size_t i=0; i<collection->vec_len; i++) {
      mal_vector_push(vec, collection->vec[i]);
    }
    for (size_t i=1; i<argc; i++) {
      mal_vector_push(vec, args[i]);
    }
    return vec;
  }
}

MalType* core_time_ms(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  UNUSED(args);
  mal_assert(argc == 0, "Expected 0 arguments to time-ms");
  struct timeval tval;
  gettimeofday(&tval, NULL);
  long long microseconds = (tval.tv_sec * 1000000) + tval.tv_usec;
  return mal_number(microseconds / 1000);
}

MalType* core_is_string(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to string?");
  MalType *val = args[0];
  return is_string(val) ? mal_true() : mal_false();
}

MalType* core_is_number(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to number?");
  MalType *val = args[0];
  return is_number(val) ? mal_true() : mal_false();
}

MalType* core_is_fn(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to fn?");
  MalType *val = args[0];
  return is_lambda(val) && !val->is_macro ? mal_true() : mal_false();
}

MalType* core_is_macro(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  mal_assert(argc == 1, "Expected 1 argument to macro?");
  MalType *val = args[0];
  return is_macro(val) ? mal_true() : mal_false();
}

void add_core_ns_to_env(MalEnv *env) {
  struct hashmap *ns = core_ns();
  struct hashmap_iter *core_iter;
  char *name;
  MalType* (*fn)(MalEnv*, size_t, MalType**);
  for (core_iter = hashmap_iter(ns); core_iter; core_iter = hashmap_iter_next(ns, core_iter)) {
    name = (char*)hashmap_iter_get_key(core_iter);
    fn = hashmap_iter_get_data(core_iter);
    env_set(env, name, mal_builtin_function(fn, name, env));
  }
}
