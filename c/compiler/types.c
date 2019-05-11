#include <assert.h>
#include <gc.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashmap.h"
#include "printer.h"
#include "reader.h"
#include "types.h"
#include "util.h"

MalType* mal_alloc() {
  MalType *val = GC_MALLOC(sizeof(MalType));
  val->meta = NULL;
  val->is_macro = 0;
  return val;
}

MalType* mal_nil() {
  MalType *val = mal_alloc();
  val->type = MAL_NIL_TYPE;
  return val;
}

int is_nil(MalType *val) {
  return val->type == MAL_NIL_TYPE;
}

MalType* mal_empty() {
  MalType *val = mal_alloc();
  val->type = MAL_EMPTY_TYPE;
  return val;
}

int is_empty(MalType *val) {
  return val->type == MAL_EMPTY_TYPE;
}

MalType* mal_true() {
  MalType *val = mal_alloc();
  val->type = MAL_TRUE_TYPE;
  return val;
}

MalType* mal_false() {
  MalType *val = mal_alloc();
  val->type = MAL_FALSE_TYPE;
  return val;
}

MalType* mal_cons(MalType *car, MalType *cdr) {
  MalType *val = mal_alloc();
  val->type = MAL_CONS_TYPE;
  val->car = car;
  val->cdr = cdr;
  return val;
}

MalType* mal_car(MalType *val) {
  assert(is_cons(val));
  return val->car;
}

MalType* mal_cdr(MalType *val) {
  assert(is_cons(val));
  return val->cdr;
}

MalType* mal_car2(MalType *val) {
  assert(is_cons(val) || is_vector(val));
  if (is_cons(val)) {
    return val->car;
  } else {
    return val->vec[0];
  }
}

MalType* mal_cdr2(MalType *val) {
  assert(is_cons(val) || is_vector(val));
  if (is_cons(val)) {
    return val->cdr;
  } else {
    MalType *rest = mal_empty();
    for (int i=mal_vector_len(val)-1; i>=1; i--) {
      rest = mal_cons(mal_vector_ref(val, i), rest);
    }
    return rest;
  }
}

size_t mal_list_len(MalType *val) {
  if (is_empty(val)) return 0;
  assert(is_cons(val));
  size_t len = 1;
  MalType *cell = val;
  while (!is_empty(cell = mal_cdr(cell))) {
    len++;
  }
  return len;
}

MalType* mal_list_ref(MalType *val, size_t index) {
  assert(is_cons(val));
  struct list_or_vector_iter *iter;
  size_t i = 0;
  for (iter = list_or_vector_iter(val); iter; iter = list_or_vector_iter_next(iter)) {
    if (i == index) {
      return list_or_vector_iter_get_obj(iter);
    }
    i++;
  }
  assert(0 && "index out of range");
}


#define STRING_GROW_FACTOR 2

MalType* mal_string(char *str) {
  MalType *val = mal_alloc();
  size_t len = strlen(str);
  val->type = MAL_STRING_TYPE;
  val->str_len = len;
  val->str_cap = len;
  val->str = GC_MALLOC(len + 1);
  snprintf(val->str, len + 1, "%s", str);
  return val;
}

void mal_grow_string(MalType *val, size_t capacity) {
  size_t len = strlen(val->str);
  assert(capacity >= len);
  val->str = GC_REALLOC(val->str, capacity + 1);
  val->str_cap = capacity;
}

void mal_grow_string_at_least(MalType *val, size_t min_capacity) {
  size_t capacity = val->str_cap;
  if (capacity >= min_capacity) return;
  if (capacity > 0 && min_capacity <= capacity * STRING_GROW_FACTOR) {
    mal_grow_string(val, capacity * STRING_GROW_FACTOR);
  } else {
    mal_grow_string(val, min_capacity);
  }
}

void mal_string_append(MalType *val, char *str) {
  assert(is_string(val));
  size_t new_len = strlen(str);
  if (new_len == 0) return;
  size_t total_len = val->str_len + new_len;
  mal_grow_string_at_least(val, total_len);
  strcat(val->str, str);
  val->str_len = total_len;
  assert(strlen(val->str) == val->str_len);
}

void mal_string_append_mal_string(MalType *val, MalType *str) {
  assert(strlen(str->str) == str->str_len);
  mal_string_append(val, str->str);
}

void mal_string_append_long_long(MalType *val, long long n) {
  return mal_string_append(val, long_long_to_string(n));
}

void mal_string_append_char(MalType *val, char c) {
  assert(is_string(val));
  size_t total_len = val->str_len + 1;
  mal_grow_string_at_least(val, total_len);
  val->str[total_len - 1] = c;
  val->str[total_len] = 0;
  val->str_len = total_len;
}

MalType* mal_string_replace(MalType *orig, char *find, char *replace) {
  char *pos = strstr(orig->str, find);
  assert(pos);
  size_t index = pos - orig->str;
  size_t find_len = strlen(find);
  char *before = substring(orig->str, 0, index);
  char *after = substring(orig->str, index + find_len, orig->str_len - (index + find_len));
  MalType *final = mal_string(before);
  mal_string_append(final, replace);
  mal_string_append(final, after);
  return final;
}

MalType* mal_string_replace_all(MalType *orig, char *find, char *replace) {
  MalType *final = mal_string("");
  char *pos = NULL;
  char *str = orig->str, *before;
  size_t find_len = strlen(find);
  while ((pos = strstr(str, find))) {
    size_t index = pos - str;
    before = substring(str, 0, index);
    mal_string_append(final, before);
    mal_string_append(final, replace);
    str = str + index + find_len;
  }
  mal_string_append(final, str);
  return final;
}

char* mal_string_substring(MalType *orig, size_t start, size_t len) {
  assert(start < orig->str_len);
  assert(start + len <= orig->str_len);
  char *buffer = GC_MALLOC(len + 1);
  snprintf(buffer, len + 1, "%s", orig->str + start);
  return buffer;
}

MalType* mal_string_to_list(MalType *orig) {
  assert(is_string(orig));
  MalType *vec = mal_vector();
  char buffer[5];
  for (size_t i=0; i<orig->str_len; i++) {
    buffer[0] = orig->str[i];
    if (((unsigned char)buffer[0] >> 3) == 30) { // 11110xxx, 4 bytes
      if (i + 3 >= orig->str_len) return mal_error(mal_string("Invalid utf-8 encoding in string"));
      buffer[1] = orig->str[++i];
      buffer[2] = orig->str[++i];
      buffer[3] = orig->str[++i];
      buffer[4] = 0;
    } else if (((unsigned char)buffer[0] >> 4) == 14) { // 1110xxxx, 3 bytes
      if (i + 2 >= orig->str_len) return mal_error(mal_string("Invalid utf-8 encoding in string"));
      buffer[1] = orig->str[++i];
      buffer[2] = orig->str[++i];
      buffer[3] = 0;
    } else if (((unsigned char)buffer[0] >> 5) == 6) { // 110xxxxx, 2 bytes
      if (i + 1 >= orig->str_len) return mal_error(mal_string("Invalid utf-8 encoding in string"));
      buffer[1] = orig->str[++i];
      buffer[2] = 0;
    } else {
      buffer[1] = 0;
    }
    mal_vector_push(vec, mal_string(buffer));
  }
  return mal_vector_to_list(vec);
}

#define VECTOR_INIT_SIZE 10
#define VECTOR_GROW_FACTOR 2

MalType* mal_vector() {
  MalType *val = mal_alloc();
  val->type = MAL_VECTOR_TYPE;
  val->vec_len = 0;
  val->vec_cap = VECTOR_INIT_SIZE;
  val->vec = GC_MALLOC(sizeof(MalType*) * val->vec_cap);
  return val;
}

size_t mal_vector_len(MalType *val) {
  assert(is_vector(val));
  return val->vec_len;
}

MalType* mal_vector_ref(MalType *val, size_t index) {
  assert(is_vector(val));
  return val->vec[index];
}

void mal_vector_push(MalType *vector, MalType *value) {
  assert(is_vector(vector));
  size_t capacity = vector->vec_cap;
  size_t len = mal_vector_len(vector);
  if (len >= capacity) {
    vector->vec_cap *= VECTOR_GROW_FACTOR;
    vector->vec = GC_REALLOC(vector->vec, sizeof(MalType*) * vector->vec_cap);
  }
  vector->vec_len++;
  vector->vec[len] = value;
}

MalType* mal_vector_to_list(MalType *val) {
  int len = mal_vector_len(val);
  MalType *cell = mal_empty();
  for (int i=len-1; i>=0; i--) {
    cell = mal_cons(mal_vector_ref(val, i), cell);
  }
  return cell;
}

MalType* mal_vector_range(MalType *vec, int start, int stop_exclusive) {
  int len = mal_vector_len(vec);
  if (stop_exclusive == -1 || stop_exclusive > len) stop_exclusive = len;
  MalType *new_vec = mal_vector();
  for (int i=start; i<stop_exclusive; i++) {
    mal_vector_push(new_vec, mal_vector_ref(vec, i));
  }
  return new_vec;
}

#define HASHMAP_INIT_SIZE 10

MalType* mal_hashmap() {
  MalType *val = mal_alloc();
  val->type = MAL_HASHMAP_TYPE;
  hashmap_init(&val->hashmap, hashmap_hash_string, hashmap_compare_string, HASHMAP_INIT_SIZE);
  hashmap_set_key_alloc_funcs(&val->hashmap, hashmap_alloc_key_string, NULL);
  return val;
}

MalType* mal_hashmap_get(MalType *map, MalType *key) {
  assert(is_hashmap(map));
  return hashmap_get(&map->hashmap, pr_str(key, 1));
}

void mal_hashmap_put(MalType *map, MalType *key, MalType *val) {
  assert(is_hashmap(map));
  char *key_str = pr_str(key, 1);
  hashmap_remove(&map->hashmap, key_str);
  hashmap_put(&map->hashmap, key_str, (void*)val);
}

void mal_hashmap_remove(MalType *map, MalType *key) {
  assert(is_hashmap(map));
  hashmap_remove(&map->hashmap, pr_str(key, 1));
}

size_t mal_hashmap_size(MalType *map) {
  assert(is_hashmap(map));
  return hashmap_size(&map->hashmap);
}

MalType* mal_hashmap_keys_to_vector(MalType *map) {
  assert(is_hashmap(map));
  MalType *keys_vec = mal_vector();
  struct hashmap_iter *iter;
  MalType *key;
  for (iter = hashmap_iter(&map->hashmap); iter; iter = hashmap_iter_next(&map->hashmap, iter)) {
    key = read_str((char*)hashmap_iter_get_key(iter));
    mal_vector_push(keys_vec, key);
  }
  return keys_vec;
}

MalType* mal_keyword(char *name) {
  MalType *val = mal_alloc();
  val->type = MAL_KEYWORD_TYPE;
  val->keyword = name;
  return val;
}

MalType* mal_number(long long number) {
  MalType *val = mal_alloc();
  val->type = MAL_NUMBER_TYPE;
  val->number = number;
  return val;
}

MalType* mal_symbol(char *name) {
  MalType *val = mal_alloc();
  val->type = MAL_SYMBOL_TYPE;
  val->symbol = string(name);
  return val;
}

MalType* mal_closure(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env) {
  MalType *val = mal_alloc();
  val->type = MAL_LAMBDA_TYPE;
  val->fn = fn;
  val->function_name = NULL;
  val->env = env;
  val->argc = 0;
  val->args = NULL;
  val->is_macro = 0;
  return val;
}

MalType* mal_builtin_function(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), char *function_name, MalEnv *env) {
  MalType *val = mal_alloc();
  val->type = MAL_LAMBDA_TYPE;
  val->fn = fn;
  val->function_name = function_name;
  val->env = env;
  val->argc = 0;
  val->args = NULL;
  val->is_macro = 0;
  return val;
}

MalType* mal_continuation(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env, size_t argc, MalType **args) {
  MalType *val = mal_alloc();
  val->type = MAL_CONTINUATION_TYPE;
  val->fn = fn;
  val->function_name = NULL;
  val->env = env;
  val->argc = argc;
  val->args = args;
  val->is_macro = 0;
  return val;
}

MalType* mal_continuation_0(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env) {
  return mal_continuation(fn, env, 0, NULL);
}

MalType* mal_continuation_1(MalType* (*fn)(MalEnv *env, size_t argc, MalType **args), MalEnv *env, MalType *arg) {
  MalType **args = GC_MALLOC(sizeof(MalType*) * 1);
  args[0] = arg;
  return mal_continuation(fn, env, 1, args);
}

MalType* mal_atom(MalType *inner_val) {
  MalType *val = mal_alloc();
  val->type = MAL_ATOM_TYPE;
  val->atom_val = inner_val;
  return val;
}

MalType* mal_blank_line() {
  MalType *val = mal_alloc();
  val->type = MAL_BLANK_LINE_TYPE;
  return val;
}

MalType* mal_error(MalType *inner_val) {
  MalType *val = mal_alloc();
  val->type = MAL_ERROR_TYPE;
  val->error_val = inner_val;
  return val;
}

size_t list_or_vector_len(MalType *obj) {
  assert(is_empty(obj) || is_cons(obj) || is_vector(obj));
  if (is_empty(obj)) {
    return 0;
  } else if (is_cons(obj)) {
    return mal_list_len(obj);
  } else {
    return mal_vector_len(obj);
  }
}

struct list_or_vector_iter* list_or_vector_iter(MalType *obj) {
  assert(is_list_like(obj));
  if (is_empty(obj) || (obj->type == MAL_VECTOR_TYPE && obj->vec_len == 0)) return NULL;
  struct list_or_vector_iter *iter = GC_MALLOC(sizeof(struct list_or_vector_iter));
  iter->cell = obj;
  if (is_cons(obj)) {
    iter->type = LIST_ITER;
  } else {
    iter->type = VECTOR_ITER;
    iter->len = obj->vec_len;
    iter->i = 0;
  }
  return iter;
}

struct list_or_vector_iter* list_or_vector_iter_next(struct list_or_vector_iter *iter) {
  if (!iter) {
    return NULL;
  } else if (iter->type == LIST_ITER) {
    iter->cell = mal_cdr(iter->cell);
    if (!iter->cell || is_empty(iter->cell)) return NULL;
  } else {
    if (iter->i+1 >= iter->len) {
      return NULL;
    }
    iter->i++;
  }
  return iter;
}

int list_or_vector_iter_is_last(struct list_or_vector_iter *iter) {
  if (!iter) {
    return 0;
  } else if (iter->type == LIST_ITER) {
    return is_empty(mal_cdr(iter->cell));
  } else {
    return iter->i+2 >= iter->len;
  }
}

MalType* list_or_vector_iter_get_obj(struct list_or_vector_iter *iter) {
  if (!iter) {
    return NULL;
  } else if (iter->type == LIST_ITER) {
    return iter->cell ? mal_car(iter->cell) : NULL;
  } else {
    return iter->i < iter->len ? mal_vector_ref(iter->cell, iter->i) : NULL;
  }
}

int list_or_vector_index_of(MalType *list, MalType *val) {
  MalType *item;
  struct list_or_vector_iter *iter;
  int index = 0;
  for (iter = list_or_vector_iter(list); iter; iter = list_or_vector_iter_next(iter)) {
    item = list_or_vector_iter_get_obj(iter);
    if (is_equal(item, val)) {
      return index;
    }
    index++;
  }
  return -1;
}

MalType* mal_sprintf(char *format, ...) {
  MalType *out = mal_string("");
  char c, c2;
  size_t len = strlen(format);
  va_list ap;
  va_start(ap, format);
  va_end(ap);
  for (size_t i=0; i<len; i++) {
    c = format[i];
    if (c == '%') {
      c2 = format[++i];
      switch (c2) {
        case 's':
          mal_string_append(out, va_arg(ap, char*));
          break;
        case 'S':
          mal_string_append_mal_string(out, va_arg(ap, MalType*));
          break;
        case 'i':
          mal_string_append(out, pr_str(mal_number(va_arg(ap, int)), 1));
          break;
        case 'z':
          mal_string_append(out, pr_str(mal_number(va_arg(ap, size_t)), 1));
          break;
        case 'p':
          mal_string_append(out, pr_str(mal_number((size_t)va_arg(ap, MalType*)), 1));
          break;
        case '%':
          mal_string_append_char(out, '%');
          break;
        default:
          printf("Unknown format specifier: %%%c", c2);
          exit(1);
      }
    } else {
      mal_string_append_char(out, c);
    }
  }
  return out;
}

int is_equal(MalType *arg1, MalType *arg2) {
  if (is_list_like(arg1) && is_list_like(arg2)) {
    return list_or_vector_is_equal(arg1, arg2);
  }
  if (arg1->type != arg2->type) {
    return 0;
  }
  switch (arg1->type) {
    case MAL_EMPTY_TYPE:
    case MAL_FALSE_TYPE:
    case MAL_NIL_TYPE:
    case MAL_TRUE_TYPE:
      return 1;
    case MAL_HASHMAP_TYPE:
      return hashmap_is_equal(arg1, arg2);
    case MAL_KEYWORD_TYPE:
      return strcmp(arg1->keyword, arg2->keyword) == 0;
    case MAL_NUMBER_TYPE:
      return arg1->number == arg2->number;
    case MAL_STRING_TYPE:
      return strcmp(arg1->str, arg2->str) == 0;
    case MAL_SYMBOL_TYPE:
      return strcmp(arg1->symbol, arg2->symbol) == 0;
    default:
      return 0;
  }
}

int list_or_vector_is_equal(MalType *arg1, MalType *arg2) {
  assert(is_list_like(arg1) && is_list_like(arg2));
  struct list_or_vector_iter *iter1 = list_or_vector_iter(arg1),
                             *iter2 = list_or_vector_iter(arg2);
  MalType *item1, *item2;
  if (!iter1 && !iter2) {
    return 1; // both empty
  }
  do {
    item1 = list_or_vector_iter_get_obj(iter1);
    item2 = list_or_vector_iter_get_obj(iter2);
    if (!item1 || !item2 || !is_equal(item1, item2)) {
      return 0;
    }
    iter1 = list_or_vector_iter_next(iter1);
    iter2 = list_or_vector_iter_next(iter2);
  } while(iter1 && iter2);
  if (iter1 || iter2) {
    return 0; // one of the lists still has items
  }
  return 1;
}

int hashmap_is_equal(MalType *map1, MalType *map2) {
  assert(is_hashmap(map1) && is_hashmap(map2));
  if (mal_hashmap_size(map1) != mal_hashmap_size(map2)) {
    return 0;
  }
  MalType *keys1 = mal_hashmap_keys_to_vector(map1);
  MalType *keys2 = mal_hashmap_keys_to_vector(map2);
  if (!list_or_vector_is_equal(keys1, keys2)) {
    return 0;
  }
  MalType *key, *val1, *val2;
  for (size_t i=0; i<keys1->vec_len; i++) {
    key = keys1->vec[i];
    val1 = mal_hashmap_get(map1, key);
    val2 = mal_hashmap_get(map2, key);
    if (!is_equal(val1, val2)) {
      return 0;
    }
  }
  return 1;
}

MalType* trampoline(MalType *result) {
  while (result->type == MAL_CONTINUATION_TYPE) {
    result = (result->fn)(result->env, result->argc, result->args);
  }
  return result;
}
