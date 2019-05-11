#include <assert.h>
#include <gc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hashmap.h"
#include "printer.h"
#include "types.h"
#include "util.h"

char *BLANK_LINE = "";

char* pr_str(MalType *val, int print_readably) {
  char *str;
  switch (val->type) {
    case MAL_ATOM_TYPE:
      return pr_atom(val, print_readably);
    case MAL_BLANK_LINE_TYPE:
      if (print_readably == 2) {
        return BLANK_LINE;
      } else {
        str = GC_MALLOC(1);
        str = 0;
        return str;
      }
    case MAL_CONS_TYPE:
      return pr_list(val, print_readably);
    case MAL_CONTINUATION_TYPE:
      return mal_sprintf("<continuation %p>", val)->str;
    case MAL_EMPTY_TYPE:
      return string("()");
    case MAL_FALSE_TYPE:
      return string("false");
    case MAL_HASHMAP_TYPE:
      return pr_hashmap(val, print_readably);
    case MAL_KEYWORD_TYPE:
      return mal_sprintf(":%s", val->keyword)->str;
    case MAL_LAMBDA_TYPE:
      return string("<lambda>");
    case MAL_NIL_TYPE:
      return string("nil");
    case MAL_NUMBER_TYPE:
      return long_long_to_string(val->number);
    case MAL_STRING_TYPE:
      return pr_string(val, print_readably);
    case MAL_SYMBOL_TYPE:
      return string(val->symbol);
    case MAL_TRUE_TYPE:
      return string("true");
    case MAL_VECTOR_TYPE:
      return pr_vector(val, print_readably);
    case MAL_ERROR_TYPE:
      printf("This shouldn't happen; pr_str() got mal_error(%s)\n", pr_str(val->error_val, 1));
      assert(0);
    default:
      printf("I don't know how to print this type: %d\n", val->type);
      exit(1);
  }
}

char* pr_atom(MalType *val, int print_readably) {
  return mal_sprintf("(atom %s)", pr_str(val->atom_val, print_readably))->str;
}

char* pr_list(MalType *val, int print_readably) {
  if (is_empty(val)) {
    return string("()");
  }
  char *item_str;
  MalType *str = mal_string("(");
  do {
    if (is_cons(val)) {
      item_str = pr_str(mal_car(val), print_readably);
      mal_string_append(str, item_str);
      mal_string_append_char(str, ' ');
    } else {
      mal_string_append(str, ". ");
      mal_string_append(str, pr_str(val, print_readably));
      mal_string_append_char(str, ' ');
      break;
    }
  } while (!is_empty(val = mal_cdr(val)));
  str->str[strlen(str->str) - 1] = ')';
  return str->str;
}

char* pr_vector(MalType *val, int print_readably) {
  size_t len = mal_vector_len(val);
  if (len == 0) {
    return string("[]");
  }
  MalType *str = mal_string("[");
  char *item_str;
  for(size_t i=0; i<len; i++) {
    item_str = pr_str(mal_vector_ref(val, i), print_readably);
    mal_string_append(str, item_str);
    if (i < len-1) mal_string_append_char(str, ' ');
  }
  mal_string_append_char(str, ']');
  return str->str;
}

char* pr_hashmap(MalType *val, int print_readably) {
  char *key_str, *val_str;
  if (mal_hashmap_size(val) == 0) {
    return string("{}");
  }
  MalType *str = mal_string("{");
  struct hashmap_iter *iter;
  for (iter = hashmap_iter(&val->hashmap); iter; iter = hashmap_iter_next(&val->hashmap, iter)) {
    key_str = (char*)hashmap_iter_get_key(iter);
    val_str = pr_str((MalType*)hashmap_iter_get_data(iter), print_readably);
    mal_string_append(str, key_str);
    mal_string_append_char(str, ' ');
    mal_string_append(str, val_str);
    mal_string_append_char(str, ' ');
  }
  str->str[strlen(str->str) - 1] = '}';
  return str->str;
}

char* pr_string(MalType *val, int print_readably) {
  assert(strlen(val->str) == val->str_len);
  if (print_readably) {
    size_t len = val->str_len;
    char *orig = val->str;
    MalType *repr = mal_string("\"");
    for (size_t i=0; i<len; i++) {
      switch (orig[i]) {
        case '\n':
          mal_string_append(repr, "\\n");
          break;
        case '"':
          mal_string_append(repr, "\\\"");
          break;
        case '\\':
          mal_string_append(repr, "\\\\");
          break;
        default:
          mal_string_append_char(repr, orig[i]);
      }
    }
    mal_string_append_char(repr, '"');
    return repr->str;
  } else {
    size_t len = val->str_len + 1;
    char *str = GC_MALLOC(len);
    snprintf(str, len, "%s", val->str);
    return str;
  }
}
