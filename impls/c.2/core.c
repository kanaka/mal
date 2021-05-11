#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include <gc.h>

/* only needed for ffi */
#ifdef WITH_FFI
#include <dlfcn.h>
#include <ffi.h>
#endif

#include "libs/hashmap/hashmap.h"
#include "core.h"
#include "types.h"
#include "printer.h"
#include "reader.h"
#include "env.h"

#define STRING_BUFFER_SIZE 128

/* forward references to main file */
MalType* apply(MalType* fn, list args);

/* core ns functions */
MalType* mal_add(list);
MalType* mal_sub(list);
MalType* mal_mul(list);
MalType* mal_div(list);

MalType* mal_prn(list);
MalType* mal_println(list);
MalType* mal_pr_str(list);
MalType* mal_str(list);
MalType* mal_read_string(list);
MalType* mal_slurp(list);

MalType* mal_list(list);
MalType* mal_list_questionmark(list);
MalType* mal_empty_questionmark(list);
MalType* mal_count(list);
MalType* mal_cons(list);
MalType* mal_concat(list);
MalType* mal_nth(list);
MalType* mal_first(list);
MalType* mal_rest(list);

MalType* mal_equals(list);
MalType* mal_lessthan(list);
MalType* mal_lessthanorequalto(list);
MalType* mal_greaterthan(list);
MalType* mal_greaterthanorequalto(list);

MalType* mal_atom(list);
MalType* mal_atom_questionmark(list);
MalType* mal_deref(list);
MalType* mal_reset_bang(list);
MalType* mal_swap_bang(list);

MalType* mal_throw(list);
MalType* mal_apply(list);
MalType* mal_map(list);

MalType* mal_nil_questionmark(list);
MalType* mal_true_questionmark(list);
MalType* mal_false_questionmark(list);
MalType* mal_symbol_questionmark(list);
MalType* mal_keyword_questionmark(list);
MalType* mal_symbol(list);
MalType* mal_keyword(list);

MalType* mal_vec(list);
MalType* mal_vector(list);
MalType* mal_vector_questionmark(list);
MalType* mal_sequential_questionmark(list);
MalType* mal_hash_map(list);
MalType* mal_map_questionmark(list);
MalType* mal_assoc(list);
MalType* mal_dissoc(list);
MalType* mal_get(list);
MalType* mal_contains_questionmark(list);
MalType* mal_keys(list);
MalType* mal_vals(list);
MalType* mal_string_questionmark(list);
MalType* mal_number_questionmark(list);
MalType* mal_fn_questionmark(list);
MalType* mal_macro_questionmark(list);

MalType* mal_time_ms(list);
MalType* mal_conj(list);
MalType* mal_seq(list);
MalType* mal_meta(list);
MalType* mal_with_meta(list);

/* only needed for ffi */
#ifdef WITH_FFI
MalType* mal_dot(list);
#endif

ns* ns_make_core() {

  ns* core = GC_MALLOC(sizeof(*core));

  hashmap core_functions = NULL;

  /* arithmetic */
  core_functions = hashmap_put(core_functions, "+", mal_add);
  core_functions = hashmap_put(core_functions, "-", mal_sub);
  core_functions = hashmap_put(core_functions, "*", mal_mul);
  core_functions = hashmap_put(core_functions, "/", mal_div);

  /* strings */
  core_functions = hashmap_put(core_functions, "prn", mal_prn);
  core_functions = hashmap_put(core_functions, "pr-str", mal_pr_str);
  core_functions = hashmap_put(core_functions, "str", mal_str);
  core_functions = hashmap_put(core_functions, "println", mal_println);
  core_functions = hashmap_put(core_functions, "read-string", mal_read_string);

  /* files */
  core_functions = hashmap_put(core_functions, "slurp", mal_slurp);

  /* lists */
  core_functions = hashmap_put(core_functions, "list", mal_list);
  core_functions = hashmap_put(core_functions, "empty?", mal_empty_questionmark);
  core_functions = hashmap_put(core_functions, "count", mal_count);
  core_functions = hashmap_put(core_functions, "cons", mal_cons);
  core_functions = hashmap_put(core_functions, "concat", mal_concat);
  core_functions = hashmap_put(core_functions, "nth", mal_nth);
  core_functions = hashmap_put(core_functions, "first", mal_first);
  core_functions = hashmap_put(core_functions, "rest", mal_rest);

  /* predicates */
  core_functions = hashmap_put(core_functions, "=", mal_equals);
  core_functions = hashmap_put(core_functions, "<", mal_lessthan);
  core_functions = hashmap_put(core_functions, "<=", mal_lessthanorequalto);
  core_functions = hashmap_put(core_functions, ">", mal_greaterthan);
  core_functions = hashmap_put(core_functions, ">=", mal_greaterthanorequalto);

  core_functions = hashmap_put(core_functions, "list?", mal_list_questionmark);
  core_functions = hashmap_put(core_functions, "nil?", mal_nil_questionmark);
  core_functions = hashmap_put(core_functions, "true?", mal_true_questionmark);
  core_functions = hashmap_put(core_functions, "false?", mal_false_questionmark);
  core_functions = hashmap_put(core_functions, "symbol?", mal_symbol_questionmark);
  core_functions = hashmap_put(core_functions, "keyword?", mal_keyword_questionmark);
  core_functions = hashmap_put(core_functions, "vector?", mal_vector_questionmark);
  core_functions = hashmap_put(core_functions, "sequential?", mal_sequential_questionmark);
  core_functions = hashmap_put(core_functions, "map?", mal_map_questionmark);
  core_functions = hashmap_put(core_functions, "string?", mal_string_questionmark);
  core_functions = hashmap_put(core_functions, "number?", mal_number_questionmark);
  core_functions = hashmap_put(core_functions, "fn?", mal_fn_questionmark);
  core_functions = hashmap_put(core_functions, "macro?", mal_macro_questionmark);

  /* atoms */
  core_functions = hashmap_put(core_functions, "atom", mal_atom);
  core_functions = hashmap_put(core_functions, "atom?", mal_atom_questionmark);
  core_functions = hashmap_put(core_functions, "deref", mal_deref);
  core_functions = hashmap_put(core_functions, "reset!", mal_reset_bang);
  core_functions = hashmap_put(core_functions, "swap!", mal_swap_bang);

  /* other */
  core_functions = hashmap_put(core_functions, "throw", mal_throw);
  core_functions = hashmap_put(core_functions, "apply", mal_apply);
  core_functions = hashmap_put(core_functions, "map", mal_map);

  core_functions = hashmap_put(core_functions, "symbol", mal_symbol);
  core_functions = hashmap_put(core_functions, "keyword", mal_keyword);
  core_functions = hashmap_put(core_functions, "vec", mal_vec);
  core_functions = hashmap_put(core_functions, "vector", mal_vector);
  core_functions = hashmap_put(core_functions, "hash-map", mal_hash_map);

  /* hash-maps */
  core_functions = hashmap_put(core_functions, "contains?", mal_contains_questionmark);
  core_functions = hashmap_put(core_functions, "assoc", mal_assoc);
  core_functions = hashmap_put(core_functions, "dissoc", mal_dissoc);
  core_functions = hashmap_put(core_functions, "get", mal_get);
  core_functions = hashmap_put(core_functions, "keys", mal_keys);
  core_functions = hashmap_put(core_functions, "vals", mal_vals);

  /* misc */
  core_functions = hashmap_put(core_functions, "time-ms", mal_time_ms);
  core_functions = hashmap_put(core_functions, "conj", mal_conj);
  core_functions = hashmap_put(core_functions, "seq", mal_seq);
  core_functions = hashmap_put(core_functions, "meta", mal_meta);
  core_functions = hashmap_put(core_functions, "with-meta", mal_with_meta);

  /* only needed for ffi */
  #ifdef WITH_FFI
  core_functions = hashmap_put(core_functions, ".", mal_dot);
  #endif

  core->mappings = core_functions;
  return core;
}

/* core function definitons */

MalType* mal_add(list args) {
  /* Accepts any number of arguments */

  int return_float = 0;

  long i_sum = 0;
  double r_sum = 0.0;

  while(args) {

    MalType* val = args->data;
    if (!is_number(val)) {
      return make_error("'+': expected numerical arguments");
    }

    if (is_integer(val) && !return_float) {
      i_sum = i_sum + val->value.mal_integer;
    }
    else if (is_integer(val)) {
      r_sum = (double)i_sum + r_sum + val->value.mal_integer;
      i_sum = 0;
    }
    else {
      r_sum = (double)i_sum + r_sum + val->value.mal_float;
      i_sum = 0;
      return_float = 1;
    }
    args = args->next;
  }

  if (return_float) {
    return make_float(r_sum);
  } else {
    return make_integer(i_sum);
  }
}

MalType* mal_sub(list args) {
  /* Accepts any number of arguments */

  int return_float = 0;

  long i_sum = 0;
  double r_sum = 0.0;

  if (args) {

    MalType* val = args->data;
    args = args->next;

    if (!is_number(val)) {
      return make_error_fmt("'-': expected numerical arguments");
    }

    if (is_integer(val)) {
      i_sum = val->value.mal_integer;
    } else {
      r_sum = val->value.mal_float;
      return_float = 1;
    }

      while(args) {

      val = args->data;

      if (!is_number(val)) {
        return make_error_fmt("'-': expected numerical arguments");
      }

      if (is_integer(val) && !return_float) {
        i_sum = i_sum - val->value.mal_integer;
      }
      else if (is_integer(val)) {
        r_sum = (double)i_sum + r_sum - (double)val->value.mal_integer;
        i_sum = 0;
      }
      else {
        r_sum = (double)i_sum + r_sum - val->value.mal_float;
        i_sum = 0;
        return_float = 1;
      }
      args = args->next;
    }
  }

  if (return_float) {
    return make_float(r_sum);
  } else {
    return make_integer(i_sum);
  }
}


MalType* mal_mul(list args) {
  /* Accepts any number of arguments */

  int return_float = 0;

  long i_product = 1;
  double r_product = 1.0;

  while(args) {

    MalType* val = args->data;

    if (!is_number(val)) {
      return make_error_fmt("'*': expected numerical arguments");
    }

    if (is_integer(val) && !return_float) {
      i_product *= val->value.mal_integer;
    }
    else if (is_integer(val)) {
      r_product *= (double)val->value.mal_integer;
      r_product *= (double)i_product;
      i_product = 1;
    }
    else {
      r_product *= (double)i_product;
      r_product *= val->value.mal_float;
      i_product = 1;
      return_float = 1;
    }
    args = args->next;
  }

  if (return_float) {
    return make_float(r_product);
  } else {
    return make_integer(i_product);
  }
}

MalType* mal_div(list args) {
  /* Accepts any number of arguments */

  int return_float = 0;

  long i_product = 1;
  double r_product = 1.0;

  if (args) {
    MalType* val = args->data;

    if (!is_number(val)) {
      return make_error_fmt("'/': expected numerical arguments");
    }

    if (is_integer(val)) {
      i_product = val->value.mal_integer;
    } else {
      r_product = val->value.mal_float;
      return_float = 1;
    }

    args = args->next;

    while(args) {

      val = args->data;

      if (!is_number(val)) {
        return make_error_fmt("'/': expected numerical arguments");
      }

      /* integer division */
      if (is_integer(val) && !return_float) {
        i_product /= val->value.mal_integer;
      }
      /* promote integer to double */
      else if (is_integer(val)) {
        if (i_product != 1) {
          r_product = (double)i_product / (double)val->value.mal_integer;
          i_product = 1;
        } else {
          r_product /= (double)val->value.mal_integer;
        }
      }
      /* double division */
      else {
        return_float = 1;
        if (i_product != 1) {
          r_product = (double)i_product / val->value.mal_float;
          i_product = 1;
        } else {
          r_product /= val->value.mal_float;
        }
      }
      args = args->next;
    }
  }

  if (return_float) {
    return make_float(r_product);
  } else {
    return make_integer(i_product);
  }
}

MalType* mal_lessthan(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error_fmt("'<': expected exactly two arguments");
  }

  MalType* first_val = args->data;
  MalType* second_val = args->next->data;

  if (!is_number(first_val) || !is_number(second_val)) {
      return make_error_fmt("'<': expected numerical arguments");
  }

  int cmp = 0;

  if (is_integer(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_integer < second_val->value.mal_integer);
  }
  else if (is_integer(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_integer < second_val->value.mal_float);
  }
  else if (is_float(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_float < second_val->value.mal_integer);
  }
  else if (is_float(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_float < second_val->value.mal_float);
  }
  else {
    /* shouldn't happen unless new numerical type is added */
    return make_error_fmt("'<': unknown numerical type");
  }

  if (cmp) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_lessthanorequalto(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error_fmt("'<=': expected exactly two arguments");
  }

  MalType* first_val = args->data;
  MalType* second_val = args->next->data;

  if (!is_number(first_val) || !is_number(second_val)) {
      return make_error_fmt("'<=': expected numerical arguments");
  }

  int cmp = 0;

  if (is_integer(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_integer <= second_val->value.mal_integer);
  }
  else if (is_integer(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_integer <= second_val->value.mal_float);
  }
  else if (is_float(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_float <= second_val->value.mal_integer);
  }
  else if (is_float(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_float < second_val->value.mal_float);
  }
  else {
    /* shouldn't happen unless new numerical type is added */
    return make_error_fmt("'<=': unknown numerical type");
  }

  if (cmp) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_greaterthan(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error_fmt("'>': expected exactly two arguments");
  }

  MalType* first_val = args->data;
  MalType* second_val = args->next->data;

  if (!is_number(first_val) || !is_number(second_val)) {
      return make_error_fmt("'>': expected numerical arguments");
  }

  int cmp = 0;

  if (is_integer(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_integer > second_val->value.mal_integer);
  }
  else if (is_integer(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_integer > second_val->value.mal_float);
  }
  else if (is_float(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_float > second_val->value.mal_integer);
  }
  else if (is_float(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_float > second_val->value.mal_float);
  }
  else {
    /* shouldn't happen unless new numerical type is added */
    return make_error_fmt("'>': unknown numerical type");
  }

  if (cmp) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_greaterthanorequalto(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error_fmt("'>=': expected exactly two arguments");
  }

  MalType* first_val = args->data;
  MalType* second_val = args->next->data;

  if (!is_number(first_val) || !is_number(second_val)) {
      return make_error_fmt("'>=': expected numerical arguments");
  }

  int cmp = 0;

  if (is_integer(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_integer >= second_val->value.mal_integer);
  }
  else if (is_integer(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_integer >= second_val->value.mal_float);
  }
  else if (is_float(first_val) && is_integer(second_val)) {
    cmp = (first_val->value.mal_float >= second_val->value.mal_integer);
  }
  else if (is_float(first_val) && is_float(second_val)) {
    cmp = (first_val->value.mal_float >= second_val->value.mal_float);
  }
  else {
    /* shouldn't happen unless new numerical type is added */
    return make_error_fmt("'>=': unknown numerical type");
  }

  if (cmp) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_equals(list args) {
  /* Accepts any type of arguments */

  if (!args || !args->next || args->next->next) {
    return make_error_fmt("'=': expected exactly two arguments");
  }

  MalType* first_val = args->data;
  MalType* second_val = args->next->data;

  if (is_sequential(first_val) && is_sequential(second_val)) {
    return equal_lists(first_val, second_val);
  }
  else if (first_val->type != second_val->type) {
    return make_false();
  }
  else {

    switch(first_val->type) {

    case MALTYPE_INTEGER:

      if (first_val->value.mal_integer == second_val->value.mal_integer) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_FLOAT:

      if (first_val->value.mal_float == second_val->value.mal_float) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_SYMBOL:

      if (strcmp(first_val->value.mal_symbol, second_val->value.mal_symbol) == 0) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_STRING:
      if (strcmp(first_val->value.mal_string, second_val->value.mal_string) == 0) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_KEYWORD:
      if (strcmp(first_val->value.mal_keyword, second_val->value.mal_keyword) == 0) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_HASHMAP:
      return equal_hashmaps(first_val, second_val);
      break;

    case MALTYPE_TRUE:
    case MALTYPE_FALSE:
    case MALTYPE_NIL:

      return make_true();
      break;

    case MALTYPE_FUNCTION:

      if (first_val->value.mal_function == second_val->value.mal_function) {
        return make_true();
      } else {
        return make_false();
      }
      break;

    case MALTYPE_CLOSURE:

      if (&first_val->value.mal_closure == &second_val->value.mal_closure) {
        return make_true();
      } else {
        return make_false();
      }
      break;
    }
  }
  return make_false();
}

MalType* mal_list(list args) {
  /* Accepts any number and type of arguments */
  return make_list(args);
}

MalType* mal_nth(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error("'nth': Expected exactly two arguments");
  }

  MalType* lst = args->data;
  MalType* n = args->next->data;

  if (!is_sequential(lst)) {
    return make_error_fmt("'nth': first argument is not a list or vector: '%s'\n", pr_str(lst, UNREADABLY));
  }

  if (!is_integer(n)) {
    return make_error_fmt("'nth': second argument is not an integer: '%s'\n", pr_str(lst, UNREADABLY));
  }

  MalType* result = list_nth(lst->value.mal_list, n->value.mal_integer);

  if (result) {
    return result;
  }
  else {
    return make_error_fmt("'nth': index %s out of bounds for: '%s'\n", \
                          pr_str(n, UNREADABLY), pr_str(lst, UNREADABLY));
  }
}

MalType* mal_first(list args) {

  if (!args || args->next) {
    return make_error("'first': expected exactly one argument");
  }

  MalType* lst = args->data;

  if (!is_sequential(lst) && !is_nil(lst)) {
    return make_error("'first': expected a list or vector");
  }

  MalType* result = list_first(lst->value.mal_list);

  if (result) {
    return result;
  }
  else {
    return make_nil();
  }
}

MalType* mal_rest(list args) {

  if (!args || args->next) {
    return make_error("'rest': expected exactly one argument");
  }

  MalType* lst = args->data;

  if (!is_sequential(lst) && !is_nil(lst)) {
    return make_error("'rest': expected a list or vector");
  }

  list result = list_rest(lst->value.mal_list);

  if (lst) {
    return make_list(result);
  }
  else {
    return make_nil();
  }
}


MalType* mal_cons(list args) {

  if (!args || (args->next && args->next->next)) {
    return make_error("'cons': Expected exactly two arguments");
  }

  MalType* lst = args->next->data;
  if (is_sequential(lst)) {
     return make_list(list_push(lst->value.mal_list, args->data));
  }
  else if (is_nil(lst)) {
    return make_list(list_push(NULL, args->data));
  }
  else {
    return make_error_fmt("'cons': second argument is not a list or vector: '%s'\n", \
                          pr_str(lst, UNREADABLY));
  }
}

MalType* mal_concat(list args) {

  /* return an empty list for no arguments */
  if (!args) {
    return make_list(NULL);
  }

  list new_list = NULL;
  while (args) {

    MalType* val = args->data;

    /* skip nils */
    if (is_nil(val)) {
      args = args->next;
      continue;
    }
    /* concatenate lists and vectors */
    else if (is_sequential(val)) {

      list lst = val->value.mal_list;
      new_list = list_concatenate(new_list, lst);
      args = args->next;
    }
    /* raise an error for any non-sequence types */
    else {
      return make_error_fmt("'concat': all arguments must be lists or vectors '%s'", \
                            pr_str(val, UNREADABLY));
    }
  }
  return make_list(new_list);
}

MalType* mal_count(list args) {

  if (args->next) {
    return make_error_fmt("'count': too many arguments");
  }

  MalType* val = args->data;
  if (!is_sequential(val) && !is_nil(val)) {
        return make_error_fmt("'count': argument is not a list or vector: '%s'\n", \
                              pr_str(val, UNREADABLY));
  }
  return make_integer(list_count(val->value.mal_list));
}


MalType* mal_list_questionmark(list args) {

  if (args->next) {
    return make_error_fmt("'list?': too many arguments");
  }

  MalType* val = args->data;
  if (is_list(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_empty_questionmark(list args) {

  if (args->next) {
    return make_error_fmt("'empty?': too many arguments");
  }

  MalType* val = args->data;
  if (!is_sequential(val)) {
    return make_error_fmt("'empty?': argument is not a list or vector: '%s'\n", pr_str(val, UNREADABLY));
  }

  if (!val->value.mal_list) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_pr_str(list args) {
  /* Accepts any number and type of arguments */
  return as_str(args, READABLY, " ");
}

MalType* mal_str(list args) {
  /* Accepts any number and type of arguments */
  return as_str(args, UNREADABLY, "");
}

MalType* mal_prn(list args) {
  /* Accepts any number and type of arguments */
   return print(args, READABLY, " ");
}

MalType* mal_println(list args) {
  /* Accepts any number and type of arguments */
  return print(args, UNREADABLY, " ");
}

MalType* mal_read_string(list args) {

  if (!args || args->next) {
    return make_error_fmt("'read-string': expected exactly one argument");
  }

  MalType* val = args->data;
  if (!is_string(val)) {
    return make_error_fmt("'read-string': expected a string argument '%s'", pr_str(val, UNREADABLY));
  }
  return read_str(val->value.mal_string);
}

MalType* mal_slurp(list args) {

  if (args->next) {
    return make_error_fmt("'slurp': too many arguments");
  }

  MalType* filename = args->data;
  if (!is_string(filename)) {
    return make_error_fmt("'slurp': expected a string argument");
  }

  long file_length = 0;
  FILE* file = fopen(filename->value.mal_string, "rb");

  if (!file){
    return make_error_fmt("'slurp': file not found '%s'", pr_str(filename, UNREADABLY));
  }

  fseek(file, 0, SEEK_END);
  file_length = ftell(file);
  fseek(file, 0, SEEK_SET);

  char* buffer = (char*)GC_MALLOC(sizeof(*buffer) * file_length + 1);
  if (file_length != fread(buffer, sizeof(*buffer), file_length, file)) {
    return make_error_fmt("'slurp': failed to read file '%s'", pr_str(filename, UNREADABLY));
  }

  fclose(file);

  buffer[file_length] = '\0';
  return make_string(buffer);
}

MalType* mal_atom(list args) {

  if (!args || args->next) {
    return make_error_fmt("'atom': expected exactly one argument");
  }

  MalType* val = args->data;
  return make_atom(val);
}

MalType* mal_atom_questionmark(list args) {

  if (!args || args->next) {
    return make_error_fmt("'atom?': expected exactly one argument");
  }

  MalType* val = args->data;

  if (is_atom(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_deref(list args) {

  if (!args || args->next) {
    return make_error_fmt("'deref': expected exactly one argument");
  }

  MalType* val = args->data;

  if (!is_atom(val)) {
    return make_error_fmt("'deref': value is not an atom '%s'", pr_str(val, UNREADABLY));
  }

  return val->value.mal_atom;
}

MalType* mal_reset_bang(list args) {

  if (!args || args->next->next) {
    return make_error_fmt("'reset!': expected exactly two arguments");
  }

  MalType* val = args->data;

  if (!is_atom(val)) {
    return make_error_fmt("'reset!': value is not an atom '%s'", pr_str(val, UNREADABLY));
  }

  val->value.mal_atom = args->next->data;
  return args->next->data;
}

MalType* mal_swap_bang(list args) {

  MalType* val = args->data;

  if (!is_atom(val)) {
    return make_error_fmt("'swap!': first argument is not an atom '%s'", pr_str(val, UNREADABLY));
  }

  MalType* fn = args->next->data;

  if (!is_callable(fn)) {
    return make_error_fmt("'swap!': second argument is not callable '%s'", pr_str(fn, UNREADABLY));
  }

  list fn_args = args->next->next;
  fn_args = list_push(fn_args, val->value.mal_atom);

  MalType* result = apply(fn, fn_args);

  if (is_error(result)) {
    return result;
  }
  else {
    val->value.mal_atom = result;
    return result;
  }
}

MalType* mal_throw(list args) {

  if (!args || args->next) {
    return make_error_fmt("'throw': expected exactly one argument");
  }

  MalType* val = args->data;

  /* re-throw an existing exception */
  if (is_error(val)) {
    return val;
  }
  /* create a new exception */
  else {
    return wrap_error(val);
  }
}

MalType* mal_apply(list args) {

  if (!args || !args->next) {
    return make_error("'apply': expected at least two arguments");
  }

  MalType* func = args->data;

  if (!is_callable(func)) {
    return make_error("'apply': first argument must be callable");
  }

  /* assemble loose arguments */
  args = args->next;
  list lst = NULL;
  while(args->next) {
    lst = list_push(lst, args->data);
    args = args->next;
  }

  MalType* final = args->data;

  if (is_sequential(final)) {
    lst = list_concatenate(list_reverse(lst), final->value.mal_list);
  }
  else {
    lst = list_push(lst, final);
    lst = list_reverse(lst);
  }

  return apply(func, lst);
}

MalType* mal_map(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error("'map': expected two arguments");
  }

  MalType* func = args->data;

  if (!is_callable(func)) {
    return make_error("'map': first argument must be a function");
  }

  MalType* arg = args->next->data;

  if (!is_sequential(arg)) {
    return make_error("'map': second argument must be a list or vector");
  }

  list arg_list = arg->value.mal_list;
  list result_list = NULL;

  while(arg_list) {

    MalType* result = apply(func, list_make(arg_list->data));

    /* early return if error */
    if (is_error(result)) {
      return result;
    }
    else {
      result_list = list_push(result_list, result);
    }
    arg_list = arg_list->next;
  }
  return make_list(list_reverse(result_list));
}

MalType* mal_nil_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'nil?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_nil(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_true_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'true?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_true(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_false_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'false?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_false(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_symbol_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'symbol?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_symbol(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_symbol(list args) {

  if (!args || args->next) {
    return make_error("'symbol': expected a single argument");
  }

  MalType* val = args->data;

  if (!is_string(val)) {
    return make_error("'symbol': expected a string argument");
  }
  else {
    return make_symbol(val->value.mal_string);
  }
}

MalType* mal_keyword(list args) {

  if (!args || args->next) {
    return make_error("'keyword': expected a single argument");
  }

  MalType* val = args->data;

  if (!is_string(val) && !is_keyword(val)) {
    return make_error("'keyword': expected a string argument");
  }
  else {
    return make_keyword(val->value.mal_string);
  }
}

MalType* mal_keyword_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'keyword?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_keyword(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_vec(list args) {

  /* Accepts a single argument */

  if (!args || args->next) {
    return make_error("'vec': expected a single argument");
  }

  MalType* val = args->data;

  if (!is_vector(val) && !is_list(val) && !is_hashmap(val)) {
    return make_error("'vec': expected a vector, list or hashmap");
  }

  MalType* new_val = copy_type(val);
  new_val->type = MALTYPE_VECTOR;

  return new_val;
}

MalType* mal_vector(list args) {
  /* Accepts any number and type of arguments */
  return make_vector(args);
}

MalType* mal_vector_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'vector?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_vector(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_sequential_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'sequential?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_sequential(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_hash_map(list args) {

  if (args && list_count(args) % 2 == 1) {
    return make_error("'hashmap': odd number of arguments, expected key/value pairs");
  }

  list args_iterator = args;
  while (args_iterator) {

    MalType* val = args_iterator->data;

    if (!is_keyword(val) && !is_string(val) && !is_symbol(val)) {
      return make_error("'hashmap': keys must be keywords, symbols or strings");
    }
    args_iterator = args_iterator->next;
    args_iterator = args_iterator->next;
  }

  return make_hashmap(args);
}

MalType* mal_map_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'map?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_hashmap(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}


MalType* mal_get(list args) {
  /* TODO: implement a proper hashmap */

  if (!args || args->next->next) {
    return make_error("'get': expected exactly two arguments");
  }

  MalType* map = args->data;

  if (!is_hashmap(map) && !is_nil(map)) {
    return make_error("'get': expected a map for the first argument");
  }

  MalType* result = hashmap_getf(map->value.mal_list, get_fn(args->next->data), get_fn);

  if (!result) {
    return make_nil();
  }

  return result;
}

MalType* mal_contains_questionmark(list args) {

  if (!args || args->next->next) {
    return make_error("'contains?': expected exactly two arguments");
  }

  MalType* map = args->data;

  if (!is_hashmap(map)) {
    return make_error("'contains?': expected a map for the first argument");
  }

  MalType* result = hashmap_getf(map->value.mal_list, get_fn(args->next->data), get_fn);

  if (!result) {
    return make_false();
  }
  else {
    return make_true();
  }
}

MalType* mal_assoc(list args) {

  if (!args || !args->next || !args->next->next) {
    return make_error("'assoc': expected at least three arguments");
  }

  MalType* map = args->data;

  if (!is_hashmap(map)) {
    return make_error("'assoc': expected a map for the first argument");
  }

  if (list_count(args->next)%2 != 0) {
    return make_error("'assoc': expected even number of key/value pairs");
  }


  list new_lst = list_reverse(list_copy(map->value.mal_list));
  args = args->next;

  while (args) {

    /* try to update copy in-place */
    hashmap result = hashmap_updatef(new_lst, get_fn(args->data), args->next->data, get_fn);

    if (result) {
      new_lst = result;
    }
    /* add a new key/value pair */
    else {
      new_lst = list_push(new_lst,args->next->data);
      new_lst = list_push(new_lst,args->data);
    }
    args = args->next->next;
  }
  return make_hashmap(new_lst);
}

MalType* mal_dissoc(list args) {

  if (!args || !args->next) {
    return make_error("'dissoc': expected at least two arguments");
  }

  MalType* map = args->data;

  if (!is_hashmap(map)) {
    return make_error("'dissoc': expected a map for the first argument");
  }

  list source_list = map->value.mal_list;
  list new_list = NULL;
  args = args->next;

  while(source_list) {

    list dis_args = args;
    long dis = 0;


    while(dis_args) {

      list tmp = NULL;
      tmp = list_push(tmp, source_list->data);
      tmp = list_push(tmp, dis_args->data);
      MalType* cmp = mal_equals(tmp);

      if (is_true(cmp)) {
        dis = 1;
        break;
      }
      dis_args = dis_args->next;
    }

    if (!dis) {
      new_list = list_push(new_list, source_list->data);
      new_list = list_push(new_list, source_list->next->data);
    }
    source_list = source_list->next->next;
  }

  return make_hashmap(list_reverse(new_list));
}


MalType* mal_keys(list args) {

  if (!args || args->next) {
    return make_error("'keys': expected exactly one argument");
  }

  MalType* map = args->data;

  if (!is_hashmap(map)) {
    return make_error("'keys': expected a map");
  }

  list lst = map->value.mal_list;
  if (!lst) {
    return make_list(NULL);
  }

  list result = list_make(lst->data);
  while(lst->next->next) {

    lst = lst->next->next;
    result = list_push(result, lst->data);
  }
  return make_list(result);
}

MalType* mal_vals(list args) {

  if (!args || args->next) {
    return make_error("'vals': expected exactly one argument");
  }

  MalType* map = args->data;

  if (!is_hashmap(map)) {
    return make_error("'vals': expected a map");
  }

  list lst = map->value.mal_list;
  if (!lst) {
    return make_list(NULL);
  }

  lst = lst->next;
  list result = list_make(lst->data);
  while(lst->next) {

    lst = lst->next->next;
    result = list_push(result, lst->data);
  }
  return make_list(result);
}

MalType* mal_string_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'string?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_string(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}


MalType* mal_number_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'number?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_number(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}


MalType* mal_fn_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'fn?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_callable(val) && !is_macro(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}

MalType* mal_macro_questionmark(list args) {

  if (!args || args->next) {
    return make_error("'macro?': expected a single argument");
  }

  MalType* val = args->data;

  if (is_macro(val)) {
    return make_true();
  }
  else {
    return make_false();
  }
}


MalType* mal_time_ms(list args) {

  struct timeval tv;
  gettimeofday(&tv, NULL);
  long ms = tv.tv_sec * 1000 + tv.tv_usec/1000.0 + 0.5;

  return make_float(ms);
}


MalType* mal_conj(list args) {

  if (!args || !args->next) {
    return make_error("'conj': Expected at least two arguments");
  }

  MalType* lst = args->data;

  if (!is_sequential(lst)) {
    return make_error_fmt("'conj': first argument is not a list or vector: '%s'\n", \
                          pr_str(lst, UNREADABLY));
  }

  list rest = args->next;

  if (is_list(lst)) {

    list new_lst = list_reverse(list_copy(lst->value.mal_list));

    while(rest) {
      new_lst = list_push(new_lst, rest->data);
      rest = rest->next;
  }
    return make_list(new_lst);
  }
  else /* is_vector(lst) */ {

    list new_lst = list_copy(lst->value.mal_list);

    while(rest) {
      new_lst = list_push(new_lst, rest->data);
      rest = rest->next;
    }
    return make_vector(list_reverse(new_lst));
  }
}

MalType* mal_seq(list args) {

  if (!args || args->next) {
    return make_error("'seq': expected exactly one argument");
  }

  MalType* val = args->data;

  if (is_sequential(val)) {

    /* empy list or vector */
    if (!val->value.mal_list) {
      return make_nil();
    }
    else {
      return make_list(val->value.mal_list);
    }
  }
  else if (is_string(val)) {

    /* empty string */
    if (*(val->value.mal_string) == '\0') {
      return make_nil();
    }
    else {

      char* ch = val->value.mal_string;
      list lst = NULL;

      while(*ch != '\0') {
        char* new_ch = GC_MALLOC(sizeof(*new_ch));
        strncpy(new_ch, ch, 1);

        lst = list_push(lst, make_string(new_ch));
        ch++;
      }
      return make_list(list_reverse(lst));
    }
  }
  else if (is_nil(val)) {
    return make_nil();
  }
  else {
    return make_error("'seq': expected a list, vector or string");
  }
}

MalType* mal_meta(list args) {

  if (!args || args->next) {
    return make_error("'meta': expected exactly one argument");
  }

  MalType* val = args->data;

  if (!is_sequential(val) && !is_hashmap(val) && !is_callable(val)) {
    return make_error("'meta': metadata not supported for data type");
  }

  if (!val->metadata) {
    return make_nil();
  } else {
    return val->metadata;
  }
}

MalType* mal_with_meta(list args) {

  if (!args || !args->next || args->next->next) {
    return make_error("'with-meta': expected exactly two arguments");
  }

  MalType* val = args->data;

  if (!is_sequential(val) && !is_hashmap(val) && !is_callable(val)) {
    return make_error("'with-meta': metadata not supported for data type");
  }

  MalType* metadata = args->next->data;

  MalType* new_val = copy_type(val);
  new_val->metadata = metadata;

  return new_val;
}


/* helper functions */

MalType* as_str(list args, int readably, char* separator) {

  long buffer_length = STRING_BUFFER_SIZE;
  long separator_length = strlen(separator);
  char* buffer = GC_MALLOC(sizeof(*buffer) * STRING_BUFFER_SIZE);
  long char_count = 0;

  while(args) {

    MalType* arg = args->data;
    char* str = pr_str(arg, readably);
    int len = strlen(str);

    char_count += len;
    char_count += separator_length;
    if (char_count >= buffer_length) {
      buffer = GC_REALLOC(buffer, sizeof(*buffer) * char_count + 1);
    }

    strncat(buffer, str, char_count);
    args = args->next;

    if (args) {
      strcat(buffer, separator);
    }
  }
  return make_string(buffer);
}

MalType* print(list args, int readably, char* separator) {

  while(args) {

    printf("%s", pr_str(args->data, readably));
    args = args->next;

    if (args) {
      printf("%s", separator);
    }
  }
  printf("\n");

  return make_nil();
}

MalType* equal_lists(MalType* list1, MalType* list2) {

  list first = list1->value.mal_list;
  list second = list2->value.mal_list;

  if (list_count(first) != list_count(second)) {
    return make_false();
  }
  else {

    while(first && second) {

      list args = NULL;
      args = list_push(args, second->data);
      args = list_push(args, first->data);

      MalType* cmp = mal_equals(args);

      if (is_false(cmp)) {
        return make_false();
        break;
      }
      first = first->next;
      second = second->next;
    }
    return make_true();
  }
}

MalType* equal_hashmaps(MalType* map1, MalType* map2) {

  list first = map1->value.mal_list;
  list second = map2->value.mal_list;

  if (!first && !second) {
    return make_true();
  }

  if (list_count(first) != list_count(second)) {
    return make_false();
  }

  while (first) {

    MalType* key1 = first->data;
    MalType* val1 = first->next->data;
    MalType* val2 = hashmap_getf(second, get_fn(key1), get_fn);

    if (!val2) {
      return make_false();
    }

    list args = NULL;
    args = list_push(args, val1);
    args = list_push(args, val2);

    MalType* cmp = mal_equals(args);

    if (is_false(cmp)) {
      return make_false();
      break;
    }
    first = first->next->next;
  }
  return make_true();
}

/* helper function for get */
char* get_fn(gptr data) {

  MalType* val = data;

  switch (val->type) {

  case MALTYPE_STRING:

    return (val->value.mal_string);
    break;

  case MALTYPE_SYMBOL:

    return (val->value.mal_symbol);
    break;

  case MALTYPE_KEYWORD:

    return (val->value.mal_keyword);
    break;

  default:
    return NULL;
  }
}

#ifdef WITH_FFI
MalType* mal_dot(list args) {

  /* (. "lib" "return type" "function" "arg1 type" "arg 1" ...) */

  if (!args || !args->next || !args->next->next) {
    return make_error("'.': expected at least three arguments");
  }

  MalType* lib_name = (MalType*)args->data;

  if (!is_string(lib_name) && !is_nil(lib_name)) {
    return make_error("'.': expected library name or nil for first argument");
  }

  MalType* return_type = (MalType*)args->next->data;

  if (!is_string(return_type)) {
    return make_error("'.': expected string (return type) for second argument");
  }

  MalType* fn_name = (MalType*)args->next->next->data;

  if (!is_string(fn_name)) {
    return make_error("'.': expected string (function name) for third argument");
  }

  int args_count = list_count(args) - 3;

  if (args_count % 2 == 1) {
    return make_error("'.': expected even number of argument types and values");
  }

  list arg_types_list = NULL;
  list arg_vals_list = NULL;

  args = args->next->next->next;
  while(args) {

    MalType* val_type = (MalType*)args->data;
    MalType* val = (MalType*)args->next->data;

    if (!is_string(val_type))  {
      return make_error_fmt("'.': expected strings for argument types: '%s'", pr_str(val_type, UNREADABLY));
    }

    arg_types_list = list_push(arg_types_list, val_type);
    arg_vals_list = list_push(arg_vals_list, val);

    args = args->next->next;
  }

  arg_types_list = list_reverse(arg_types_list);
  arg_vals_list = list_reverse(arg_vals_list);

  /* open a shared library dynamically and get hold of a function */
  gptr lib_handle;
  if (!is_nil(lib_name)) {
    lib_handle = dlopen(lib_name->value.mal_string, RTLD_LAZY);
  } else {
    lib_handle = dlopen(NULL, RTLD_LAZY);
  }

  if (!lib_handle) {
    return make_error_fmt("'ffi`' reports: %s", dlerror());
  }

  gptr fn = dlsym(lib_handle, fn_name->value.mal_string);

  char* error;
  if ((error = dlerror()) != NULL) {
    return make_error_fmt("'ffi' dlsym could not get handle to function '%s': %s", fn_name->value.mal_string, error);
  }

  /* use libffi to call function */

  ffi_cif cif;
  ffi_type* ret_type;
  ffi_type* arg_types[20];
  void* arg_vals[20];
  ffi_status status;
  ffi_type* ffi_get_type(char *type, MalType* err);

  MalType* mal_err = make_nil();

  /* set return type */
  MalType* make_type(char *type);
  MalType* retval = make_type(return_type->value.mal_string);

  ret_type = ffi_get_type(return_type->value.mal_string, mal_err);
  if (is_error(mal_err)) { return mal_err; }

  int arg_count = list_count(arg_types_list);

  /* Set the argument types and values */
  for (int i = 0; i < arg_count; i++) {

    MalType* val_type = (MalType*)arg_types_list->data;
    arg_types[i] = ffi_get_type(val_type->value.mal_string, mal_err);
    if (is_error(mal_err)) { return mal_err; }

    MalType* val = (MalType*)arg_vals_list->data;
    arg_vals[i] = &(val->value);

    arg_types_list = arg_types_list->next;
    arg_vals_list = arg_vals_list->next;
  }

  /* perform the call */
  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arg_count, ret_type, arg_types);

  if (status != FFI_OK) {
    return make_error_fmt("'ffi' call to ffi_prep_cif failed with code: %d\n", status);
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
ffi_type* ffi_get_type(char *type, MalType* err) {

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
    err = make_error_fmt("'ffi' type not recognised '%'", type);
    return NULL;
  }
}

/* helper function for ffi */
MalType* make_type(char *type) {

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
    return make_error_fmt("'ffi' type not supported '%s'", type);
  }
}
#endif
