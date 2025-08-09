#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "linked_list.h"
#include "types.h"
#include "reader.h"
#include "printer.h"
#include "env.h"
#include "error.h"
#include "hashmap.h"
#include "readline.h"
#include "vector.h"

#define PROMPT_STRING "user> "

MalType apply(MalType, list); // For the apply phase and core apply/map/swap.
list evaluate_list(list, Env*);
MalType evaluate_vector(vector_t, Env*);
MalType evaluate_hashmap(hashmap lst, Env* env);
MalType eval_defbang(list, Env*);
MalType eval_letstar(list, Env*);

typedef MalType (*special_t)(list, Env*);
struct map* specials;

#define generic_arithmetic(name, op, iconst, fconst)      \
  MalType name(list args) {                               \
    explode2(#op, args, a1, a2);                          \
    long i1, i2;                                          \
    double f1, f2;                                        \
    if (is_integer(a1, &i1)) {                            \
      if (is_integer(a2, &i2)) return iconst(i1 op i2);   \
      if (is_float  (a2, &f2)) return fconst(i1 op f2);   \
      bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a2); \
    }                                                     \
    if (is_float(a1, &f1)) {                              \
      if (is_integer(a2, &i2)) return iconst(f1 op i2);   \
      if (is_float  (a2, &f2)) return fconst(f1 op f2);   \
      bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a2); \
    }                                                     \
    bad_type(#op, MALTYPE_INTEGER | MALTYPE_FLOAT, a1);   \
  }
generic_arithmetic(mal_add, +, make_integer, make_float)
generic_arithmetic(mal_sub, -, make_integer, make_float)
generic_arithmetic(mal_mul, *, make_integer, make_float)
generic_arithmetic(mal_div, /, make_integer, make_float)

MalType READ(const char* str) {

  return read_str(str);
  // Implicit error propagation
}

MalType EVAL(MalType ast, Env* env) {

  MalType dbgeval = env_get(env, SYMBOL_DEBUG_EVAL);
  if (dbgeval && (type(dbgeval) & ~(MALTYPE_FALSE | MALTYPE_NIL)))
    printf("EVAL: %50M env: %H\n", ast, env_as_map(env));

  if (type(ast) == MALTYPE_SYMBOL) {
    MalType symbol_value = env_get(env, ast);
    if (symbol_value)
      return symbol_value;
    else
      make_error("'%M' not found", ast);
  }

  vector_t vec;
  if ((vec = is_vector(ast))) {
    return evaluate_vector(vec, env);
    // Implicit error propagation
  }

  hashmap map;
  if ((map = is_hashmap(ast))) {
    return evaluate_hashmap(map, env);
    // Implicit error propagation
  }

  /* not a list */
  list lst;
  if (!is_list(ast, &lst)) { return ast; }

  /* empty list */
  if(lst == NULL) { return ast; }

  /* list */
  MalType first = lst->data;
  lst = lst->next;

    /* handle special symbols first */
  if (type(first) & MALTYPE_SYMBOL) {
    special_t special = hashmap_get(specials, first);
    if (special) {
      return special(lst, env);
    }
  }

  /* first element is not a special symbol */
  MalType func = EVAL(first, env);
  if (mal_error) { return NULL; }
  check_type("apply phase", MALTYPE_FUNCTION, func);
  //  Evaluate the arguments
  list evlst = evaluate_list(lst, env);
  if (mal_error) return NULL;

  /* apply the first element of the list to the arguments */
  return apply(func, evlst);
  // Implicit error propagation
}

void PRINT(MalType val) {

  printf("%M\n", val);
}

void rep(const char* str, Env* env) {

  MalType a = READ(str);
  if (!mal_error) {
    MalType b = EVAL(a, env);
    if (!mal_error) {
      PRINT(b);
      return;
    }
  }
  MalType e = mal_error;
  mal_error = NULL; // before printing
  printf("Uncaught error: %M\n", e);
}

int main() {

  types_init();
  printer_init();

  specials = map_empty();
  specials = hashmap_put(specials, SYMBOL_DEF,        eval_defbang);
  specials = hashmap_put(specials, SYMBOL_LET,        eval_letstar);

  Env* repl_env = env_make(NULL);

  env_set(repl_env, make_symbol("+"), make_function(mal_add));
  env_set(repl_env, make_symbol("-"), make_function(mal_sub));
  env_set(repl_env, make_symbol("*"), make_function(mal_mul));
  env_set(repl_env, make_symbol("/"), make_function(mal_div));

    const char* input;
    while((input = readline_gc(PROMPT_STRING))) {

      /* print prompt and get input*/
      /* Check for EOF (Ctrl-D) */

      /* call Read-Eval-Print */
      rep(input, repl_env);
    }
    printf("\n");

  return EXIT_SUCCESS;
}

MalType eval_defbang(list lst, Env* env) {

  explode2("def!", lst, defbang_symbol, defbang_value);

  MalType result = EVAL(defbang_value, env);
  if (mal_error) {
    return NULL;
  }
  check_type("def!", MALTYPE_SYMBOL, defbang_symbol);
  env_set(env, defbang_symbol, result);
  return result;
}

MalType eval_letstar(list lst, Env* env) {

  explode2("let*", lst, bindings, forms);

  check_type("let*", MALTYPE_LIST | MALTYPE_VECTOR, bindings);

  seq_cursor bindings_list = seq_iter(bindings);
  Env* letstar_env = env_make(env);

  /* evaluate the bindings */
  while(seq_cont(bindings, bindings_list)) {

    MalType symbol = seq_item(bindings, bindings_list);
    bindings_list = seq_next(bindings, bindings_list);
    if(!seq_cont(bindings, bindings_list)) {
      bad_arg_count("let*", "an even number of binding pairs",
                            bindings);
    }
    MalType value = EVAL(seq_item(bindings, bindings_list), letstar_env);

    /* early return from error */
    if (mal_error) {
      return NULL;
    }

    check_type("let*", MALTYPE_SYMBOL, symbol);
    env_set(letstar_env, symbol, value);
    bindings_list = seq_next(bindings, bindings_list);
  }

  return EVAL(forms, letstar_env);
}

list evaluate_list(list lst, Env* env) {

  list evlst = NULL;
  list* evlst_last = &evlst;
  while (lst) {

    MalType val = EVAL(lst->data, env);

    if (mal_error) {
      return NULL;
    }

    *evlst_last = list_push(NULL, val);
    evlst_last = &(*evlst_last)->next;
    lst = lst->next;
  }
  return evlst;
}

MalType evaluate_vector(vector_t lst, Env* env) {
  size_t capacity = lst->count;
  struct vector* evlst = vector_new(capacity);
  for (size_t i = 0; i < capacity; i++) {
    MalType new = EVAL(lst->nth[i], env);
    if (mal_error) return NULL;
    vector_append(&capacity, &evlst, new);
  }
  assert(evlst->count == capacity);
  return make_vector(evlst);
}

MalType evaluate_hashmap(hashmap lst, Env* env) {
  // map_empty() would be OK, but we know the size in advance and can
  // spare inefficient reallocations.
  struct map* evlst = map_copy(lst);
  for (map_cursor c = map_iter(lst); map_cont(lst, c); c = map_next(lst, c)) {
    MalType new = EVAL(map_val(lst, c), env);
    if (mal_error) return false;
    evlst = hashmap_put(evlst, map_key(lst, c), new);
  }
  return make_hashmap(evlst);
}

MalType apply(MalType fn, list args) {

  function_t fun_ptr = is_function(fn);
  assert(fun_ptr);

    return (*fun_ptr)(args);
    // Implicit error propagation
}
