#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "linked_list.h"
#include "types.h"
#include "reader.h"
#include "printer.h"
#include "error.h"
#include "hashmap.h"
#include "readline.h"
#include "vector.h"

#define PROMPT_STRING "user> "

MalType apply(MalType, list); // For the apply phase and core apply/map/swap.
list evaluate_list(list, hashmap);
MalType evaluate_vector(vector_t, hashmap);
MalType evaluate_hashmap(hashmap, hashmap);

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

MalType EVAL(MalType ast, hashmap env) {

  /* printf("EVAL: %M\n", ast); */

  if (type(ast) == MALTYPE_SYMBOL) {
    MalType symbol_value = hashmap_get(env, ast);
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

void rep(const char* str, hashmap env) {

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

  struct map* repl_env = map_empty();

  repl_env = hashmap_put(repl_env, make_symbol("+"), make_function(mal_add));
  repl_env = hashmap_put(repl_env, make_symbol("-"), make_function(mal_sub));
  repl_env = hashmap_put(repl_env, make_symbol("*"), make_function(mal_mul));
  repl_env = hashmap_put(repl_env, make_symbol("/"), make_function(mal_div));

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

list evaluate_list(list lst, hashmap env) {

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

MalType evaluate_vector(vector_t lst, hashmap env) {
  int capacity = lst->count;
  struct vector* evlst = vector_new(capacity);
  for(int i = 0; i <= lst->count - 1; i++) {
    MalType new = EVAL(lst->nth[i], env);
    if (mal_error) return NULL;
    vector_append(&capacity, &evlst, new);
  }
  return make_vector(evlst);
}

MalType evaluate_hashmap(hashmap lst, hashmap env) {
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
