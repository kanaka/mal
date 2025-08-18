#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "linked_list.h"
#include "types.h"
#include "reader.h"
#include "printer.h"
#include "env.h"
#include "core.h"
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
MalType eval_if(list, Env*);
MalType eval_fnstar(list, const Env*);
MalType eval_do(list, Env*);

typedef MalType (*special_t)(list, Env*);
struct map* specials;

MalType READ(const char* str) {

  return read_str(str);
  // Implicit error propagation
}

Env* env_apply(MalClosure closure, list args) {
  //  Return the closure definition and update env if all went OK,
  //  else return an error.
  Env* fn_env = env_make(closure->env);
  MalType params = closure->fnstar_args->data;

  assert(type(params) & (MALTYPE_LIST | MALTYPE_VECTOR));
  seq_cursor c = seq_iter(params);
  list a = args;
  while (true) {
    if (!seq_cont(params, c)) {
      if (a) {
        make_error("'apply': expected %M, got [%N]", params, args);
      }
      break;
    }
    MalType parameter = seq_item(params, c);
    if (equal_forms(parameter, SYMBOL_AMPERSAND)) {
      c = seq_next(params, c);
      assert(seq_cont(params, c));
      env_set(fn_env, seq_item(params, c), make_list(a));
      break;
    }
    if (!a) {
      make_error("'apply': expected %M, got [%N]", params, args);
    }
    env_set(fn_env, parameter, a->data);
    c = seq_next(params, c);
    a = a->next;
  }
  return fn_env;
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
  check_type("apply phase", MALTYPE_CLOSURE | MALTYPE_FUNCTION, func);
  //  Evaluate the arguments
  list evlst = evaluate_list(lst, env);
  if (mal_error) return NULL;

  /* apply the first element of the list to the arguments */
  MalClosure closure;
  if ((closure = is_closure(func))) {

      return EVAL(closure->fnstar_args->next->data,
                  env_apply(closure, evlst));
      // Implicit error propagation
  }
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

//  Variant reporting errors during startup.
void re(const char *str, Env* env) {
    MalType a = READ(str);
    if (!mal_error) {
      EVAL(a, env);
      if (!mal_error) {
        return;
      }
    }
    MalType result = mal_error;
    mal_error = NULL; // before printing
    printf("Error during startup: %M\n", result);
    exit(EXIT_FAILURE);
}

int main() {

  types_init();
  printer_init();

  specials = map_empty();
  specials = hashmap_put(specials, SYMBOL_DEF,        eval_defbang);
  specials = hashmap_put(specials, SYMBOL_LET,        eval_letstar);
  specials = hashmap_put(specials, SYMBOL_IF,         eval_if);
  specials = hashmap_put(specials, SYMBOL_FN,         eval_fnstar);
  specials = hashmap_put(specials, SYMBOL_DO,         eval_do);

  Env* repl_env = env_make(NULL);

  ns core;
  size_t core_size;
  ns_make_core(&core, &core_size);
  while(core_size--) {
    const char* symbol = core[core_size].key;
    function_t function = core[core_size].value;
    env_set(repl_env, make_symbol(symbol), make_function(function));
  }

  /* add functions written in mal - not using rep as it prints the result */
  re("(def! not (fn* (a) (if a false true)))", repl_env);

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

MalType eval_if(list lst, Env* env) {

  if (!lst) {
    bad_arg_count("if", "two or three arguments", lst);
  }
  MalType raw_condition = lst->data;
  list l1 = lst->next;
  if (!l1) {
    bad_arg_count("if", "two or three arguments", lst);
  }
  MalType then_form = l1->data;
  list l2 = l1->next;
  MalType else_form;
  if (l2) {
    else_form = l2->data;
    if (l2->next) {
      bad_arg_count("if", "two or three arguments", lst);
    }
  }
  else {
    else_form = NULL;
  }

  MalType condition = EVAL(raw_condition, env);

  if (mal_error) {
    return NULL;
  }

  if (type(condition) & (MALTYPE_FALSE | MALTYPE_NIL)) {

    /* check whether false branch is present */
    if(else_form) {
      return EVAL(else_form, env);
    }
    else {
      return make_nil();
    }

  } else {
    return EVAL(then_form, env);
  }
}

MalType eval_do(list lst, Env* env) {

  /* handle empty 'do' */
  if (!lst) {
    return make_nil();
  }

  /* evaluate all but the last form */
  while (lst->next) {

    EVAL(lst->data, env);

    /* return error early */
    if (mal_error) {
      return NULL;
    }
    lst = lst->next;
  }
  /* return the last form for TCE evaluation */
  return EVAL(lst->data, env);
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

MalType eval_fnstar(list lst, const Env* env) {

  if (!lst || !lst->next || lst->next->next) {
    bad_arg_count("fn*", "two parameters", lst);
  }
  MalType parameters = lst->data;
  check_type("fn*", MALTYPE_LIST | MALTYPE_VECTOR, parameters);

  for (seq_cursor c = seq_iter(parameters);
       seq_cont(parameters, c);
       c = seq_next(parameters, c)) {

    MalType val = seq_item(parameters, c);

    if (!is_symbol(val)) {
      bad_type("fn*", MALTYPE_SYMBOL, val);
    }

    if (equal_forms(val, SYMBOL_AMPERSAND)) {
      c = seq_next(parameters, c);
      if (!val) {
        make_error("'fn*': no symbol after &: '%N'", lst);
      }
      val = seq_item(parameters, c);
      /* & is found and there is a single symbol after */
      check_type("fn*", MALTYPE_SYMBOL, val);
      /* & is found and there extra symbols after */
      c = seq_next(parameters, c);
      if (seq_cont(parameters, c)) {
        make_error("'fn*': extra symbols after &: '%N'", lst);
      }
      break;
    }
  }

  return make_closure(env, lst);
}

MalType apply(MalType fn, list args) {

  function_t fun_ptr;
  if ((fun_ptr = is_function(fn))) {

    return (*fun_ptr)(args);
    // Implicit error propagation
  }
  else {

    MalClosure closure = is_closure(fn);
    assert(closure);
    MalType ast = closure->fnstar_args->next->data;
    Env* env = env_apply(closure, args);
    if (mal_error) return NULL;
    return EVAL(ast, env);
    // Implicit error propagation
  }
}
