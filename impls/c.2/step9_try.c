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
MalType eval_letstar(list, Env**); // TCO
MalType eval_if(list, Env*); // TCO (even for nil)
MalType eval_fnstar(list, const Env*);
MalType eval_do(list, Env*); // TCO in the same env
MalType eval_quote(list);
MalType eval_quasiquote(list);
MalType quasiquote(MalType);
MalType quasiquote_vector(vector_t);
MalType quasiquote_list(list);
MalType quasiquote_folder(MalType first, MalType qq_rest);
MalType eval_defmacrobang(list, Env*);
MalType eval_try(list, Env**); // TCO unless the environment is set to NULL.

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
        make_error("'apply': expected [%M], got [%N]", params, args);
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
      make_error("'apply': expected [%M], got [%N]", params, args);
    }
    env_set(fn_env, parameter, a->data);
    c = seq_next(params, c);
    a = a->next;
  }
  return fn_env;
}

MalType EVAL(MalType ast, Env* env) {

  /* Use goto to jump here rather than calling eval for tail-call elimination */
 TCE_entry_point:

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
    if (equal_forms(first, SYMBOL_DEF)) {
      return eval_defbang(lst, env);
      // Implicit error propagation
    }
    else if (equal_forms(first, SYMBOL_LET)) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_letstar(lst, &env);

      if (mal_error) { return NULL; }
      goto TCE_entry_point;
    }
    else if (equal_forms(first, SYMBOL_IF)) {

      /* TCE - modify ast directly and jump back to eval */
      ast = eval_if(lst, env);

      if (mal_error) { return NULL; }
      goto TCE_entry_point;
    }
    else if (equal_forms(first, SYMBOL_FN)) {
      return eval_fnstar(lst, env);
      // Implicit error propagation
    }
    else if (equal_forms(first, SYMBOL_DO)) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_do(lst, env);

      if (mal_error) { return NULL; }
      goto TCE_entry_point;
    }
    else if (equal_forms(first, SYMBOL_QUOTE)) {
      return eval_quote(lst);
      // Implicit error propagation
    }
    else if (equal_forms(first, SYMBOL_QUASIQUOTE)) {

      ast = eval_quasiquote(lst);

      if (mal_error) { return NULL; }
      goto TCE_entry_point;
    }
    else if (equal_forms(first, SYMBOL_DEFMACRO)) {
      return eval_defmacrobang(lst, env);
      // Implicit error propagation
    }
    else if (equal_forms(first, SYMBOL_TRY)) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_try(lst, &env);
      if (mal_error) return NULL;

      if(!env) { return ast; }
      goto TCE_entry_point;
    }

  /* first element is not a special symbol */
  MalType func = EVAL(first, env);
  if (mal_error) { return NULL; }
  check_type("apply phase", MALTYPE_CLOSURE | MALTYPE_FUNCTION | MALTYPE_MACRO, func);
  if (type(func) == MALTYPE_MACRO) {
    ast = apply(func, lst);
    if (mal_error) { return NULL; }
    goto TCE_entry_point;
  }
  //  Evaluate the arguments
  list evlst = evaluate_list(lst, env);
  if (mal_error) return NULL;

  /* apply the first element of the list to the arguments */
  MalClosure closure;
  if ((closure = is_closure(func))) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = closure->fnstar_args->next->data;
      env = env_apply(closure, evlst);

      if (mal_error) return NULL;
      goto TCE_entry_point;
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

/* declare as global so it can be accessed by mal_eval */
Env* repl_env;

MalType mal_eval(list args) {

  explode1("eval", args, ast);
  return EVAL(ast, repl_env);
  // Implicit error propagation
}

int main(int argc, char** argv) {

  types_init();
  printer_init();

  repl_env = env_make(NULL);

  ns core;
  size_t core_size;
  ns_make_core(&core, &core_size);
  while(core_size--) {
    const char* symbol = core[core_size].key;
    function_t function = core[core_size].value;
    env_set(repl_env, make_symbol(symbol), make_function(function));
  }

  env_set(repl_env, make_symbol("eval"), make_function(mal_eval));

  /* add functions written in mal - not using rep as it prints the result */
  re("(def! not (fn* (a) (if a false true)))", repl_env);
  re("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env);
  re("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env);

  /* make command line arguments available in the environment */
  list lst = NULL;
  while(1 < --argc) {
    lst = list_push(lst, make_string(argv[argc]));
  }
  env_set(repl_env, make_symbol("*ARGV*"), make_list(lst));

  /* run in script mode if a filename is given */
  if (argc) {

    /* first argument on command line is filename */
    const char* load_command = mal_printf("(load-file \"%s\")", argv[1]);
    re(load_command, repl_env);
  }
  /* run in repl mode when no cmd line args */
  else {

    const char* input;
    while((input = readline_gc(PROMPT_STRING))) {

      /* print prompt and get input*/
      /* Check for EOF (Ctrl-D) */

      /* call Read-Eval-Print */
      rep(input, repl_env);
    }
    printf("\n");
  }
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

MalType eval_letstar(list lst, Env** env) {

  explode2("let*", lst, bindings, forms);

  check_type("let*", MALTYPE_LIST | MALTYPE_VECTOR, bindings);

  seq_cursor bindings_list = seq_iter(bindings);
  Env* letstar_env = env_make(*env);

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

  *env = letstar_env;
  return forms;
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
      return else_form;
    }
    else {
      return make_nil();
    }

  } else {
    return then_form;
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
  return lst->data;
}

MalType eval_quote(list lst) {

  explode1("quote", lst, form);
  return form;
}

MalType eval_quasiquote(list lst) {
  explode1("quasiquote", lst, form);
  return quasiquote(form);
  // Implicit error propagation.
}

MalType quasiquote(MalType ast) {

  /* argument to quasiquote is a vector: (quasiquote [first rest]) */
  list lst;
  vector_t vec;
  if ((vec = is_vector(ast))) {

    return quasiquote_vector(vec);
    // Implicit error propagation
  }

  /* argument to quasiquote is a list: (quasiquote (first rest)) */
  else if (is_list(ast, &lst)){

    if(lst) {
      MalType first = lst->data;
      if(equal_forms(first, SYMBOL_UNQUOTE)) {
        lst = lst->next;
        explode1("unquote", lst, unquoted);
        return unquoted;
      }
    }
    return quasiquote_list(lst);
    // Implicit error propagation
  }
  /* argument to quasiquote is not self-evaluating and isn't sequential: (quasiquote val)
     => (quote val) */
  else if(type(ast) & (MALTYPE_HASHMAP | MALTYPE_SYMBOL)) {

    list lst = NULL;
    lst = list_push(lst, ast);
    lst = list_push(lst, SYMBOL_QUOTE);
    return make_list(lst);
  }
  /* argument to quasiquote is self-evaluating: (quasiquote val)
     => val */
  else {
    return ast;
  }
}

MalType quasiquote_vector(vector_t vec) {

  MalType result = make_list(NULL);
  for (size_t i = vec->count; i--; ) {
    result = quasiquote_folder(vec->nth[i], result);
    if (mal_error) return NULL;
  }

    list lst = NULL;
    lst = list_push(lst, result);
    lst = list_push(lst, SYMBOL_VEC);

    return make_list(lst);
}

MalType quasiquote_list(list args) {

    /* handle empty list: (quasiquote ())
       => () */
    if (!args) {
      return make_list(NULL);
    }

    MalType first = args->data;

    MalType qq_rest = quasiquote_list(args->next);
    if(mal_error) return NULL;

    return quasiquote_folder(first, qq_rest);
    // Implicit error propagation.
}

MalType quasiquote_folder(MalType first, MalType qq_rest) {

    /* handle splice-unquote: (quasiquote ((splice-unquote first-second) rest))
       => (concat first-second (quasiquote rest)) */
    list lst;
    if(is_list(first, &lst)) {
      if(lst) {
        MalType lst_first = lst->data;
        if (equal_forms(lst_first, SYMBOL_SPLICE_UNQUOTE)) {
          lst = lst->next;
          explode1("splice-unquote", lst, unquoted);
          return make_list(list_push(list_push(list_push(NULL, qq_rest),
                                               unquoted),
                                     SYMBOL_CONCAT));
        }
      }
    }
    MalType qqted = quasiquote(first);
    if(mal_error) return NULL;
    return make_list(list_push(list_push(list_push(NULL, qq_rest),
                                         qqted),
                               SYMBOL_CONS));
}

MalType eval_defmacrobang(list lst, Env* env) {

  explode2("defmacro!", lst, defbang_symbol, defbang_value);

  MalType result = EVAL(defbang_value, env);

  if (mal_error) return NULL;

  MalClosure closure = is_closure(result);
  if (!closure) {
    bad_type("defmacro!", MALTYPE_CLOSURE, result);
  }
  result = make_macro(closure->env, closure->fnstar_args);
  check_type("defmacro!", MALTYPE_SYMBOL, defbang_symbol);
  env_set(env, defbang_symbol, result);
  return result;
}

MalType eval_try(list lst, Env** env) {

  if (!lst) {
    bad_arg_count("try*", "one or two arguments", lst);
  }

  MalType try_clause = lst->data;

  list l = lst->next;
  if (!l) {
    /* no catch* clause */
    return try_clause;
  }

  MalType catch_clause = l->data;
  if (l->next) {
    bad_arg_count("try*", "one or two arguments", lst);
  }

  /* process catch* clause */
  check_type("try*", MALTYPE_LIST, catch_clause);
  list catch_list;
  if (!is_list(catch_clause, &catch_list)) {
    bad_type("try*", MALTYPE_LIST, catch_clause);
  }
  explode3("try*(catch clause)", catch_list, catch_symbol, a2, handler);
  if (!equal_forms(catch_symbol, SYMBOL_CATCH)) {
    make_error("'try*': catch* clause is missing catch* symbol: %M",
                          catch_clause);
  }
  check_type("try*", MALTYPE_SYMBOL, a2);

  MalType try_result = EVAL(try_clause, *env);
  if(!mal_error) {
    *env = NULL;                // prevent TCO
    return try_result;
  }

  /* bind the symbol to the exception */
  Env* catch_env = env_make(*env);
  env_set(catch_env,
          a2, mal_error);
  mal_error = NULL;
  *env = catch_env;

  return handler;
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

/* used by core functions but not EVAL as doesn't do TCE */
MalType apply(MalType fn, list args) {

  function_t fun_ptr;
  if ((fun_ptr = is_function(fn))) {

    return (*fun_ptr)(args);
    // Implicit error propagation
  }
  else {

    MalClosure closure = is_closure(fn);
    if (!closure) closure = is_macro(fn);
    assert(closure);
    MalType ast = closure->fnstar_args->next->data;
    Env* env = env_apply(closure, args);
    if (mal_error) return NULL;
    return EVAL(ast, env);
    // Implicit error propagation
  }
}
