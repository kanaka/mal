#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "libs/linked_list/linked_list.h"
#include "types.h"
#include "reader.h"
#include "printer.h"
#include "env.h"

#define SYMBOL_DEFBANG "def!"
#define SYMBOL_LETSTAR "let*"

#define PROMPT_STRING "user> "

MalType apply(MalType, list);
MalType evaluate_list(list*, list, Env*);
// Store the result into the first argument and return NULL
// If an error occurs, it is returned.
MalType evaluate_vector(list, Env*);
MalType evaluate_hashmap(list, Env*);
MalType eval_defbang(list, Env*);
MalType eval_letstar(list, Env*);

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
generic_arithmetic(mal_add, +, make_integer, make_float)
generic_arithmetic(mal_sub, -, make_integer, make_float)
generic_arithmetic(mal_mul, *, make_integer, make_float)
generic_arithmetic(mal_div, /, make_integer, make_float)

MalType READ(const char* str) {

  return read_str(str);
}

MalType EVAL(MalType ast, Env* env) {

  MalType dbgeval = env_get(env, "DEBUG-EVAL");
  if (dbgeval && ! is_false(dbgeval) && ! is_nil(dbgeval))
    printf("EVAL: %M\n", ast);

  if (is_symbol(ast)) {
    MalType symbol_value = env_get(env, ast->value.mal_string);
    if (symbol_value)
      return symbol_value;
    else
      // make_error would prefix with EVAL: and break some tests
      return wrap_error(make_string(mal_printf("'%M' not found", ast)));
  }

  if (is_vector(ast)) {
    return evaluate_vector(ast->value.mal_list, env);
  }

  if (is_hashmap(ast)) {
    return evaluate_hashmap(ast->value.mal_list, env);
  }

  /* not a list */
  if (!is_list(ast)) { return ast; }

  list lst = ast->value.mal_list;

  /* empty list */
  if(lst == NULL) { return ast; }

  /* list */
  MalType first = lst->data;
  lst = lst->next;

  if(is_symbol(first)) {
    const char* symbol = first->value.mal_string;

    /* handle special symbols first */
    if (strcmp(symbol, SYMBOL_DEFBANG) == 0) {
      return eval_defbang(lst, env);
    }
    else if (strcmp(symbol, SYMBOL_LETSTAR) == 0) {
      return eval_letstar(lst, env);
    }
  }
  /* first element is not a special symbol */
  MalType func = EVAL(first, env);
  if (is_error(func)) { return func; }
  //  Evaluate the arguments
  list evlst;
  MalType error = evaluate_list(&evlst, lst, env);
  if(error) return error;

  /* apply the first element of the list to the arguments */
  return apply(func, evlst);
}

void PRINT(MalType val) {

  printf("%M\n", val);
}

void rep(const char* str, Env* env) {

  PRINT(EVAL(READ(str), env));
}

int main(int, char**) {

  printer_init();

  Env* repl_env = env_make(NULL);

  env_set(repl_env, "+", make_function(mal_add));
  env_set(repl_env, "-", make_function(mal_sub));
  env_set(repl_env, "*", make_function(mal_mul));
  env_set(repl_env, "/", make_function(mal_div));

    char* input;
    while((input = readline(PROMPT_STRING))) {

      /* print prompt and get input*/
      /* readline allocates memory for input */
      /* Check for EOF (Ctrl-D) */
      /* add input to history */
      add_history(input);

      /* call Read-Eval-Print */
      rep(input, repl_env);

      /* have to release the memory used by readline */
      free(input);
    }
    printf("\n");
  return EXIT_SUCCESS;
}

MalType eval_defbang(list lst, Env* env) {

  MalType defbang_symbol = eat_argument(lst);
  MalType defbang_value = eat_argument(lst);
  check_empty(lst);

  MalType result = EVAL(defbang_value, env);
  if (!is_error(result)){
    env_set(env, as_symbol(defbang_symbol), result);
  }
  return result;
}

MalType eval_letstar(list lst, Env* env) {

  MalType bindings = eat_argument(lst);
  MalType forms = eat_argument(lst);
  check_empty(lst);

  list bindings_list = as_sequence(bindings);
  Env* letstar_env = env_make(env);

  /* evaluate the bindings */
  while(bindings_list) {

    MalType symbol = bindings_list->data;
    bindings_list = bindings_list->next;
    if(!bindings_list) {
      return make_error_fmt("expected an even number of binding pairs, got: %M",
                            bindings);
    }
    MalType value = EVAL(bindings_list->data, letstar_env);

    /* early return from error */
    if (is_error(value)) {
      return value;
    }

    env_set(letstar_env, as_symbol(symbol), value);
    bindings_list = bindings_list->next;
  }

  return EVAL(forms, letstar_env);
}

MalType evaluate_list(list* evlst, list lst, Env* env) {

  *evlst = NULL;
  list* evlst_last = evlst;
  while (lst) {

    MalType val = EVAL(lst->data, env);

    if (is_error(val)) {
      return val;
    }

    *evlst_last = list_push(NULL, val);
    evlst_last = &(*evlst_last)->next;
    lst = lst->next;
  }
  return NULL;
}

MalType evaluate_vector(list lst, Env* env) {
  /* TODO: implement a real vector */
  list evlst;
  MalType error = evaluate_list(&evlst, lst, env);
  if(error) return error;
  return make_vector(evlst);
}

MalType evaluate_hashmap(list lst, Env* env) {
  /* TODO: implement a real hashmap */
  list evlst;
  MalType error = evaluate_list(&evlst, lst, env);
  if(error) return error;
  return make_hashmap(evlst);
}

MalType apply(MalType fn, list args) {
  if (is_function(fn)) {

    MalType(*fun_ptr)(list) = fn->value.mal_function;
    return (*fun_ptr)(args);
  }
  else {
    return bad_type(fn, "a function");
  }
}
