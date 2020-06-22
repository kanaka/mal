#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gc.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "types.h"
#include "reader.h"
#include "printer.h"
#include "env.h"

#define SYMBOL_DEFBANG "def!"
#define SYMBOL_LETSTAR "let*"

#define PROMPT_STRING "user> "

MalType* READ(char* str) {

  return read_str(str);
}

MalType* EVAL(MalType* ast, Env* env) {

  /* forward references */
  MalType* eval_ast(MalType* ast, Env* env);
  MalType* eval_defbang(MalType* ast, Env* env);
  MalType* eval_letstar(MalType* ast, Env* env);

  /* NULL */
  if (!ast) { return make_nil(); }

  /* not a list */
  if (!is_list(ast)) { return eval_ast(ast, env); }

  /* empty list */
  if (ast->value.mal_list == NULL) { return ast; }

  /* list */
  MalType* first = (ast->value.mal_list)->data;
  char* symbol = first->value.mal_symbol;

  if (is_symbol(first)) {

    /* handle special symbols first */
    if (strcmp(symbol, SYMBOL_DEFBANG) == 0) {
      return eval_defbang(ast, env);
    }
    else if (strcmp(symbol, SYMBOL_LETSTAR) == 0) {
      return eval_letstar(ast, env);
    }
  }
  /* first element is not a special symbol */
  MalType* evaluated_list = eval_ast(ast, env);

  if (is_error(evaluated_list)) { return evaluated_list; }

  /* apply the first element of the list to the arguments */
  list evlst = evaluated_list->value.mal_list;
  MalType* func = evlst->data;

  if (is_function(func)) {
    return (*func->value.mal_function)(evlst->next);
  }
  else {
    return make_error_fmt("Error: first item in list is not callable: %s.", \
                          pr_str(func, UNREADABLY));
  }
}

void PRINT(MalType* val) {

  char* output = pr_str(val, READABLY);
  printf("%s\n", output);
}

void rep(char* str, Env* env) {

  PRINT(EVAL(READ(str), env));
}

int main(int argc, char** argv) {

  MalType* mal_add(list args);
  MalType* mal_sub(list args);
  MalType* mal_mul(list args);
  MalType* mal_div(list args);

  /* Greeting message */
  puts("Make-a-lisp version 0.0.3\n");
  puts("Press Ctrl+d to exit\n");

  Env* repl_env = env_make(NULL, NULL, NULL, NULL);
  repl_env = env_set_C_fn(repl_env, "+", mal_add);
  repl_env = env_set_C_fn(repl_env, "-", mal_sub);
  repl_env = env_set_C_fn(repl_env, "*", mal_mul);
  repl_env = env_set_C_fn(repl_env, "/", mal_div);

  while (1) {

    /* print prompt and get input*/
    /* readline allocates memory for input */
    char* input = readline(PROMPT_STRING);

    /* Check for EOF (Ctrl-D) */
    if (!input) {
      printf("\n");
      return 0;
    }

    /* add input to history */
    add_history(input);

    /* call Read-Eval-Print */
    rep(input, repl_env);

    /* have to release the memory used by readline */
    free(input);
  }

  return 0;
}

MalType* eval_ast(MalType* ast, Env* env) {

  /* forward references */
  list evaluate_list(list lst, Env* env);
  list evaluate_vector(list lst, Env* env);
  list evaluate_hashmap(list lst, Env* env);

  if (is_symbol(ast)) {

    MalType* symbol_value = env_get(env, ast);

    if (symbol_value) {
      return symbol_value;
    } else {
      return make_error_fmt("var '%s' not found", pr_str(ast, UNREADABLY));
    }
  }
  else if (is_list(ast)) {

    list result = evaluate_list(ast->value.mal_list, env);

    if (!result || !is_error(result->data)) {
      return make_list(result);
    } else {
      return result->data;
    }
  }
  else if (is_vector(ast)) {

    list result = evaluate_vector(ast->value.mal_list, env);

    if (!result || !is_error(result->data)) {
      return make_vector(result);
    } else {
      return result->data;
    }
  }
  else if (is_hashmap(ast)) {

    list result = evaluate_hashmap(ast->value.mal_list, env);

    if (!result || !is_error(result->data)) {
      return make_hashmap(result);
    } else {
      return result->data;
    }
  }
  else {
    return ast;
  }
}

MalType* eval_defbang(MalType* ast, Env* env) {

  list lst = (ast->value.mal_list)->next;

  /* TODO: Check the number and types of parameters */
  MalType* defbang_symbol = lst->data;
  MalType* defbang_value = lst->next->data;

  MalType* result = EVAL(defbang_value, env);

  if (!is_error(result)) {
    env_set(env, defbang_symbol, result);
  }
  return result;
}

MalType* eval_letstar(MalType* ast, Env* env) {

  list lst = ast->value.mal_list;
  lst = lst->next;

  /* TODO: Check the bindings list is valid, has an even number of elements, etc*/
  Env* letstar_env = env_make(env, NULL, NULL, NULL);

  MalType* letstar_bindings = lst->data;
  list letstar_bindings_list = letstar_bindings->value.mal_list;

  /* evaluate the bindings */
  while(letstar_bindings_list) {

    MalType* symbol = letstar_bindings_list->data;
    MalType* value = letstar_bindings_list->next->data;
    letstar_env = env_set(letstar_env, symbol, EVAL(value, letstar_env));

    letstar_bindings_list = letstar_bindings_list->next->next; /* pop symbol and value*/
  }

  /* evaluate the forms in the presence of bindings */
  MalType* forms = lst->next->data;
  return EVAL(forms, letstar_env);
}

list evaluate_list(list lst, Env* env) {

  list evlst = NULL;
  while (lst) {
    evlst = list_push(evlst, EVAL(lst->data, env));
    lst = lst->next;
  }
  return list_reverse(evlst);
}

list evaluate_vector(list lst, Env* env) {
  /* TODO: implement a real vector */
  list evlst = NULL;
  while (lst) {
    evlst = list_push(evlst, EVAL(lst->data, env));
    lst = lst->next;
  }
  return list_reverse(evlst);
}

list evaluate_hashmap(list lst, Env* env) {
  /* TODO: implement a real hashmap */
  list evlst = NULL;
  while (lst) {

    /* keys are unevaluated */
    evlst = list_push(evlst, lst->data);
    lst = lst->next;
    /* values are evaluated */
    evlst = list_push(evlst, EVAL(lst->data, env));
    lst = lst->next;
  }
  return list_reverse(evlst);
}

MalType* mal_add(list args) {

  MalType* result = GC_MALLOC(sizeof(*result));
  result->type = MALTYPE_INTEGER;

  list arg_list = args;

  long sum = 0;
  while(arg_list) {

    MalType* val = arg_list->data;
    /* TODO: check argument type */

    sum = sum + val->value.mal_integer;

    arg_list = arg_list->next;
  }

  result->value.mal_integer = sum;
  return result;
}

MalType* mal_sub(list args) {

  long sum;
  MalType* result = GC_MALLOC(sizeof(*result));
  result->type = MALTYPE_INTEGER;

  list arg_list = args;
  if (arg_list) {

    MalType* first_val = arg_list->data;
    arg_list = arg_list->next;
    /* TODO: check argument type */

    sum = first_val->value.mal_integer;
    while(arg_list) {

      MalType* val = arg_list->data;
      /* TODO: check argument type */

      sum = sum - val->value.mal_integer;

      arg_list = arg_list->next;
    }
  }
  else {
    sum = 0;
  }

  result->value.mal_integer = sum;
  return result;
}

MalType* mal_mul(list args) {

  MalType* result = GC_MALLOC(sizeof(*result));
  result->type = MALTYPE_INTEGER;

  list arg_list = args;

  long product = 1;
  while(arg_list) {

    MalType* val = arg_list->data;
    /* TODO: check argument type */

    product *= val->value.mal_integer;

    arg_list = arg_list->next;
  }

  result->value.mal_integer = product;
  return result;
}

MalType* mal_div(list args) {

  long product;
  MalType* result = GC_MALLOC(sizeof(*result));
  result->type = MALTYPE_INTEGER;

  list arg_list = args;

  if (arg_list) {
    MalType* first_val = arg_list->data;
    /* TODO: check argument type */
    product = first_val->value.mal_integer;
    arg_list = arg_list->next;

    while(arg_list) {

      MalType* val = arg_list->data;
      /* TODO: check argument type */

    product /= (val->value.mal_integer);
    arg_list = arg_list->next;
    }
  } else {
    product = 1;
  }
  result->value.mal_integer = product;
  return result;
}
