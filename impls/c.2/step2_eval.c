#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>
#include <editline/readline.h>
#include <editline/history.h>

#include "libs/linked_list/linked_list.h"
#include "types.h"
#include "reader.h"
#include "printer.h"

#define PROMPT_STRING "user> "

MalType apply(MalType, list);
MalType evaluate_list(list*, list, list);
// Store the result into the first argument and return NULL
// If an error occurs, it is returned.
MalType evaluate_vector(list, list);
MalType evaluate_hashmap(list, list);

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

MalType EVAL(MalType ast, list env) {

  /* printf("EVAL: %M\n", ast); */

  if (is_symbol(ast)) {
    for(list l=env; l; l=l->next->next) {
      if(!strcmp(l->data->value.mal_string, ast->value.mal_string)) {
        return l->next->data;
      }
    }
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

void rep(const char* str, list env) {

  PRINT(EVAL(READ(str), env));
}

int main(int, char**) {

  printer_init();

  list repl_env = NULL;
  repl_env = list_push(repl_env, make_function(mal_add));
  repl_env = list_push(repl_env, make_symbol("+"));
  repl_env = list_push(repl_env, make_function(mal_sub));
  repl_env = list_push(repl_env, make_symbol("-"));
  repl_env = list_push(repl_env, make_function(mal_mul));
  repl_env = list_push(repl_env, make_symbol("*"));
  repl_env = list_push(repl_env, make_function(mal_div));
  repl_env = list_push(repl_env, make_symbol("/"));

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
MalType evaluate_list(list* evlst, list lst, list env) {

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

MalType evaluate_vector(list lst, list env) {
  /* TODO: implement a real vector */
  list evlst;
  MalType error = evaluate_list(&evlst, lst, env);
  if(error) return error;
  return make_vector(evlst);
}

MalType evaluate_hashmap(list lst, list env) {
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
