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
#include "core.h"

#define SYMBOL_DEFBANG "def!"
#define SYMBOL_LETSTAR "let*"
#define SYMBOL_IF "if"
#define SYMBOL_FNSTAR "fn*"
#define SYMBOL_DO "do"

#define PROMPT_STRING "user> "

MalType* READ(char* str) {

  return read_str(str);
}

MalType* EVAL(MalType* ast, Env* env) {

  /* forward references */
  MalType* eval_ast(MalType* ast, Env* env);
  MalType* eval_defbang(MalType* ast, Env* env);
  MalType* eval_letstar(MalType* ast, Env* env);
  MalType* eval_if(MalType* ast, Env* env);
  MalType* eval_fnstar(MalType* ast, Env* env);
  MalType* eval_do(MalType* ast, Env* env);

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
    else if (strcmp(symbol, SYMBOL_IF) == 0) {
      return eval_if(ast, env);
    }
    else if (strcmp(symbol, SYMBOL_FNSTAR) == 0) {
      return eval_fnstar(ast, env);
    }
    else if (strcmp(symbol, SYMBOL_DO) == 0) {
      return eval_do(ast, env);
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
  else if (is_closure(func)) {

    MalClosure* closure = func->value.mal_closure;
    list params = (closure->parameters)->value.mal_list;

    long param_count = list_count(params);
    long arg_count = list_count(evlst->next);

    if (param_count > arg_count) {
      return make_error("too few arguments supplied to function");
    }
    else if ((param_count < arg_count) && !closure->more_symbol) {
      return make_error("too many arguments supplied to function");
    }
    else {

      Env* new_env = env_make(closure->env, params, evlst->next, closure->more_symbol);
      return EVAL(closure->definition, new_env);
    }
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

  /* Greeting message */
  puts("Make-a-lisp version 0.0.4\n");
  puts("Press Ctrl+d to exit\n");

  Env* repl_env = env_make(NULL, NULL, NULL, NULL);

  ns* core = ns_make_core();
  hashmap mappings = core->mappings;

  while (mappings) {
    char* symbol = mappings->data;
    MalType*(*function)(list) = (MalType*(*)(list))mappings->next->data;

    env_set_C_fn(repl_env, symbol, function);

    /* pop symbol and function from hashmap/list */
    mappings = mappings->next->next;
  }

  /* add not function */
  /* not using rep as it prints the result */
  EVAL(READ("(def! not (fn* (a) (if a false true)))"), repl_env);

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

  if (!lst || !lst->next || lst->next->next) {
    return make_error_fmt("'def!': expected exactly two arguments");
  }

  MalType* defbang_symbol = lst->data;

  if (!is_symbol(defbang_symbol)) {
    return make_error_fmt("'def!': expected symbol for first argument");
  }

  MalType* defbang_value = lst->next->data;
  MalType* result = EVAL(defbang_value, env);

  if (!is_error(result)){
    env = env_set(env, defbang_symbol, result);
  }
  return result;
}

MalType* eval_letstar(MalType* ast, Env* env) {

  list lst = ast->value.mal_list;

  if (!lst->next) {
    return make_error("'let*': missing bindings list");
  }

  MalType* bindings = lst->next->data;
  MalType* forms = lst->next->next ? lst->next->next->data : make_nil();

  if (!is_sequential(bindings)) {
    return make_error("'let*': first argument is not list or vector");
  }

  list bindings_list = bindings->value.mal_list;
  if (list_count(bindings_list) % 2 == 1) {
    return  make_error("'let*': expected an even number of binding pairs");
  }

  Env* letstar_env = env_make(env, NULL, NULL, NULL);

  /* evaluate the bindings */
  while(bindings_list) {

    MalType* symbol = bindings_list->data;
    MalType* value = EVAL(bindings_list->next->data, letstar_env);

    /* early return from error */
    if (is_error(value)) { return  value; }

    env_set(letstar_env, symbol, value);
    bindings_list = bindings_list->next->next;
  }
  return EVAL(forms, letstar_env);
}

MalType* eval_if(MalType* ast, Env* env) {

  list lst = ast->value.mal_list;

  if (!lst->next || !lst->next->next) {
    return make_error("'if': too few arguments");
  }

  if (lst->next->next->next && lst->next->next->next->next) {
    return make_error("'if': too many arguments");
  }

  MalType* condition = EVAL(lst->next->data, env);

  if (is_error(condition)) { return condition; }

  if (is_false(condition) || is_nil(condition)) {

    /* check whether false branch is present */
    if (lst->next->next->next) {
      return EVAL(lst->next->next->next->data, env);
    }
    else {
      return make_nil();
    }

  } else {
    return EVAL(lst->next->next->data, env);
  }
}

MalType* eval_fnstar(MalType* ast, Env* env) {

  /* forward reference */
  MalType* regularise_parameters(list* params, MalType** more);

  list lst = ast->value.mal_list;

  if (!lst->next) {
    return make_error("'fn*': missing argument list");
  }
  else if (!lst->next->next) {
    return make_error("'fn*': missing function body");
  }

  MalType* params = lst->next->data;
  list params_list = params->value.mal_list;

  MalType* more_symbol = NULL;

  MalType* result = regularise_parameters(&params_list, &more_symbol);
  if (is_error(result)) { return result; }

  MalType* definition = lst->next->next->data;
  MalType* regular_params = make_list(params_list);

  return make_closure(env, regular_params, definition, more_symbol);
}

MalType* eval_do(MalType* ast, Env* env) {

  list lst = ast->value.mal_list;

  /* handle empty 'do' */
  if (!lst->next) { return make_nil(); }

  /* evaluate all but the last form */
  lst = lst->next;
  while (lst->next) {

    MalType* val = EVAL(lst->data, env);

    /* return error early */
    if (is_error(val)) { return val; }
    lst = lst->next;
  }
  /* return the last value */
  return EVAL(lst->data, env);
}

list evaluate_list(list lst, Env* env) {

  list evlst = NULL;
  while (lst) {

    MalType* val = EVAL(lst->data, env);

    if (is_error(val)) {
      return list_make(val);
    }

    evlst = list_push(evlst, val);
    lst = lst->next;
  }
  return list_reverse(evlst);
}

list evaluate_vector(list lst, Env* env) {
  /* TODO: implement a real vector */
  list evlst = NULL;
  while (lst) {

    MalType* val = EVAL(lst->data, env);

    if (is_error(val)) {
      return list_make(val);
    }

    evlst = list_push(evlst, val);
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
    MalType* val = EVAL(lst->data, env);

    if (is_error(val)) {
      return list_make(val);
    }

    evlst = list_push(evlst, val);
    lst = lst->next;
  }
  return list_reverse(evlst);
}

MalType* regularise_parameters(list* args, MalType** more_symbol) {

  /* forward reference */
  char* symbol_fn(gptr data);

  list regular_args = NULL;
  while (*args) {

    MalType* val = (*args)->data;

    if (!is_symbol(val)) {
      return make_error_fmt("non-symbol found in fn argument list '%s'", \
                            pr_str(val, UNREADABLY));
    }

    if (val->value.mal_symbol[0] == '&') {

      /* & is found but there is no symbol */
      if (val->value.mal_symbol[1] == '\0' && !(*args)->next) {
        return make_error("missing symbol after '&' in argument list");
      }
      /* & is found and there is a single symbol after */
      else if ((val->value.mal_symbol[1] == '\0' && (*args)->next &&
                is_symbol((*args)->next->data) && !(*args)->next->next)) {

        /* TODO: check symbol is no a duplicate of one already on the list */
        *more_symbol = (*args)->next->data;
        break;
      }
      /* & is found and there extra symbols after */
      else if ((val->value.mal_symbol[1] == '\0' && (*args)->next && (*args)->next->next)) {
        return make_error_fmt("unexpected symbol after '& %s' in argument list: '%s'", \
                              pr_str((*args)->next->data, UNREADABLY),  \
                              pr_str((*args)->next->next->data, UNREADABLY));
      }
      /* & is found as part of the symbol and no other symbols */
      else if (val->value.mal_symbol[1] != '\0' && !(*args)->next) {
        *more_symbol = make_symbol((val->value.mal_symbol + 1));
        break;
      }
      /* & is found as part of the symbol but there are other symbols after */
      else if (val->value.mal_symbol[1] != '\0' && (*args)->next) {
        return make_error_fmt("unexpected symbol after '%s' in argument list: '%s'", \
                              pr_str(val, UNREADABLY),  \
                              pr_str((*args)->next->data, UNREADABLY));
       }
    }

    /* & is not found - add the symbol to the regular argument list */
    else {

      if (list_findf(regular_args, val->value.mal_symbol, symbol_fn) > 0) {
        return make_error_fmt("duplicate symbol in argument list: '%s'", pr_str(val, UNREADABLY));
      }
      else {
        regular_args = list_push(regular_args, val);
      }
    }
    *args = (*args)->next;
  }

  *args = list_reverse(regular_args);
  return make_nil();
}

char* symbol_fn(gptr data) {
  MalType* val = data;
  return (val->value.mal_symbol);
}

/* silence the compiler after swap!, apply, and map are added to the core */
MalType* apply(MalType* ast, Env* env) {
  return make_nil();
}
