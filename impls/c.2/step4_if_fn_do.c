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
#include "env.h"
#include "core.h"

#define SYMBOL_DEFBANG "def!"
#define SYMBOL_LETSTAR "let*"
#define SYMBOL_DO "do"
#define SYMBOL_IF "if"
#define SYMBOL_FNSTAR "fn*"

#define PROMPT_STRING "user> "

MalType apply(MalType, list);
MalType evaluate_list(list*, list, Env*);
// Store the result into the first argument and return NULL
// If an error occurs, it is returned.
MalType evaluate_vector(list, Env*);
MalType evaluate_hashmap(list, Env*);
MalType eval_defbang(list, Env*);
MalType eval_letstar(list, Env*);
MalType eval_if(list, Env*);
MalType eval_fnstar(list, const Env*);
MalType eval_do(list, Env*);

MalType READ(const char* str) {

  return read_str(str);
}

MalType env_apply(MalClosure closure, list args, Env** env) {
  //  Return the closure definition and update env if all went OK,
  //  else return an error.
  Env* fn_env = env_make(closure->env);
  for(size_t i=0; i<closure->param_len; i++) {
    env_set(fn_env, closure->parameters[i], eat_argument(args));
  }
  /* set the 'more' symbol if there is one */
  if (closure->more_symbol) {
    env_set(fn_env, closure->more_symbol, make_list(args));
  }
  else {
    check_empty(args);
  }
  *env = fn_env;
  return closure->definition;
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
    else if (strcmp(symbol, SYMBOL_IF) == 0) {
      return eval_if(lst, env);
    }
    else if (strcmp(symbol, SYMBOL_FNSTAR) == 0) {
      return eval_fnstar(lst, env);
    }
    else if (strcmp(symbol, SYMBOL_DO) == 0) {
      return eval_do(lst, env);
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

//  Variant reporting errors during startup.
void re(const char *str, Env* env) {
  MalType result = EVAL(READ(str), env);
  if(is_error(result)) {
    printf("Error during startup: %M\n", result);
    exit(EXIT_FAILURE);
  }
}

int main(int, char**) {

  printer_init();

  Env* repl_env = env_make(NULL);

  ns core;
  size_t core_size;
  ns_make_core(&core, &core_size);
  while(core_size--) {
    const char* symbol = core[core_size].key;
    function_t function = core[core_size].value;
    env_set(repl_env, symbol, make_function(function));
  }

  /* add functions written in mal - not using rep as it prints the result */
  re("(def! not (fn* (a) (if a false true)))", repl_env);

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

MalType eval_if(list lst, Env* env) {

  MalType raw_condition = eat_argument(lst);
  MalType then_form = eat_argument(lst);
  MalType else_form;
  if(lst) {
    else_form = lst->data;
    lst = lst->next;
    check_empty(lst);
  }
  else {
    else_form = NULL;
  }

  MalType condition = EVAL(raw_condition, env);

  if (is_error(condition)) {
    return condition;
  }

  if (is_false(condition) || is_nil(condition)) {

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
  MalType val = make_nil();
  while(lst) {
    val = EVAL(lst->data, env);
    /* return error early */
    if (is_error(val)) break;
    lst = lst->next;
  }
  return val;
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

MalType eval_fnstar(list lst, const Env* env) {

  MalType a1 = eat_argument(lst);
  MalType definition = eat_argument(lst);
  check_empty(lst);
  list args = as_sequence(a1);
  size_t param_len = 0;
  const char** parameters = GC_MALLOC(list_count(args)*sizeof(char*));
  const char* more_symbol = NULL;

  while (args) {

    MalType val = args->data;

    const char* val_sym = as_symbol(val);

    if(val_sym[0] == '&') {

      /* & is found but there is no symbol */
      if (val_sym[1] == '\0') {
        if(!args->next) {
          return make_error_fmt("missing symbol after '&' in argument list");
        }
      /* & is found and there is a single symbol after */
        else if(!args->next->next) {
          more_symbol = as_symbol(args->next->data);
          break;
        }
      /* & is found and there extra symbols after */
        else {
          return make_error_fmt("unexpected symbol after '& %M' in argument list: '%M'",
                                args->next->data, args->next->next->data);
        }
      }
      /* & is found as part of the symbol and no other symbols */
      else if(!args->next) {
        more_symbol = val_sym + 1;
        break;
      }
      /* & is found as part of the symbol but there are other symbols after */
      else {
        return make_error_fmt("unexpected symbol after '%s' in argument list: '%M'",
                              val_sym, args->next->data);
      }
    }

    /* & is not found - add the symbol to the regular argument list */
    else {

      for(size_t i=0; i<param_len; i++) {
        if(!strcmp(val_sym, parameters[i])) {
          return make_error_fmt("duplicate symbol in argument list: '%s'",
                                parameters[i]);
        }
      }
      parameters[param_len++] = val_sym;
    }
    args = args->next;
  }

  return make_closure(env, param_len, parameters, definition, more_symbol);
}

MalType apply(MalType fn, list args) {

  if (is_function(fn)) {

    MalType(*fun_ptr)(list) = fn->value.mal_function;
    return (*fun_ptr)(args);
  }
  else if(is_closure(fn)) {

    Env* env;
    MalType ast = env_apply(fn->value.mal_closure, args, &env);
    if(is_error(ast)) return ast;
    return EVAL(ast, env);
  }
  else {
    return bad_type(fn, "a function or closure");
  }
}
