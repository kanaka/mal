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
#define SYMBOL_QUOTE "quote"
#define SYMBOL_QUASIQUOTE "quasiquote"
#define SYMBOL_UNQUOTE "unquote"
#define SYMBOL_SPLICE_UNQUOTE "splice-unquote"
#define SYMBOL_DEFMACROBANG "defmacro!"

#define PROMPT_STRING "user> "

MalType apply(MalType, list); // For the apply phase and core apply/map/swap.
MalType evaluate_list(list*, list, Env*);
// Store the result into the first argument and return NULL
// If an error occurs, it is returned.
MalType evaluate_vector(list, Env*);
MalType evaluate_hashmap(list, Env*);
MalType eval_defbang(list, Env*);
MalType eval_letstar(list, Env**); // TCO
MalType eval_if(list, Env**); // TCO unless the environment is set to NULL.
MalType eval_fnstar(list, const Env*);
MalType eval_do(list, Env*); // TCO in the same env
MalType eval_quote(list);
MalType eval_quasiquote(list);
MalType quasiquote(MalType);
MalType quasiquote_vector(list);
MalType quasiquote_list(list);
MalType eval_defmacrobang(list, Env*);

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

  /* Use goto to jump here rather than calling eval for tail-call elimination */
 TCE_entry_point:

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

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_letstar(lst, &env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_IF) == 0) {

      /* TCE - modify ast directly and jump back to eval */
      ast = eval_if(lst, &env);

      if (is_error(ast)) { return ast; }
      if(env) goto TCE_entry_point; else return ast;
    }
    else if (strcmp(symbol, SYMBOL_FNSTAR) == 0) {
      return eval_fnstar(lst, env);
    }
    else if (strcmp(symbol, SYMBOL_DO) == 0) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_do(lst, env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_QUOTE) == 0) {
      return eval_quote(lst);
    }
    else if (strcmp(symbol, SYMBOL_QUASIQUOTE) == 0) {

      ast = eval_quasiquote(lst);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_DEFMACROBANG) == 0) {
      return eval_defmacrobang(lst, env);
    }
  }
  /* first element is not a special symbol */
  MalType func = EVAL(first, env);
  if (is_error(func)) { return func; }
  if(func->type == MALTYPE_MACRO) {
    ast = apply(func, lst);
    if (is_error(ast)) { return ast; }
    goto TCE_entry_point;
  }
  //  Evaluate the arguments
  list evlst;
  MalType error = evaluate_list(&evlst, lst, env);
  if(error) return error;

  /* apply the first element of the list to the arguments */
  if (is_closure(func)) {

      MalClosure closure = func->value.mal_closure;

      /* TCE - modify ast and env directly and jump back to eval */
      ast = env_apply(closure, evlst, &env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
  }
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

/* declare as global so it can be accessed by mal_eval */
Env* global_env;

MalType mal_eval(list args) {

  MalType ast = eat_argument(args);
  check_empty(args);
  return EVAL(ast, global_env);
}

int main(int argc, char** argv) {

  printer_init();

  Env* repl_env = env_make(NULL);
  global_env = repl_env;

  ns core;
  size_t core_size;
  ns_make_core(&core, &core_size);
  while(core_size--) {
    const char* symbol = core[core_size].key;
    function_t function = core[core_size].value;
    env_set(repl_env, symbol, make_function(function));
  }

  env_set(repl_env, "eval", make_function(mal_eval));

  /* add functions written in mal - not using rep as it prints the result */
  re("(def! not (fn* (a) (if a false true)))", repl_env);
  re("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env);
  re("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env);

  /* make command line arguments available in the environment */
  list lst = NULL;
  while(1 < --argc) {
    lst = list_push(lst, make_string(argv[argc]));
  }
  env_set(repl_env, "*ARGV*", make_list(lst));

  /* run in script mode if a filename is given */
  if (argc) {

    /* first argument on command line is filename */
    const char* load_command = mal_printf("(load-file \"%s\")", argv[1]);
    re(load_command, repl_env);
  }
  /* run in repl mode when no cmd line args */
  else {

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
  }
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

MalType eval_letstar(list lst, Env** env) {

  MalType bindings = eat_argument(lst);
  MalType forms = eat_argument(lst);
  check_empty(lst);

  list bindings_list = as_sequence(bindings);
  Env* letstar_env = env_make(*env);

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

  *env = letstar_env;
  return forms;
}

MalType eval_if(list lst, Env** env) {

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

  MalType condition = EVAL(raw_condition, *env);

  if (is_error(condition)) {
    return condition;
  }

  if (is_false(condition) || is_nil(condition)) {

    /* check whether false branch is present */
    if(else_form) {
      return else_form;
    }
    else {
      *env = NULL;              // prevent TCO
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

    MalType val = EVAL(lst->data, env);

    /* return error early */
    if (is_error(val)) {
      return val;
    }
    lst = lst->next;
  }
  /* return the last form for TCE evaluation */
  return lst->data;
}

MalType eval_quote(list lst) {

  MalType form = eat_argument(lst);
  check_empty(lst);
  return form;
}

MalType eval_quasiquote(list lst) {
  MalType form = eat_argument(lst);
  check_empty(lst);
  return quasiquote(form);
}

MalType quasiquote(MalType ast) {

  /* argument to quasiquote is a vector: (quasiquote [first rest]) */
  if (is_vector(ast)) {

    return quasiquote_vector(ast->value.mal_list);
  }

  /* argument to quasiquote is a list: (quasiquote (first rest)) */
  else if (is_list(ast)){

    list lst = ast->value.mal_list;
    if(lst) {
      MalType first = lst->data;
      if(is_symbol(first)
         && !strcmp(first->value.mal_string, SYMBOL_UNQUOTE)) {
        lst = lst->next;
        MalType unquoted = eat_argument(lst);
        check_empty(lst);
        return unquoted;
      }
    }
    return quasiquote_list(lst);
  }
  /* argument to quasiquote is not self-evaluating and isn't sequential: (quasiquote val)
     => (quote val) */
  else if(is_hashmap(ast) || is_symbol(ast)) {

    list lst = NULL;
    lst = list_push(lst, ast);
    lst = list_push(lst, make_symbol("quote"));
    return make_list(lst);
  }
  /* argument to quasiquote is self-evaluating: (quasiquote val)
     => val */
  else {
    return ast;
  }
}

MalType quasiquote_vector(list args) {

  MalType result = quasiquote_list(args);

  if (is_error(result)) {
    return result;
  } else {
    list lst = NULL;
    lst = list_push(lst, result);
    lst = list_push(lst, make_symbol("vec"));

    return make_list(lst);
  }
}

MalType quasiquote_list(list args) {

    /* handle empty list: (quasiquote ())
       => () */
    if (!args) {
      return make_list(NULL);
    }

    MalType first = args->data;

    MalType qq_rest = quasiquote_list(args->next);
    if(is_error(qq_rest)) return qq_rest;

    /* handle splice-unquote: (quasiquote ((splice-unquote first-second) rest))
       => (concat first-second (quasiquote rest)) */
    if(is_list(first)) {
      list lst = first->value.mal_list;
      if(lst) {
        MalType lst_first = lst->data;
        if(is_symbol(lst_first)
           && !strcmp(lst_first->value.mal_string, SYMBOL_SPLICE_UNQUOTE)) {
          lst = lst->next;
          MalType unquoted = eat_argument(lst);
          check_empty(lst);
          return make_list(list_push(list_push(list_push(NULL, qq_rest),
                                               unquoted),
                                     make_symbol("concat")));
        }
      }
    }
    MalType qqted = quasiquote(first);
    if(is_error(qqted)) return qqted;
    return make_list(list_push(list_push(list_push(NULL, qq_rest),
                                         quasiquote(first)),
                               make_symbol("cons")));
}

MalType eval_defmacrobang(list lst, Env* env) {

  MalType defbang_symbol = eat_argument(lst);
  MalType defbang_value = eat_argument(lst);
  check_empty(lst);

  MalType result = EVAL(defbang_value, env);

  if (!is_error(result)) {
    result = make_macro(as_closure(result));
    env_set(env, as_symbol(defbang_symbol), result);
  }
  return result;
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

/* used by core functions but not EVAL as doesn't do TCE */
MalType apply(MalType fn, list args) {

  if (is_function(fn)) {

    MalType(*fun_ptr)(list) = fn->value.mal_function;
    return (*fun_ptr)(args);
  }
  else if(is_closure(fn) || is_macro(fn)) {

    Env* env;
    MalType ast = env_apply(fn->value.mal_closure, args, &env);
    if(is_error(ast)) return ast;
    return EVAL(ast, env);
  }
  else {
    return bad_type(fn, "a function, closure or macro");
  }
}
