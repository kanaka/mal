#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
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
#define SYMBOL_DO "do"
#define SYMBOL_IF "if"
#define SYMBOL_FNSTAR "fn*"
#define SYMBOL_QUOTE "quote"
#define SYMBOL_QUASIQUOTE "quasiquote"
#define SYMBOL_QUASIQUOTEEXPAND "quasiquoteexpand"
#define SYMBOL_UNQUOTE "unquote"
#define SYMBOL_SPLICE_UNQUOTE "splice-unquote"
#define SYMBOL_DEFMACROBANG "defmacro!"
#define SYMBOL_MACROEXPAND "macroexpand"
#define SYMBOL_TRYSTAR "try*"
#define SYMBOL_CATCHSTAR "catch*"

#define PROMPT_STRING "user> "

MalType* READ(char* str) {

  return read_str(str);
}

MalType* EVAL(MalType* ast, Env* env) {

  /* forward references */
  MalType* eval_ast(MalType* ast, Env* env);
  MalType* eval_defbang(MalType* ast, Env** env);
  void eval_letstar(MalType** ast, Env** env);
  void eval_if(MalType** ast, Env** env);
  MalType* eval_fnstar(MalType* ast, Env* env);
  MalType* eval_do(MalType* ast, Env* env);
  MalType* eval_quote(MalType* ast);
  MalType* eval_quasiquote(MalType* ast);
  MalType* eval_quasiquoteexpand(MalType* ast);
  MalType* eval_defmacrobang(MalType*, Env** env);
  MalType* eval_macroexpand(MalType* ast, Env* env);
  MalType* macroexpand(MalType* ast, Env* env);
  void eval_try(MalType** ast, Env** env);

  /* Use goto to jump here rather than calling eval for tail-call elimination */
 TCE_entry_point:

  /* NULL */
  if (!ast) { return make_nil(); }

  /* macroexpansion */
  ast = macroexpand(ast, env);
  if (is_error(ast)) { return ast; }

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
      return eval_defbang(ast, &env);
    }
    else if (strcmp(symbol, SYMBOL_LETSTAR) == 0) {

      /* TCE - modify ast and env directly and jump back to eval */
      eval_letstar(&ast, &env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_IF) == 0) {

      /* TCE - modify ast directly and jump back to eval */
      eval_if(&ast, &env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_FNSTAR) == 0) {
      return eval_fnstar(ast, env);
    }
    else if (strcmp(symbol, SYMBOL_DO) == 0) {

      /* TCE - modify ast and env directly and jump back to eval */
      ast = eval_do(ast, env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_QUOTE) == 0) {
      return eval_quote(ast);
    }
    else if (strcmp(symbol, SYMBOL_QUASIQUOTE) == 0) {

      ast = eval_quasiquote(ast);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
    else if (strcmp(symbol, SYMBOL_QUASIQUOTEEXPAND) == 0) {

      list lst = ast->value.mal_list;
      return eval_quasiquote(make_list(lst));
    }
    else if (strcmp(symbol, SYMBOL_DEFMACROBANG) == 0) {
      return eval_defmacrobang(ast, &env);
    }
    else if (strcmp(symbol, SYMBOL_MACROEXPAND) == 0) {
      return eval_macroexpand(ast, env);
    }
    else if (strcmp(symbol, SYMBOL_TRYSTAR) == 0) {

      /* TCE - modify ast and env directly and jump back to eval */
      eval_try(&ast, &env);

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
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

      /* TCE - modify ast and env directly and jump back to eval */
      env = env_make(closure->env, params, evlst->next, closure->more_symbol);
      ast = func->value.mal_closure->definition;

      if (is_error(ast)) { return ast; }
      goto TCE_entry_point;
    }
  }
  else {
    return make_error_fmt("first item in list is not callable: '%s'",   \
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

/* declare as global so it can be accessed by mal_eval */
Env* global_env;

MalType* mal_eval(list args) {

  MalType* ast = args->data;
  return EVAL(ast, global_env);
}

MalType* mal_readline(list args) {

  if (!args || args->next) {
    return make_error("'readline': expected exactly one argument");
  }

  MalType* prompt = args->data;

  if (!is_string(prompt)) {
    return make_error_fmt("'readline': argument is not a string '%s'", \
                          pr_str(prompt, UNREADABLY));
  }

  char* str = readline(prompt->value.mal_string);

  if (str) {
    add_history(str);
    return make_string(str);
  }
  else {
    return make_nil();
  }
}

int main(int argc, char** argv) {

  Env* repl_env = env_make(NULL, NULL, NULL, NULL);
  global_env = repl_env;

  ns* core = ns_make_core();
  hashmap mappings = core->mappings;

  while (mappings) {
    char* symbol = mappings->data;
    MalType*(*function)(list) = mappings->next->data;

    env_set_C_fn(repl_env, symbol, function);

    /* pop symbol and function from hashmap/list */
    mappings = mappings->next->next;
  }

  env_set_C_fn(repl_env, "eval", mal_eval);
  env_set_C_fn(repl_env, "readline", mal_readline);

  /* add functions written in mal - not using rep as it prints the result */
  EVAL(READ("(def! not (fn* (a) (if a false true)))"), repl_env);
  EVAL(READ("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"), repl_env);
  EVAL(READ("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"), repl_env);

  /* make command line arguments available in the environment */
  list lst = NULL;
  for (long i = 2; i < argc; i++) {
    lst = list_push(lst, make_string(argv[i]));
  }
  env_set(repl_env, make_symbol("*ARGV*"), make_list(list_reverse(lst)));
  env_set(repl_env, make_symbol("*host-language*"), make_string("c.2"));

  /* run in script mode if a filename is given */
  if (argc > 1) {

    /* first argument on command line is filename */
    char* load_command = snprintfbuf(1024, "(load-file \"%s\")", argv[1]);
    EVAL(READ(load_command), repl_env);
  }
  /* run in repl mode when no cmd line args */
  else {

    /* Greeting message */
    EVAL(READ("(println (str \"Mal [\" *host-language* \"]\"))"), repl_env);

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

MalType* eval_defbang(MalType* ast, Env** env) {

  list lst = (ast->value.mal_list)->next;

  if (!lst || !lst->next || lst->next->next) {
    return make_error_fmt("'def!': expected exactly two arguments");
  }

  MalType* defbang_symbol = lst->data;

  if (!is_symbol(defbang_symbol)) {
    return make_error_fmt("'def!': expected symbol for first argument");
  }

  MalType* defbang_value = lst->next->data;
  MalType* result = EVAL(defbang_value, *env);

  if (!is_error(result)){
    *env = env_set(*env, defbang_symbol, result);
  }
  return result;
}

void eval_letstar(MalType** ast, Env** env) {

  list lst = (*ast)->value.mal_list;

  if (!lst->next) {
    *ast = make_error("'let*': missing bindings list");
    return;
  }

  MalType* bindings = lst->next->data;
  MalType* forms = lst->next->next ? lst->next->next->data : make_nil();

  if (!is_sequential(bindings)) {
    *ast = make_error("'let*': first argument is not list or vector");
    return;
  }

  list bindings_list = bindings->value.mal_list;
  if (list_count(bindings_list) % 2 == 1) {
    *ast = make_error("'let*': expected an even number of binding pairs");
    return;
  }

  Env* letstar_env = env_make(*env, NULL, NULL, NULL);

  /* evaluate the bindings */
  while(bindings_list) {

    MalType* symbol = bindings_list->data;
    MalType* value = EVAL(bindings_list->next->data, letstar_env);

    /* early return from error */
    if (is_error(value)) {
      *ast = value;
      return;
    }

    env_set(letstar_env, symbol, value);
    bindings_list = bindings_list->next->next;
  }

  *env = letstar_env;
  *ast = forms;
  return;
}

void eval_if(MalType** ast, Env** env) {

  list lst = (*ast)->value.mal_list;

  if (!lst->next || !lst->next->next) {
    *ast = make_error("'if': too few arguments");
    return;
  }

  if (lst->next->next->next && lst->next->next->next->next) {
    *ast = make_error("'if': too many arguments");
    return;
  }

  MalType* condition = EVAL(lst->next->data, *env);

  if (is_error(condition)) {
    *ast = condition;
    return;
  }

  if (is_false(condition) || is_nil(condition)) {

    /* check whether false branch is present */
    if (lst->next->next->next) {
      *ast = lst->next->next->next->data;
      return;
    }
    else {
      *ast = make_nil();
      return;
    }

  } else {
    *ast = lst->next->next->data;
    return;
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
  if (!lst->next) {
    return make_nil();
  }

  /* evaluate all but the last form */
  lst = lst->next;
  while (lst->next) {

    MalType* val = EVAL(lst->data, env);

    /* return error early */
    if (is_error(val)) {
      return val;
    }
    lst = lst->next;
  }
  /* return the last form for TCE evaluation */
  return lst->data;
}

MalType* eval_quote(MalType* ast) {

  list lst = (ast->value.mal_list)->next;

  if (!lst) {
    return make_nil();
  }
  else if (lst->next) {
    return make_error("'quote': expected exactly one argument");
  }
  else {
    return lst->data;
  }
}

MalType* eval_quasiquote(MalType* ast) {

  /* forward reference */
  MalType* quasiquote(MalType* ast);

  list lst = ast->value.mal_list;

  /* no arguments (quasiquote) */
  if (!lst->next) {
    return make_nil();
  }

  /* too many arguments */
  else if (lst->next->next) {
    return make_error("'quasiquote': expected exactly one argument");
  }
  return quasiquote(lst->next->data);
}

MalType* quasiquote(MalType* ast) {

  /* forward references */
  MalType* quasiquote_list(MalType* ast);
  MalType* quasiquote_vector(MalType* ast);

  /* argument to quasiquote is self-evaluating: (quasiquote val)
     => val */
  if (is_self_evaluating(ast)) {
    return ast;
  }

  /* argument to quasiquote is a vector: (quasiquote [first rest]) */
  else if (is_vector(ast)) {

    return quasiquote_vector(ast);
  }

  /* argument to quasiquote is a list: (quasiquote (first rest)) */
  else if (is_list(ast)){

    return quasiquote_list(ast);
  }
  /* argument to quasiquote is not self-evaluating and isn't sequential: (quasiquote val)
     => (quote val) */
  else {

    list lst = list_make(ast);
    lst = list_push(lst, make_symbol("quote"));
    return make_list(lst);
  }
}

MalType* quasiquote_vector(MalType* ast) {

  /* forward references */
  MalType* quasiquote_list(MalType* ast);

  list args = ast->value.mal_list;

  if (args) {

    MalType* first = args->data;

    /* if first element is unquote return quoted */
    if (is_symbol(first) && strcmp(first->value.mal_symbol, SYMBOL_UNQUOTE) == 0) {

      list lst = list_make(ast);
      lst = list_push(lst, make_symbol("quote"));

      return make_list(lst);
    }
  }

  /* otherwise process like a list */

  list lst = list_make(make_symbol("vec"));

  MalType* result = quasiquote_list(ast);

  if (is_error(result)) {
    return result;
  } else {
    lst = list_push(lst, result);
  }

  lst = list_reverse(lst);
  return make_list(lst);
}

MalType* quasiquote_list(MalType* ast) {

    list args = ast->value.mal_list;

    /* handle empty list: (quasiquote ())
       => () */
    if (!args) {
      return make_list(NULL);
    }

    MalType* first = args->data;

    /* handle unquote: (quasiquote (unquote second))
       => second */
     if (is_symbol(first) && strcmp(first->value.mal_symbol, SYMBOL_UNQUOTE) == 0 && args->next) {

      if (args->next->next) {
    	return make_error("'quasiquote': unquote expected exactly one argument");
      }
      else {
    	return args->next->data;
      }
    }

    /* handle splice-unquote: (quasiquote ((splice-unquote first-second) rest))
       => (concat first-second (quasiquote rest)) */
    else if (is_list(first) &&
	     first->value.mal_list != NULL &&
	     is_symbol(first->value.mal_list->data) &&
             strcmp(((MalType*)first->value.mal_list->data)->value.mal_symbol, SYMBOL_SPLICE_UNQUOTE) == 0) {

      if (!first->value.mal_list->next) {
        return make_error("'quasiquote': splice-unquote expected exactly one argument");
      }

      MalType* first_second = first->value.mal_list->next->data;
      list lst = list_make(make_symbol("concat"));
      lst = list_push(lst, first_second);

      MalType* rest = quasiquote(make_list(args->next));
      if (is_error(rest)) {
        return rest;
      }

      lst = list_push(lst, rest);
      lst = list_reverse(lst);

      return make_list(lst);
    }
    /* handle all other lists recursively: (quasiquote (first rest))
       => (cons (quasiquote first) (quasiquote rest)) */
    else {

      list lst = list_make(make_symbol("cons"));

      MalType* first = quasiquote(args->data);
      if (is_error(first)) {
        return first;
      } else {
        lst = list_push(lst, first);
      }

      MalType* rest = quasiquote(make_list(args->next));
      if (is_error(rest)) {
        return rest;
      } else {
        lst = list_push(lst, rest);
      }

      lst = list_reverse(lst);
      return make_list(lst);
    }
}

MalType* eval_defmacrobang(MalType* ast, Env** env) {

  list lst = (ast->value.mal_list)->next;

  if (!lst || !lst->next || lst->next->next) {
    return make_error_fmt("'defmacro!': expected exactly two arguments");
  }

  MalType* defbang_symbol = lst->data;

  if (!is_symbol(defbang_symbol)) {
    return make_error_fmt("'defmacro!': expected symbol for first argument");
  }

  MalType* defbang_value = lst->next->data;
  MalType* result = EVAL(defbang_value, *env);

  if (!is_error(result)) {
    result = copy_type(result);
    result->is_macro = 1;
    *env = env_set(*env, defbang_symbol, result);
  }
  return result;
}

MalType* eval_macroexpand(MalType* ast, Env* env) {

  /* forward reference */
  MalType* macroexpand(MalType* ast, Env* env);

  list lst = ast->value.mal_list;

  if (!lst->next) {
    return make_nil();
  }
  else if (lst->next->next) {
    return make_error("'macroexpand': expected exactly one argument");
  }
  else {
    return macroexpand(lst->next->data, env);
  }
}

MalType* macroexpand(MalType* ast, Env* env) {

  /* forward reference */
  int is_macro_call(MalType* ast, Env* env);

  while(is_macro_call(ast, env)) {

    list lst = ast->value.mal_list;

    MalType* macro_fn = env_get(env, lst->data);
    MalClosure* cls = macro_fn->value.mal_closure;
    MalType* more_symbol = cls->more_symbol;

    list params_list = (cls->parameters)->value.mal_list;
    list args_list = lst->next;

    env = env_make(cls->env, params_list, args_list, more_symbol);
    ast = EVAL(cls->definition, env);
  }
  return ast;
}

void eval_try(MalType** ast, Env** env) {

  list lst = (*ast)->value.mal_list;

  if (!lst->next) {
    *ast = make_nil();
    return;
  }

  if (lst->next->next && lst->next->next->next) {
    *ast = make_error("'try*': expected maximum of two arguments");
    return;
  }

  MalType* try_clause = lst->next->data;
  MalType* try_result = EVAL(try_clause, *env);

  /* no catch* clause */
  if (!is_error(try_result) || !lst->next->next) {
    *ast = try_result;
    return;
  }

  /* process catch* clause */
  MalType* catch_clause = lst->next->next->data;
  list catch_list = catch_clause->value.mal_list;

  if (!catch_list) {
    *ast = make_error("'try*': catch* clause is empty");
    return;
  }

  MalType* catch_symbol = catch_list->data;
  if (strcmp(catch_symbol->value.mal_symbol, SYMBOL_CATCHSTAR) != 0) {
    *ast = make_error("Error: catch clause is missing catch* symbol");
    return;
  }

  if (!catch_list->next || !catch_list->next->next) {
    *ast = make_error("Error: catch* clause expected two arguments");
    return;
  }

  if (!is_symbol(catch_list->next->data)) {
    *ast = make_error("Error: catch* clause expected a symbol");
    return;
  }

  /* bind the symbol to the exception */
  list symbol_list = list_make(catch_list->next->data);
  list expr_list = list_make(try_result->value.mal_error);

  Env* catch_env = env_make(*env, symbol_list, expr_list, NULL);
  *ast = catch_list->next->next->data;
  *env = catch_env;

  return;
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
        return make_error_fmt("duplicate symbol in argument list: '%s'", \
                              pr_str(val, UNREADABLY));
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
  return (((MalType*)data)->value.mal_symbol);
}

/* used by core functions but not EVAL as doesn't do TCE */
MalType* apply(MalType* fn, list args) {

  if (is_function(fn)) {

    MalType* (*fun_ptr)(list) = fn->value.mal_function;
    return (*fun_ptr)(args);
  }
  else { /* is_closure(fn) */

    MalClosure* c = fn->value.mal_closure;
    list params = (c->parameters)->value.mal_list;

    long param_count = list_count(params);
    long arg_count = list_count(args);

    if (param_count > arg_count) {
      return make_error("too few arguments supplied to function");
    }
    else if ((param_count < arg_count) && !c->more_symbol) {
      return make_error("too many arguments supplied to function");
    }
    else {
      Env* env = env_make(c->env, params, args, c->more_symbol);
      return EVAL(fn->value.mal_closure->definition, env);
    }
  }
}

int is_macro_call(MalType* ast, Env* env) {

  /* not a list */
  if (!is_list(ast)) {
    return 0;
  }

  /* empty list */
  list lst = ast->value.mal_list;
  if (!lst) {
    return 0;
  }

  /* first item not a symbol */
  MalType* first = lst->data;
  if (!is_symbol(first)) {
    return 0;
  }

  /* lookup symbol */
  MalType* val = env_get(env, first);
  if (is_error(val)) {
    return 0;
  }
  else {
    return (val->is_macro);
  }
}
