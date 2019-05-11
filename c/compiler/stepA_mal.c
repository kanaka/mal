#include <assert.h>
#include <editline/readline.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libtcc.h>

#include "core.h"
#include "env.h"
#include "printer.h"
#include "reader.h"
#include "util.h"

char program_template[] =
"#include <env.h>\n"
"#include <printer.h>\n"
"#include <reader.h>\n"
"#include <tcclib.h>\n"
"#include <types.h>\n"
"#include <util.h>\n"
"{{TOP_CODE}}\n"
"MalType* EVAL(MalEnv *env0) {\n"
"  {{EVAL_CODE}}\n"
"}";

int compile_eval(MalType *ast, MalEnv *env, int *var_num, MalType* (**EVAL)(MalEnv *env));
int gen_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num, int quoting);
int gen_call_args_code(MalType *args_name, MalType *node, MalEnv *env, struct codegen *code, int *var_num);
int gen_call_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_closure_code(MalType *fn_name, MalEnv *env, struct codegen *code, int ret);
int gen_continuation_code(char *prefix, MalType *node, MalEnv *env, struct codegen *code, int ret, int trampoline, int *var_num);
int gen_def_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_do_code(MalType *list, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_fn_code(MalType *fn_name, MalType *node, MalEnv *env, struct codegen *code, int *var_num, int new_env);
int gen_hashmap_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_if_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_keyword_code(MalType *node, struct codegen *code, int ret);
int gen_lambda_code(MalType *node, struct codegen *code, int ret);
int gen_let_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_list_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num, int quoting);
int gen_load_file(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_number_code(MalType *node, struct codegen *code, int ret);
int gen_string_code(MalType *node, struct codegen *code, int ret);
int gen_symbol_code(MalType *node, struct codegen *code, int ret);
int gen_symbol_lookup_code(MalType *node, MalEnv *env, struct codegen *code, int ret);
int gen_try_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_vector_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
MalType* quasiquote(MalType *node);
int is_macro_call(MalType *node, MalEnv *env);
void defmacro(MalType *node, MalEnv *env);
MalType* macroexpand(MalType *ast, MalEnv *env);
void append_code(MalType *code, MalType *temp_code, int ret);
MalType* next_var_name(char *base, int *var_num);
MalType* build_env_name(MalEnv *env);
MalType* trampoline(MalType *result);

MalType* READ(char *str) {
  return read_str(str);
}

MalType* EVAL(MalType *ast, MalEnv *repl_env) {
  MalType* (*EVAL)(MalEnv *env);
  int var_num = 1;
  if (compile_eval(ast, repl_env, &var_num, &EVAL)) {
    return trampoline(EVAL(repl_env));
  } else {
    printf("There was an error compiling.\n");
    return mal_blank_line();
  }
}

MalType* user_eval(MalEnv *repl_env, size_t argc, MalType **args) {
  mal_assert(argc == 1, "Expected 1 argument to eval");
  MalType* (*EVAL)(MalEnv *env);
  MalType *ast = args[0];
  int var_num = 1;
  if (compile_eval(ast, repl_env, &var_num, &EVAL)) {
    return trampoline(EVAL(repl_env));
  } else {
    return mal_error(mal_string("There was an error compiling."));
  }
}

char *PATH = NULL;

int compile_eval(MalType *ast, MalEnv *env, int *var_num, MalType* (**EVAL)(MalEnv *env)) {
  struct codegen code = { mal_string(""), mal_string(""), mal_string("") };
  int success = gen_code(ast, env, &code, 1, var_num, 0);

  if (!success) {
    return 0;
  }

  MalType *generated = mal_string_replace(mal_string(program_template), "{{TOP_CODE}}", code.top->str);

  MalType *combined = code.decl;
  mal_string_append_mal_string(combined, code.body);
  generated = mal_string_replace(generated, "{{EVAL_CODE}}", combined->str);

  if (getenv("DEBUG")) {
    printf("-----------------\n%s-----------------\n", generated->str);
  }

  TCCState *s = tcc_new();
  if (!s) {
    fprintf(stderr, "Could not create tcc state\n");
    return 0;
  }

  if (access("./tinycc/libtcc.h", F_OK) != -1) {
    tcc_set_lib_path(s, "./tinycc");
    tcc_add_include_path(s, "./tinycc");
    tcc_add_include_path(s, ".");
  } else if (PATH) {
    tcc_set_lib_path(s, mal_sprintf("%s/tinycc", PATH)->str);
    tcc_add_include_path(s, mal_sprintf("%s/tinycc", PATH)->str);
    tcc_add_include_path(s, PATH);
  } else {
    printf("Could not determine path to tinycc and libtcc.h\n");
    exit(1);
  }

  tcc_set_output_type(s, TCC_OUTPUT_MEMORY);

  if (tcc_compile_string(s, string(generated->str)) == -1) {
    fprintf(stderr, "Could not compile program\n");
    return 0;
  }

  tcc_add_symbol(s, "build_env", build_env);
  tcc_add_symbol(s, "env_get", env_get);
  tcc_add_symbol(s, "env_set", env_set);
  tcc_add_symbol(s, "mal_blank_line", mal_blank_line);
  tcc_add_symbol(s, "mal_closure", mal_closure);
  tcc_add_symbol(s, "mal_continuation", mal_continuation);
  tcc_add_symbol(s, "mal_continuation_0", mal_continuation_0);
  tcc_add_symbol(s, "mal_continuation_1", mal_continuation_1);
  tcc_add_symbol(s, "mal_empty", mal_empty);
  tcc_add_symbol(s, "mal_error", mal_error);
  tcc_add_symbol(s, "mal_false", mal_false);
  tcc_add_symbol(s, "mal_hashmap", mal_hashmap);
  tcc_add_symbol(s, "mal_hashmap_put", mal_hashmap_put);
  tcc_add_symbol(s, "mal_keyword", mal_keyword);
  tcc_add_symbol(s, "mal_nil", mal_nil);
  tcc_add_symbol(s, "mal_number", mal_number);
  tcc_add_symbol(s, "mal_sprintf", mal_sprintf);
  tcc_add_symbol(s, "mal_string", mal_string);
  tcc_add_symbol(s, "mal_symbol", mal_symbol);
  tcc_add_symbol(s, "mal_true", mal_true);
  tcc_add_symbol(s, "mal_vector", mal_vector);
  tcc_add_symbol(s, "mal_vector_push", mal_vector_push);
  tcc_add_symbol(s, "mal_vector_to_list", mal_vector_to_list);
  tcc_add_symbol(s, "pr_str", pr_str);
  tcc_add_symbol(s, "trampoline", trampoline);

  int size = tcc_relocate(s, NULL);
  if (size < 0) {
    fprintf(stderr, "Could not determine size of program\n");
    return 0;
  }

  void *mem = GC_MALLOC(size);
  if (tcc_relocate(s, mem) < 0) {
    fprintf(stderr, "Could not relocate program\n");
    return 0;
  }

  *EVAL = tcc_get_symbol(s, "EVAL");
  tcc_delete(s);

  if (!*EVAL) {
    fprintf(stderr, "Could not find symbol EVAL\n");
    return 0;
  }

  return 1;
}

int gen_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num, int quoting) {
  switch (node->type) {
    case MAL_CONS_TYPE:
      if (quoting) {
        return gen_list_code(node, env, code, ret, var_num, quoting);
      } else {
        return gen_call_code(node, env, code, ret, var_num);
      }
    case MAL_EMPTY_TYPE:
      append_code(code->body, mal_string("mal_empty()"), ret);
      return 1;
    case MAL_FALSE_TYPE:
      append_code(code->body, mal_string("mal_false()"), ret);
      return 1;
    case MAL_HASHMAP_TYPE:
      return gen_hashmap_code(node, env, code, ret, var_num);
    case MAL_KEYWORD_TYPE:
      return gen_keyword_code(node, code, ret);
    case MAL_LAMBDA_TYPE:
      return gen_lambda_code(node, code, ret);
    case MAL_NIL_TYPE:
      append_code(code->body, mal_string("mal_nil()"), ret);
      return 1;
    case MAL_NUMBER_TYPE:
      return gen_number_code(node, code, ret);
    case MAL_STRING_TYPE:
      return gen_string_code(node, code, ret);
    case MAL_SYMBOL_TYPE:
      if (quoting) {
        return gen_symbol_code(node, code, ret);
      } else {
        return gen_symbol_lookup_code(node, env, code, ret);
      }
    case MAL_TRUE_TYPE:
      append_code(code->body, mal_string("mal_true()"), ret);
      return 1;
    case MAL_VECTOR_TYPE:
      return gen_vector_code(node, env, code, ret, var_num);
    default:
      printf("unknown node type in code gen type=%d\n", node->type);
      return 0;
  }
}

int gen_call_args_code(MalType *args_name, MalType *node, MalEnv *env, struct codegen *code, int *var_num) {
  if (is_empty(node)) {
    append_code(code->decl, mal_sprintf("MalType **%S = NULL;", args_name), 0);
    return 1;
  }
  assert(is_cons(node));
  size_t arg_count = mal_list_len(node), index = 0;
  MalType *temp_code = mal_sprintf("MalType **%S = GC_MALLOC(sizeof(MalType*) * %z);\n  ", args_name, arg_count);
  struct codegen code2;
  while (!is_empty(node)) {
    assert(is_cons(node));
    mal_string_append_mal_string(temp_code, mal_sprintf("%S[%z] = bubble_if_error(", args_name, index));
    code2 = (struct codegen){ code->top, code->decl, temp_code };
    if (!gen_code(mal_car(node), env, &code2, 0, var_num, 0)) {
      return 0;
    }
    mal_string_append(temp_code, ");\n  ");
    node = mal_cdr(node);
    index++;
  }
  append_code(code->decl, temp_code, 0);
  return 1;
}

int gen_call_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  node = macroexpand(node, env);
  if (!is_cons(node)) {
    return gen_code(node, env, code, ret, var_num, 0);
  }

  MalType *sym = mal_car(node), *temp_code, *fn_name;

  if (is_symbol(sym)) {
    if (strcmp(sym->symbol, "def!") == 0) {
      return gen_def_code(mal_cdr(node), env, code, ret, var_num);
    } else if (strcmp(sym->symbol, "let*") == 0) {
      return gen_let_code(mal_cdr(node), env, code, ret, var_num);
    } else if (strcmp(sym->symbol, "do") == 0) {
      return gen_do_code(mal_cdr(node), env, code, ret, var_num);
    } else if (strcmp(sym->symbol, "if") == 0) {
      return gen_if_code(mal_cdr(node), env, code, ret, var_num);
    } else if (strcmp(sym->symbol, "fn*") == 0) {
      fn_name = next_var_name("fn", var_num);
      if (!gen_fn_code(fn_name, mal_cdr(node), env, code, var_num, 1)) {
        return 0;
      }
      return gen_closure_code(fn_name, env, code, ret);
    } else if (strcmp(sym->symbol, "quote") == 0) {
      return gen_code(mal_car(mal_cdr(node)), env, code, ret, var_num, 1);
    } else if (strcmp(sym->symbol, "quasiquote") == 0) {
      return gen_code(quasiquote(mal_car(mal_cdr(node))), env, code, ret, var_num, 0);
    } else if (strcmp(sym->symbol, "defmacro!") == 0) {
      defmacro(mal_cdr(node), env);
      append_code(code->body, mal_string("mal_blank_line()"), ret);
      return 1;
    } else if (strcmp(sym->symbol, "macroexpand") == 0) {
      node = macroexpand(mal_car(mal_cdr(node)), env);
      return gen_code(node, env, code, ret, var_num, 1);
    } else if (strcmp(sym->symbol, "try*") == 0) {
      return gen_try_code(mal_cdr(node), env, code, ret, var_num);
    } else if (strcmp(sym->symbol, "load-file") == 0) {
      return gen_load_file(mal_car(mal_cdr(node)), env, code, ret, var_num);
    }
  }

  // look up the lambda in the env
  MalType *lambda_name = next_var_name("lambda", var_num);
  temp_code = mal_string("MalType *");
  mal_string_append_mal_string(temp_code, lambda_name);
  mal_string_append(temp_code, " = ");
  struct codegen code2 = { code->top, code->decl, temp_code };
  if (!gen_code(sym, env, &code2, 0, var_num, 0)) {
    return 0;
  }
  mal_string_append(temp_code, ";\n  ");
  mal_string_append_mal_string(
    temp_code,
    mal_sprintf(
      "if (!is_lambda(%S)) { return mal_error(mal_sprintf(\"%%s is not callable\", pr_str(%S, 1))); }\n  ",
      lambda_name,
      lambda_name
    )
  );
  append_code(code->decl, temp_code, 0);

  // build the args array
  MalType *args_name, *args_node = mal_cdr(node);
  size_t args_count = mal_list_len(args_node);
  if (args_count == 0) {
    args_name = mal_string("NULL");
  } else {
    args_name = mal_string_replace(lambda_name, "lambda", "args");
    if (!gen_call_args_code(args_name, args_node, env, code, var_num)) {
      return 0;
    }
  }

  // return the functional call as a continuation
  temp_code = mal_sprintf(
    "mal_continuation(%S->fn, %S->env, %z, %S)",
    lambda_name,
    lambda_name,
    args_count,
    args_name
  );

  append_code(
    code->body,
    ret ? temp_code : mal_sprintf("trampoline(%S)", temp_code),
    ret
  );

  return 1;
}

int gen_closure_code(MalType *fn_name, MalEnv *env, struct codegen *code, int ret) {
  MalType *closure_name = mal_string("closure_");
  mal_string_append_mal_string(closure_name, fn_name);
  MalType *temp_code = mal_sprintf(
    "MalType *%S = mal_closure(%S, %S);\n  ",
    closure_name,
    fn_name,
    build_env_name(env)
  );
  append_code(code->decl, temp_code, 0);
  append_code(code->body, closure_name, ret);
  return 1;
}

int gen_continuation_code(char *prefix, MalType *node, MalEnv *env, struct codegen *code, int ret, int trampoline, int *var_num) {
  MalType *fn = mal_cons(mal_empty(), mal_cons(node, mal_empty()));
  MalType *fn_name = next_var_name(prefix, var_num);
  if (!gen_fn_code(fn_name, fn, env, code, var_num, 0)) {
    return 0;
  }
  MalType *temp_code = mal_string("");
  append_code(temp_code, mal_sprintf("mal_continuation(%S, %S, 0, NULL)", fn_name, build_env_name(env)), 0);
  append_code(code->body, trampoline ? temp_code : mal_sprintf("trampoline(%S)", temp_code), ret);
  return 1;
}

int gen_def_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalType *env_name = build_env_name(env);
  MalType *name = mal_car(node);
  assert(is_symbol(name));
  char *name_str = pr_str(mal_string(name->symbol), 1);

  int key_already_exists = !!env_get(env, name->symbol);
  if (!key_already_exists) env_set(env, name->symbol, mal_true());
  struct codegen code2 = (struct codegen){ code->top, code->decl, mal_string("") };
  MalType *val_node = mal_car(mal_cdr(node));
  if (!gen_code(val_node, env, &code2, 0, var_num, 0)) {
    if (!key_already_exists) env_delete(env, name->symbol);
    return 0;
  }

  MalType *temp_code = mal_sprintf("env_set(%S, %s, bubble_if_error(%S))", env_name, name_str, code2.body);
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_do_code(MalType *list, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  struct list_or_vector_iter *iter;
  struct codegen code2;
  MalType *node, *temp_decl, *temp_code;
  int is_last = 0;
  for (iter = list_or_vector_iter(list); iter; iter = list_or_vector_iter_next(iter)) {
    temp_decl = mal_string("");
    temp_code = mal_string("");
    node = list_or_vector_iter_get_obj(iter);
    is_last = list_or_vector_iter_is_last(iter);
    code2 = (struct codegen){ code->top, temp_decl, temp_code };
    if (is_last) {
      if (!gen_continuation_code("do", node, env, &code2, ret, ret, var_num)) {
        return 0;
      }
    } else {
      if (!gen_code(node, env, &code2, 0, var_num, 0)) {
        return 0;
      }
    }
    if (is_last) {
      append_code(code->decl, temp_decl, 0);
      append_code(code->body, temp_code, 0);
    } else {
      mal_string_append(temp_code, ";\n  ");
      append_code(code->decl, temp_decl, 0);
      append_code(code->decl, temp_code, 0);
    }
  }
  return 1;
}

int gen_fn_code(MalType *fn_name, MalType *node, MalEnv *env, struct codegen *code, int *var_num, int new_env) {
  MalType *args = mal_car(node);
  size_t arg_count = list_or_vector_len(args);

  // function header
  MalType *temp_fn = mal_sprintf("MalType* %S(MalEnv *env, size_t argc, MalType **args) {\n  ", fn_name);

  // inner env
  MalEnv *inner_env;
  MalType *inner_env_name;
  if (new_env) {
    inner_env = build_env(env);
    inner_env->num = (*var_num)++;
    inner_env_name = build_env_name(inner_env);
    mal_string_append_mal_string(temp_fn, mal_sprintf("MalEnv *%S = build_env(env);\n  ", inner_env_name));
  } else {
    inner_env = env;
    inner_env_name = build_env_name(env);
    mal_string_append_mal_string(temp_fn, mal_sprintf("MalEnv *%S = env;\n  ", inner_env_name));
  }

  // arity check
  int more_pos = list_or_vector_index_of(args, mal_symbol("&"));
  if (more_pos == -1) {
    mal_string_append_mal_string(
      temp_fn,
      mal_sprintf("mal_assert(argc == %i, \"Arity mismatch.\");\n  ", arg_count)
    );
  } else {
    mal_string_append_mal_string(
      temp_fn,
      mal_sprintf("mal_assert(argc >= %i, \"Arity mismatch.\");\n  ", more_pos)
    );
  }

  // set arguments in inner env
  if (arg_count > 0) {
    struct list_or_vector_iter *iter;
    MalType *arg;
    size_t index = 0;
    int is_more = 0;
    for (iter = list_or_vector_iter(args); iter; iter = list_or_vector_iter_next(iter)) {
      arg = list_or_vector_iter_get_obj(iter);
      assert(is_symbol(arg));
      if (strcmp("&", arg->symbol) == 0) {
        is_more = 1;
        continue;
      }
      if (is_more) {
        // accumulate remaining args in a list
        char *more_name = arg->symbol;
        mal_string_append(temp_fn, "MalType *rest_args = mal_vector();\n  ");
        mal_string_append_mal_string(
          temp_fn,
          mal_sprintf("for(size_t arg_i=%z; arg_i<argc; arg_i++) {\n  ", index)
        );
        mal_string_append(
          temp_fn,
          "  mal_vector_push(rest_args, args[arg_i]);\n  "
        );
        mal_string_append(temp_fn, "}\n  ");
        mal_string_append_mal_string(
          temp_fn,
          mal_sprintf(
            "env_set(%S, %s, mal_vector_to_list(rest_args));\n  ",
            inner_env_name,
            pr_str(mal_string(more_name), 1)
          )
        );
        env_set(inner_env, more_name, mal_true());
        break;
      } else {
        mal_string_append_mal_string(
          temp_fn,
          mal_sprintf(
            "env_set(%S, %s, args[%z]);\n  ",
            inner_env_name,
            pr_str(mal_string(arg->symbol), 1),
            index
          )
        );
        env_set(inner_env, arg->symbol, mal_true());
        index++;
      }
    }
  }

  // function body
  MalType *fn_decl = mal_string("");
  MalType *fn_code = mal_string("");
  struct codegen code2 = (struct codegen){ code->top, fn_decl, fn_code };
  MalType *fn_body = mal_cdr(node);
  assert(!is_empty(fn_body));
  if (!gen_code(mal_car(fn_body), inner_env, &code2, 1, var_num, 0)) {
    return 0;
  }
  append_code(temp_fn, fn_decl, 0);
  append_code(temp_fn, fn_code, 0);

  // closing brace
  mal_string_append(temp_fn, "\n}\n\n");
  append_code(code->top, temp_fn, 0);

  return 1;
}

int gen_hashmap_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  int len = mal_hashmap_size(node);
  if (len == 0) {
    append_code(code->body, mal_string("mal_hashmap()"), ret);
    return 1;
  }

  MalType *var_name = next_var_name("var", var_num);
  MalType *temp_code = mal_string("MalType *");
  mal_string_append_mal_string(temp_code, var_name);
  mal_string_append(temp_code, " = mal_hashmap();\n  ");

  struct hashmap_iter *iter;
  struct codegen code2;
  char *key_str;
  MalType *val;
  for (iter = hashmap_iter(&node->hashmap); iter; iter = hashmap_iter_next(&node->hashmap, iter)) {
    key_str = (char*)hashmap_iter_get_key(iter);
    val = (MalType*)hashmap_iter_get_data(iter);
    mal_string_append(temp_code, "mal_hashmap_put(");
    mal_string_append_mal_string(temp_code, var_name);
    mal_string_append(temp_code, ", ");
    code2 = (struct codegen){ code->top, code->decl, temp_code };
    if (!gen_code(read_str(key_str), env, &code2, 0, var_num, 0)) {
      return 0;
    }
    mal_string_append(temp_code, ", ");
    code2 = (struct codegen){ code->top, code->decl, temp_code };
    if (!gen_code(val, env, &code2, 0, var_num, 0)) {
      return 0;
    }
    mal_string_append(temp_code, ");\n  ");
  }

  append_code(code->decl, temp_code, 0);
  append_code(code->body, var_name, ret);
  return 1;
}

int gen_if_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalType *cond_code = mal_string("");
  struct codegen code2 = (struct codegen){ code->top, code->decl, cond_code };
  if (!gen_code(mal_car(node), env, &code2, 0, var_num, 0)) {
    return 0;
  }
  node = mal_cdr(node);
  MalType *true_code = mal_string("");
  code2.body = true_code;
  MalType *true_val = mal_car(node);
  if (is_primitive(true_val)) {
    if (!gen_code(true_val, env, &code2, 0, var_num, 0)) {
      return 0;
    }
  } else {
    if (!gen_continuation_code("if_true", true_val, env, &code2, 0, ret, var_num)) {
      return 0;
    }
  }
  node = mal_cdr(node);
  MalType *false_code = mal_string("");
  code2.body = false_code;
  MalType *false_val;
  if (is_empty(node)) {
    false_val = mal_nil();
  } else {
    false_val = mal_car(node);
  }
  if (is_primitive(false_val)) {
    if (!gen_code(false_val, env, &code2, 0, var_num, 0)) {
      return 0;
    }
  } else {
    if (!gen_continuation_code("if_false", false_val, env, &code2, 0, ret, var_num)) {
      return 0;
    }
  }
  append_code(
    code->body,
    mal_sprintf("is_truthy(%S) ? %S : %S", cond_code, true_code, false_code),
    ret
  );
  return 1;
}

int gen_keyword_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("mal_keyword(%s)", pr_str(mal_string(node->keyword), 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_lambda_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("(MalType*)%p /* %s */", node, pr_str(node, 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_let_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalEnv *inner_env = build_env(env);
  inner_env->num = (*var_num)++;
  MalType *outer_env_name = build_env_name(env);
  MalType *inner_env_name = build_env_name(inner_env);
  MalType *temp_code = mal_sprintf("MalEnv *%S = build_env(%S);\n  ", inner_env_name, outer_env_name);
  append_code(code->decl, temp_code, 0);

  temp_code = mal_string("");

  struct list_or_vector_iter *iter;
  struct codegen code2;
  MalType *name, *val, *inner_decl, *val_code;
  for (iter = list_or_vector_iter(mal_car(node)); iter; iter = list_or_vector_iter_next(iter)) {
    inner_decl = mal_string("");
    name = list_or_vector_iter_get_obj(iter);
    assert(is_symbol(name));
    iter = list_or_vector_iter_next(iter);
    val = list_or_vector_iter_get_obj(iter);
    env_set(inner_env, name->symbol, mal_true());
    code2 = (struct codegen){ code->top, inner_decl, mal_string("") };
    if (!gen_code(val, inner_env, &code2, 0, var_num, 0)) {
      return 0;
    }
    val_code = mal_sprintf("env_set(%S, %s, %S);\n  ", inner_env_name, pr_str(mal_string(name->symbol), 1), code2.body);
    mal_string_append_mal_string(temp_code, inner_decl);
    mal_string_append_mal_string(temp_code, val_code);
  }

  append_code(code->decl, temp_code, 0);

  node = mal_cdr(node);
  if (!gen_continuation_code("let", mal_car(node), inner_env, code, ret, ret, var_num)) {
    return 0;
  }

  return 1;
}

int gen_list_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num, int quoting) {
  MalType *list_name = next_var_name("list", var_num);
  MalType *temp_decl = mal_sprintf("MalType *%S = mal_vector();\n  ", list_name);
  struct list_or_vector_iter *iter;
  MalType *item;
  struct codegen code2;
  for (iter = list_or_vector_iter(node); iter; iter = list_or_vector_iter_next(iter)) {
    item = list_or_vector_iter_get_obj(iter);
    mal_string_append_mal_string(
      temp_decl,
      mal_sprintf("mal_vector_push(%S, ", list_name)
    );
    code2 = (struct codegen){ code->top, code->decl, temp_decl };
    if (!gen_code(item, env, &code2, 0, var_num, quoting)) {
      return 0;
    }
    mal_string_append(temp_decl, ");\n  ");
  }
  append_code(code->decl, temp_decl, 0);
  append_code(code->body, mal_sprintf("mal_vector_to_list(%S)", list_name), ret);
  return 1;
}

int gen_load_file(MalType *filename_node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalType *contents = core_slurp(env, 1, &filename_node);
  MalType *ast = read_str(mal_sprintf("(do %S)", contents)->str);
  return gen_code(ast, env, code, ret, var_num, 0);
}

int gen_number_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("mal_number(%s)", pr_str(node, 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_string_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("mal_string(%s)", pr_str(node, 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_symbol_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("mal_symbol(%s)", pr_str(mal_string(node->symbol), 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_symbol_lookup_code(MalType *node, MalEnv *env, struct codegen *code, int ret) {
  UNUSED(env);
  MalType *temp_code = mal_sprintf(
    "bubble_if_error(env_get(%S, %s))",
    build_env_name(env),
    pr_str(mal_string(node->symbol), 1)
  );
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_try_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalType *try_expr = mal_car(node); // (A (catch* B C))
  MalType *catch_node_list = mal_cdr(node); // ((catch* B C))
  if (is_empty(catch_node_list)) {
    return gen_code(try_expr, env, code, ret, var_num, 0);
  }
  MalType *try_as_lambda_expr = mal_cons(
    mal_empty(), // fn args (none)
    mal_cons(try_expr, mal_empty()) // fn body
  );
  MalType *try_fn_name = next_var_name("try", var_num);
  if (!gen_fn_code(try_fn_name, try_as_lambda_expr, env, code, var_num, 1)) {
    return 0;
  }
  MalType *catch_node = mal_car(catch_node_list); // (catch* B C)
  assert(is_cons(catch_node));
  MalType *catch_sym = mal_car(catch_node); // catch*
  assert(is_symbol(catch_sym) && strcmp(catch_sym->symbol, "catch*") == 0);
  MalType *arg_name = mal_car(mal_cdr(catch_node)); // B
  assert(is_symbol(arg_name));
  MalType *catch_expr = mal_car(mal_cdr(mal_cdr(catch_node))); // C
  MalType *catch_fn_name = next_var_name("catch", var_num);
  MalType *catch_as_lambda_expr = mal_cons(
    mal_cons(arg_name, mal_empty()), // fn arg
    mal_cons(catch_expr, mal_empty()) // fn body
  );
  if (!gen_fn_code(catch_fn_name, catch_as_lambda_expr, env, code, var_num, 1)) {
    return 0;
  }
  MalType *result_name = next_var_name("try_result", var_num);
  MalType *try_call_code = mal_sprintf("trampoline(mal_continuation_0(%S, %S))", try_fn_name, build_env_name(env));
  MalType *catch_call_code = mal_sprintf("mal_continuation_1(%S, %S, %S->error_val)", catch_fn_name, build_env_name(env), result_name);
  if (!ret) {
    catch_call_code = mal_sprintf("trampoline(%S)", catch_call_code);
  }
  mal_string_append_mal_string(
    code->decl,
    mal_sprintf("MalType *%S = %S;\n  ", result_name, try_call_code)
  );
  append_code(
    code->body,
    mal_sprintf("is_error(%S) ? %S : %S", result_name, catch_call_code, result_name),
    ret
  );
  return 1;
}

int gen_vector_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  MalType *var_name = next_var_name("var", var_num);
  MalType *temp_code = mal_string("MalType *");
  mal_string_append_mal_string(temp_code, var_name);
  mal_string_append(temp_code, " = mal_vector();\n  ");
  struct codegen code2;
  for(size_t i=0; i<mal_vector_len(node); i++) {
    mal_string_append(temp_code, "mal_vector_push(");
    mal_string_append_mal_string(temp_code, var_name);
    mal_string_append(temp_code, ", ");
    code2 = (struct codegen){ code->top, code->decl, temp_code };
    if (!gen_code(mal_vector_ref(node, i), env, &code2, 0, var_num, 0)) {
      return 0;
    }
    mal_string_append(temp_code, ");\n  ");
  }
  append_code(code->decl, temp_code, 0);
  append_code(code->body, var_name, ret);
  return 1;
}

MalType* quasiquote(MalType *node) {
  if (!is_pair(node)) {
    return mal_cons(mal_symbol("quote"), mal_cons(node, mal_empty()));
  } else if (is_symbol(mal_car2(node)) && strcmp(mal_car2(node)->symbol, "unquote") == 0) {
    return mal_car2(mal_cdr2(node));
  } else if (is_pair(mal_car2(node)) && is_symbol(mal_car2(mal_car2(node))) && strcmp(mal_car2(mal_car2(node))->symbol, "splice-unquote") == 0) {
    return mal_cons(
      mal_symbol("concat"),
      mal_cons(
        mal_car2(mal_cdr2(mal_car2(node))),
        mal_cons(
          quasiquote(mal_cdr2(node)),
          mal_empty()
        )
      )
    );
  } else {
    return mal_cons(
      mal_symbol("cons"),
      mal_cons(
        quasiquote(mal_car2(node)),
        mal_cons(
          quasiquote(mal_cdr2(node)),
          mal_empty()
        )
      )
    );
  }
}

int is_macro_call(MalType *node, MalEnv *env) {
  if (!is_cons(node)) {
    return 0;
  }
  MalType *sym = mal_car(node);
  if (!is_symbol(sym)) {
    return 0;
  }
  MalType *macro = env_get(env, sym->symbol);
  if (macro && macro->is_macro) {
    return 1;
  } else {
    return 0;
  }
}

void defmacro(MalType *node, MalEnv *env) {
  int var_num = 1;
  MalType* (*EVAL)(MalEnv *env);
  MalType *name = mal_car(node);
  assert(is_symbol(name));
  node = mal_car(mal_cdr(node));
  if (!compile_eval(node, env, &var_num, &EVAL)) {
    printf("Error compiling macro.\n");
    exit(1);
  }
  MalType *macro = trampoline(EVAL(env));
  macro->is_macro = 1;
  env_set(env, name->symbol, macro);
}

void inspect_env(MalEnv *env) {
  char *key_str, *val_str;
  struct hashmap_iter *iter;
  for (iter = hashmap_iter(&env->data); iter; iter = hashmap_iter_next(&env->data, iter)) {
    key_str = (char*)hashmap_iter_get_key(iter);
    val_str = pr_str((MalType*)hashmap_iter_get_data(iter), 1);
    printf("%s=%s\n", key_str, val_str);
  }
}

MalType* macroexpand(MalType *ast, MalEnv *env) {
  MalType *name, *macro, *arg_list, **args;
  size_t argc;
  while (is_macro_call(ast, env)) {
    name = mal_car(ast);
    macro = env_get(env, name->symbol);
    arg_list = mal_cdr(ast);
    argc = mal_list_len(arg_list);
    if (argc > 0) {
      args = GC_MALLOC(sizeof(MalType*) * argc);
      for (size_t i=0; i<argc; i++) {
        args[i] = mal_car(arg_list);
        arg_list = mal_cdr(arg_list);
      }
    } else {
      args = NULL;
    }
    ast = trampoline(mal_continuation(macro->fn, macro->env, argc, args));
    assert(ast != NULL);
  }
  return ast;
}

void append_code(MalType *code, MalType *temp_code, int ret) {
  if (ret) mal_string_append(code, "return ");
  mal_string_append_mal_string(code, temp_code);
  if (ret) mal_string_append(code, ";");
}

MalType* next_var_name(char *base, int *var_num) {
  MalType *var_name = mal_string(base);
  mal_string_append_long_long(var_name, (*var_num)++);
  return var_name;
}

MalType* build_env_name(MalEnv *env) {
  MalType *name = mal_string("env");
  mal_string_append_long_long(name, env->num);
  return name;
}

char* PRINT(MalType *ast) {
  return pr_str(ast, 2);
}

char* rep(char *str, MalEnv *repl_env) {
  MalType *result = EVAL(READ(str), repl_env);
  if (is_error(result)) {
    return PRINT(mal_sprintf("ERROR: %s\n", pr_str(result->error_val, 0)));
  } else {
    return PRINT(result);
  }
}

char *parent_directory(char *bin_path) {
  char *path = string(bin_path);
  size_t len;
  while ((len = strlen(path)) > 0 && path[len - 1] != '/') {
    path[len - 1] = 0;
  }
  if (len > 0) path[len - 1] = 0;
  if (strlen(path) == 0) {
    return string(".");
  } else {
    return path;
  }
}

int main(int argc, char *argv[]) {
  PATH = parent_directory(argv[0]);

  MalEnv *repl_env = build_top_env();

  struct hashmap *ns = core_ns();
  struct hashmap_iter *core_iter;
  char *name;
  MalType* (*fn)(MalEnv*, size_t, MalType**);
  for (core_iter = hashmap_iter(ns); core_iter; core_iter = hashmap_iter_next(ns, core_iter)) {
    name = (char*)hashmap_iter_get_key(core_iter);
    fn = hashmap_iter_get_data(core_iter);
    env_set(repl_env, name, mal_builtin_function(fn, name, repl_env));
  }
  env_set(repl_env, "eval", mal_builtin_function(user_eval, "eval", repl_env));

  MalType *arg_list = mal_vector();
  for (int i=2; i<argc; i++) {
    mal_vector_push(arg_list, mal_string(argv[i]));
  }
  env_set(repl_env, "*ARGV*", mal_vector_to_list(arg_list));

  env_set(repl_env, "*host-language*", mal_string("malcc"));

  rep(
    mal_sprintf(
      "(do"
        "(def! *gensym-counter* (atom 0))"
        "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"
        "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))"
        "(def! not (fn* (a) (if a false true)))"
        "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs))))))))",
      pr_str(arg_list, 1)
    )->str,
    repl_env
  );

  if (argc > 1) {
    rep(mal_sprintf("(load-file %s)", pr_str(mal_string(argv[1]), 1))->str, repl_env);
  } else {
    rep("(println (str \"Mal [\" *host-language* \"]\"))", repl_env);
    char *buffer;
    read_history("history.txt");
    while ((buffer = readline("user> ")) != NULL) {
      printf("%s\n", rep(buffer, repl_env));
      if (strlen(buffer) > 0) {
        add_history(buffer);
      }
      free(buffer);
    }
    write_history("history.txt");
  }

  return 0;
}
