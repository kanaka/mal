#include <assert.h>
#include <editline/readline.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <libtcc.h>

#include "printer.h"
#include "reader.h"
#include "util.h"

char program_template[] =
"#include <printer.h>\n"
"#include <reader.h>\n"
"#include <tcclib.h>\n"
"#include <types.h>\n"
"#include <util.h>\n"
"MalType* env_get(MalEnv *env, char *key);\n"
"{{TOP_CODE}}\n"
"MalType* EVAL(MalEnv *env) {\n"
"  {{EVAL_CODE}}\n"
"}";

int compile_eval(MalType *ast, MalEnv *env, int *var_num, MalType* (**EVAL)(MalEnv *env));
int gen_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_call_args_code(MalType *args_name, MalType *node, MalEnv *env, struct codegen *code, int *var_num);
int gen_call_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_hashmap_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_keyword_code(MalType *node, struct codegen *code, int ret);
int gen_list_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
int gen_number_code(MalType *node, struct codegen *code, int ret);
int gen_string_code(MalType *node, struct codegen *code, int ret);
int gen_symbol_code(MalType *node, struct codegen *code, int ret);
int gen_symbol_lookup_code(MalType *node, MalEnv *env, struct codegen *code, int ret);
int gen_vector_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num);
void append_code(MalType *code, MalType *temp_code, int ret);
MalType* next_var_name(char *base, int *var_num);
MalType* env_get(MalEnv *env, char *key);

MalType* READ(char *str) {
  return read_str(str);
}

MalType* EVAL(MalType *ast, MalEnv *repl_env) {
  MalType* (*EVAL)(MalEnv *env);
  int var_num = 1;
  if (compile_eval(ast, repl_env, &var_num, &EVAL)) {
    return EVAL(repl_env);
  } else {
    printf("There was an error compiling.\n");
    return mal_blank_line();
  }
}

char *PATH = NULL;

int compile_eval(MalType *ast, MalEnv *env, int *var_num, MalType* (**EVAL)(MalEnv *env)) {
  struct codegen code = { mal_string(""), mal_string(""), mal_string("") };
  int success = gen_code(ast, env, &code, 1, var_num);

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

  if(access("./tinycc/libtcc.h", F_OK) != -1) {
    tcc_set_lib_path(s, "./tinycc");
    tcc_add_include_path(s, "./tinycc");
    tcc_add_include_path(s, ".");
  } else if(PATH) {
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

  tcc_add_symbol(s, "env_get", env_get);
  tcc_add_symbol(s, "mal_empty", mal_empty);
  tcc_add_symbol(s, "mal_error", mal_error);
  tcc_add_symbol(s, "mal_hashmap", mal_hashmap);
  tcc_add_symbol(s, "mal_hashmap_put", mal_hashmap_put);
  tcc_add_symbol(s, "mal_keyword", mal_keyword);
  tcc_add_symbol(s, "mal_number", mal_number);
  tcc_add_symbol(s, "mal_sprintf", mal_sprintf);
  tcc_add_symbol(s, "mal_string", mal_string);
  tcc_add_symbol(s, "mal_vector", mal_vector);
  tcc_add_symbol(s, "mal_vector_push", mal_vector_push);
  tcc_add_symbol(s, "pr_str", pr_str);

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

int gen_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
  switch (node->type) {
    case MAL_CONS_TYPE:
      return gen_call_code(node, env, code, ret, var_num);
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
    case MAL_NIL_TYPE:
      append_code(code->body, mal_string("mal_nil()"), ret);
      return 1;
    case MAL_NUMBER_TYPE:
      return gen_number_code(node, code, ret);
    case MAL_STRING_TYPE:
      return gen_string_code(node, code, ret);
    case MAL_SYMBOL_TYPE:
      return gen_symbol_lookup_code(node, env, code, ret);
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
    if (!gen_code(mal_car(node), env, &code2, 0, var_num)) {
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
  MalType *sym = mal_car(node), *temp_code;

  // look up the lambda in the env
  MalType *lambda_name = next_var_name("lambda", var_num);
  temp_code = mal_string("MalType *");
  mal_string_append_mal_string(temp_code, lambda_name);
  mal_string_append(temp_code, " = ");
  struct codegen code2 = { code->top, code->decl, temp_code };
  if (!gen_code(sym, env, &code2, 0, var_num)) {
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

  // write the function name or pointer
  temp_code = mal_sprintf(
    "%S->fn(%S->env, %z, %S)",
    lambda_name,
    lambda_name,
    args_count,
    args_name
  );

  append_code(code->body, temp_code, ret);

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
    if (!gen_code(read_str(key_str), env, &code2, 0, var_num)) {
      return 0;
    }
    mal_string_append(temp_code, ", ");
    code2 = (struct codegen){ code->top, code->decl, temp_code };
    if (!gen_code(val, env, &code2, 0, var_num)) {
      return 0;
    }
    mal_string_append(temp_code, ");\n  ");
  }

  append_code(code->decl, temp_code, 0);
  append_code(code->body, var_name, ret);
  return 1;
}

int gen_keyword_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("mal_keyword(%s)", pr_str(mal_string(node->keyword), 1));
  append_code(code->body, temp_code, ret);
  return 1;
}

int gen_list_code(MalType *node, MalEnv *env, struct codegen *code, int ret, int *var_num) {
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
    if (!gen_code(item, env, &code2, 0, var_num)) {
      return 0;
    }
    mal_string_append(temp_decl, ");\n  ");
  }
  append_code(code->decl, temp_decl, 0);
  append_code(code->body, mal_sprintf("mal_vector_to_list(%S)", list_name), ret);
  return 1;
}

int gen_lambda_code(MalType *node, struct codegen *code, int ret) {
  MalType *temp_code = mal_sprintf("(MalType*)%p /* %s */", node, pr_str(node, 1));
  append_code(code->body, temp_code, ret);
  return 1;
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
    "bubble_if_error(env_get(env, %s))",
    pr_str(mal_string(node->symbol), 1)
  );
  append_code(code->body, temp_code, ret);
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
    if (!gen_code(mal_vector_ref(node, i), env, &code2, 0, var_num)) {
      return 0;
    }
    mal_string_append(temp_code, ");\n  ");
  }
  append_code(code->decl, temp_code, 0);
  append_code(code->body, var_name, ret);
  return 1;
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

MalType* env_get(MalEnv *env, char *key) {
  MalType *val = hashmap_get(&env->data, key);
  if (val) {
    return val;
  } else {
    return mal_error(mal_sprintf("'%s' not found", key));
  }
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

MalType* core_add(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  if (argc == 0) {
    return mal_number(0);
  } else {
    long long result = args[0]->number;
    for (size_t i=1; i<argc; i++) {
      result += args[i]->number;
    }
    return mal_number(result);
  }
}

MalType* core_sub(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  assert(argc > 0);
  long long result = args[0]->number;
  for (size_t i=1; i<argc; i++) {
    result -= args[i]->number;
  }
  return mal_number(result);
}

MalType* core_mul(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  if (argc == 0) {
    return mal_number(1);
  } else {
    long long result = args[0]->number;
    for (size_t i=1; i<argc; i++) {
      result *= args[i]->number;
    }
    return mal_number(result);
  }
}

MalType* core_div(MalEnv *env, size_t argc, MalType **args) {
  UNUSED(env);
  assert(argc > 0);
  long long result = args[0]->number;
  for (size_t i=1; i<argc; i++) {
    result /= args[i]->number;
  }
  return mal_number(result);
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
  UNUSED(argc);

  PATH = parent_directory(argv[0]);

  MalEnv repl_env;
  hashmap_init(&repl_env.data, hashmap_hash_string, hashmap_compare_string, 4);
  hashmap_set_key_alloc_funcs(&repl_env.data, hashmap_alloc_key_string, NULL);
  hashmap_put(&repl_env.data, "+", (void*)mal_closure(core_add, &repl_env));
  hashmap_put(&repl_env.data, "-", (void*)mal_closure(core_sub, &repl_env));
  hashmap_put(&repl_env.data, "*", (void*)mal_closure(core_mul, &repl_env));
  hashmap_put(&repl_env.data, "/", (void*)mal_closure(core_div, &repl_env));

  char *buffer;
  read_history("history.txt");
  while ((buffer = readline("user> ")) != NULL) {
    printf("%s\n", rep(buffer, &repl_env));
    if (strlen(buffer) > 0) {
      add_history(buffer);
    }
    free(buffer);
  }
  write_history("history.txt");

  return 0;
}
