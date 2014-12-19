#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "types.h"
#include "readline.h"
#include "reader.h"

// Declarations
MalVal *EVAL(MalVal *ast, Env *env);

// read
MalVal *READ(char prompt[], char *str) {
    char *line;
    MalVal *ast;
    if (str) {
        line = str;
    } else {
        line = _readline(prompt);
        if (!line) {
            _error("EOF");
            return NULL;
        }
    }
    ast = read_str(line);
    if (!str) { free(line); }
    return ast;
}

// eval
MalVal *eval_ast(MalVal *ast, Env *env) {
    if (!ast || mal_error) return NULL;
    if (ast->type == MAL_SYMBOL) {
        //g_print("EVAL symbol: %s\n", ast->val.string);
        return env_get(env, ast);
    } else if ((ast->type == MAL_LIST) || (ast->type == MAL_VECTOR)) {
        //g_print("EVAL sequential: %s\n", _pr_str(ast,1));
        MalVal *el = _map2((MalVal *(*)(void*, void*))EVAL, ast, env);
        if (!el || mal_error) return NULL;
        el->type = ast->type;
        return el;
    } else if (ast->type == MAL_HASH_MAP) {
        //g_print("EVAL hash_map: %s\n", _pr_str(ast,1));
        GHashTableIter iter;
        gpointer key, value;
        MalVal *seq = malval_new_list(MAL_LIST,
                                    g_array_sized_new(TRUE, TRUE, sizeof(MalVal*),
                                                        _count(ast)));
        g_hash_table_iter_init (&iter, ast->val.hash_table);
        while (g_hash_table_iter_next (&iter, &key, &value)) {
            MalVal *kname = malval_new_string((char *)key);
            g_array_append_val(seq->val.array, kname);
            MalVal *new_val = EVAL((MalVal *)value, env);
            g_array_append_val(seq->val.array, new_val);
        }
        return _hash_map(seq);
    } else {
        //g_print("EVAL scalar: %s\n", _pr_str(ast,1));
        return ast;
    }
}

MalVal *EVAL(MalVal *ast, Env *env) {
    if (!ast || mal_error) return NULL;
    //g_print("EVAL: %s\n", _pr_str(ast,1));
    if (ast->type != MAL_LIST) {
        return eval_ast(ast, env);
    }
    if (!ast || mal_error) return NULL;

    // apply list
    //g_print("EVAL apply list: %s\n", _pr_str(ast,1));
    int i, len;
    if (_count(ast) == 0) { return ast; }
    MalVal *a0 = _nth(ast, 0);
    assert_type(a0, MAL_SYMBOL, "Cannot apply %s", _pr_str(a0,1));
    if (strcmp("def!", a0->val.string) == 0) {
        //g_print("eval apply def!\n");
        MalVal *a1 = _nth(ast, 1),
               *a2 = _nth(ast, 2);
        MalVal *res = EVAL(a2, env);
        env_set(env, a1, res);
        return res;
    } else if (strcmp("let*", a0->val.string) == 0) {
        //g_print("eval apply let*\n");
        MalVal *a1 = _nth(ast, 1),
               *a2 = _nth(ast, 2),
               *key, *val;
        assert_type(a1, MAL_LIST|MAL_VECTOR,
                    "let* bindings must be list or vector");
        len = _count(a1);
        assert((len % 2) == 0, "odd number of let* bindings forms");
        Env *let_env = new_env(env, NULL, NULL);
        for(i=0; i<len; i+=2) {
            key = g_array_index(a1->val.array, MalVal*, i);
            val = g_array_index(a1->val.array, MalVal*, i+1);
            assert_type(key, MAL_SYMBOL, "let* bind to non-symbol");
            env_set(let_env, key, EVAL(val, let_env));
        }
        return EVAL(a2, let_env);
    } else {
        //g_print("eval apply\n");
        MalVal *el = eval_ast(ast, env);
        if (!el || mal_error) { return NULL; }
        MalVal *(*f)(void *, void*) = (MalVal *(*)(void*, void*))_first(el);
        return f(_nth(el, 1), _nth(el, 2));
    }
}

// print
char *PRINT(MalVal *exp) {
    if (mal_error) {
        fprintf(stderr, "Error: %s\n", mal_error->val.string);
        malval_free(mal_error);
        mal_error = NULL;
        return NULL;
    }
    return _pr_str(exp,1);
}

// repl

// read and eval
MalVal *RE(Env *env, char *prompt, char *str) {
    MalVal *ast, *exp;
    ast = READ(prompt, str);
    if (!ast || mal_error) return NULL;
    exp = EVAL(ast, env);
    if (ast != exp) {
        malval_free(ast);    // Free input structure
    }
    return exp;
}

// Setup the initial REPL environment
Env *repl_env;

void init_repl_env() {
    repl_env = new_env(NULL, NULL, NULL);

    WRAP_INTEGER_OP(plus,+)
    WRAP_INTEGER_OP(minus,-)
    WRAP_INTEGER_OP(multiply,*)
    WRAP_INTEGER_OP(divide,/)

    env_set(repl_env, malval_new_symbol("+"), (MalVal *)int_plus);
    env_set(repl_env, malval_new_symbol("-"), (MalVal *)int_minus);
    env_set(repl_env, malval_new_symbol("*"), (MalVal *)int_multiply);
    env_set(repl_env, malval_new_symbol("/"), (MalVal *)int_divide);
}

int main()
{
    MalVal *exp;
    char *output;
    char prompt[100];

    // Set the initial prompt and environment
    snprintf(prompt, sizeof(prompt), "user> ");
    init_repl_env();
 
    // repl loop
    for(;;) {
        exp = RE(repl_env, prompt, NULL);
        if (mal_error && strcmp("EOF", mal_error->val.string) == 0) {
            return 0;
        }
        output = PRINT(exp);

        if (output) { 
            g_print("%s\n", output);
            free(output);        // Free output string
        }

        //malval_free(exp);    // Free evaluated expression
    }
}
