#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "types.h"
#include "readline.h"
#include "reader.h"

// Declarations
MalVal *EVAL(MalVal *ast, GHashTable *env);

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
MalVal *eval_ast(MalVal *ast, GHashTable *env) {
    if (!ast || mal_error) return NULL;
    if (ast->type == MAL_SYMBOL) {
        //g_print("EVAL symbol: %s\n", ast->val.string);
        // TODO: check if not found
        return g_hash_table_lookup(env, ast->val.string);
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

MalVal *EVAL(MalVal *ast, GHashTable *env) {
    if (!ast || mal_error) return NULL;
    //g_print("EVAL: %s\n", _pr_str(ast,1));
    if (ast->type != MAL_LIST) {
        return eval_ast(ast, env);
    }
    if (!ast || mal_error) return NULL;

    // apply list
    //g_print("EVAL apply list: %s\n", _pr_str(ast,1));
    if (_count(ast) == 0) { return ast; }
    MalVal *a0 = _nth(ast, 0);
    assert_type(a0, MAL_SYMBOL, "Cannot invoke %s", _pr_str(a0,1));
    MalVal *el = eval_ast(ast, env);
    if (!el || mal_error) { return NULL; }
    MalVal *(*f)(void *, void*) = (MalVal *(*)(void*, void*))_first(el);
    //g_print("eval_invoke el: %s\n", _pr_str(el,1));
    return f(_nth(el, 1), _nth(el, 2));
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
MalVal *RE(GHashTable *env, char *prompt, char *str) {
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
GHashTable *repl_env;


void init_repl_env() {
    repl_env = g_hash_table_new(g_str_hash, g_str_equal);

    WRAP_INTEGER_OP(plus,+)
    WRAP_INTEGER_OP(minus,-)
    WRAP_INTEGER_OP(multiply,*)
    WRAP_INTEGER_OP(divide,/)

    g_hash_table_insert(repl_env, "+", int_plus);
    g_hash_table_insert(repl_env, "-", int_minus);
    g_hash_table_insert(repl_env, "*", int_multiply);
    g_hash_table_insert(repl_env, "/", int_divide);
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
