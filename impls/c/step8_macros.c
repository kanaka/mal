#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "types.h"
#include "readline.h"
#include "reader.h"
#include "core.h"

// Declarations
MalVal *EVAL(MalVal *ast, Env *env);
MalVal *quasiquote(MalVal *ast);

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
    if (!str) { MAL_GC_FREE(line); }
    return ast;
}

// eval
int starts_with(MalVal *ast, const char *sym) {
    if (ast->type != MAL_LIST)
        return 0;
    const MalVal * const a0 = _first(ast);
    return (a0->type & MAL_SYMBOL) && ! strcmp(sym, a0->val.string);
}

MalVal *qq_iter(GArray *xs) {
    MalVal *acc = _listX(0);
    int i;
    for (i=xs->len-1; 0<=i; i--) {
        MalVal * const elt = g_array_index(xs, MalVal*, i);
        if (starts_with(elt, "splice-unquote"))
            acc = _listX(3, malval_new_symbol("concat"), _nth(elt, 1), acc);
        else
            acc = _listX(3, malval_new_symbol("cons"), quasiquote(elt), acc);
    }
    return acc;
}

MalVal *quasiquote(MalVal *ast) {
    switch (ast->type) {
    case MAL_LIST:
        if (starts_with(ast, "unquote"))
            return _nth(ast, 1);
        else
            return qq_iter(ast->val.array);
    case MAL_VECTOR:
        return _listX(2, malval_new_symbol("vec"), qq_iter(ast->val.array));
    case MAL_HASH_MAP:
    case MAL_SYMBOL:
        return _listX(2, malval_new_symbol("quote"), ast);
    default:
        return ast;
    }
}

MalVal *EVAL(MalVal *ast, Env *env) {
    while (TRUE) {

    if (!ast || mal_error) return NULL;

    MalVal *dbgeval = env_get(env, "DEBUG-EVAL");
    if (dbgeval && !(dbgeval->type & (MAL_FALSE|MAL_NIL))) {
        g_print("EVAL: %s\n", _pr_str(ast,1));
    }

    if (ast->type == MAL_SYMBOL) {
        //g_print("EVAL symbol: %s\n", ast->val.string);
        MalVal *res = env_get(env, ast->val.string);
        assert(res, "'%s' not found", ast->val.string);
        return res;
    } else if (ast->type == MAL_LIST) {
        // Proceed after this conditional.
    } else if (ast->type == MAL_VECTOR) {
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

    // apply list
    if (_count(ast) == 0) { return ast; }

    int i, len;
    MalVal *a0 = _nth(ast, 0);
    if ((a0->type & MAL_SYMBOL) &&
        strcmp("def!", a0->val.string) == 0) {
        //g_print("eval apply def!\n");
        MalVal *a1 = _nth(ast, 1),
               *a2 = _nth(ast, 2);
        MalVal *res = EVAL(a2, env);
        if (mal_error) return NULL;
        env_set(env, a1->val.string, res);
        return res;
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("let*", a0->val.string) == 0) {
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
            env_set(let_env, key->val.string, EVAL(val, let_env));
        }
        ast = a2;
        env = let_env;
        // Continue loop
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("quote", a0->val.string) == 0) {
        //g_print("eval apply quote\n");
        return _nth(ast, 1);
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("quasiquote", a0->val.string) == 0) {
        //g_print("eval apply quasiquote\n");
        MalVal *a1 = _nth(ast, 1);
        ast = quasiquote(a1);
        // Continue loop
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("defmacro!", a0->val.string) == 0) {
        //g_print("eval apply defmacro!\n");
        MalVal *a1 = _nth(ast, 1),
               *a2 = _nth(ast, 2);
        MalVal *old = EVAL(a2, env);
        if (mal_error) return NULL;
        MalVal *res = malval_new(MAL_FUNCTION_MAL, NULL);
        res->val.func = old->val.func;
        res->ismacro = TRUE;
        env_set(env, a1->val.string, res);
        return res;
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("do", a0->val.string) == 0) {
        //g_print("eval apply do\n");
        _map2((MalVal *(*)(void*, void*))EVAL, _slice(ast, 1, _count(ast)-1), env);
        ast = _last(ast);
        // Continue loop
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("if", a0->val.string) == 0) {
        //g_print("eval apply if\n");
        MalVal *a1 = _nth(ast, 1);
        MalVal *cond = EVAL(a1, env);
        if (!cond || mal_error) return NULL;
        if (cond->type & (MAL_FALSE|MAL_NIL)) {
            // eval false slot form
            if (ast->val.array->len > 3) {
                ast = _nth(ast, 3);
            } else {
                return &mal_nil;
            }
        } else {
            // eval true slot form
            ast = _nth(ast, 2);
        }
        // Continue loop
    } else if ((a0->type & MAL_SYMBOL) &&
               strcmp("fn*", a0->val.string) == 0) {
        //g_print("eval apply fn*\n");
        MalVal *mf = malval_new(MAL_FUNCTION_MAL, NULL);
        mf->ismacro = FALSE;
        mf->val.func.evaluator = EVAL;
        mf->val.func.args = _nth(ast, 1);
        mf->val.func.body = _nth(ast, 2);
        mf->val.func.env = env;
        return mf;
    } else {
        //g_print("eval apply\n");
        MalVal *f = EVAL(a0, env);
        if (!f || mal_error) { return NULL; }
        MalVal *rest = _rest(ast);
        if (f->ismacro) {
            ast = _apply(f, rest);
            continue;
        }
        MalVal *args = _map2((MalVal *(*)(void*, void*))EVAL, rest, env);
        if (!args || mal_error) { return NULL; }
        assert_type(f, MAL_FUNCTION_C|MAL_FUNCTION_MAL,
                    "cannot apply '%s'", _pr_str(f,1));
        if (f->type & MAL_FUNCTION_MAL) {
            ast = f->val.func.body;
            env = new_env(f->val.func.env, f->val.func.args, args);
            // Continue loop
        } else {
            return _apply(f, args);
        }
    }

    } // TCO while loop
}

// print
char *PRINT(MalVal *exp) {
    if (mal_error) {
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

MalVal *do_eval(MalVal *ast) { return EVAL(ast, repl_env); }

void init_repl_env(int argc, char *argv[]) {
    repl_env = new_env(NULL, NULL, NULL);

    // core.c: defined using C
    int i;
    for(i=0; i < (sizeof(core_ns) / sizeof(core_ns[0])); i++) {
        env_set(repl_env, core_ns[i].name,
                malval_new_function(core_ns[i].func, core_ns[i].arg_cnt));
    }
    env_set(repl_env, "eval",
            malval_new_function((void*(*)(void *))do_eval, 1));

    MalVal *_argv = _listX(0);
    for (i=2; i < argc; i++) {
        MalVal *arg = malval_new_string(argv[i]);
        g_array_append_val(_argv->val.array, arg);
    }
    env_set(repl_env, "*ARGV*", _argv);

    // core.mal: defined using the language itself
    RE(repl_env, "", "(def! not (fn* (a) (if a false true)))");
    RE(repl_env, "",
       "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))");
    RE(repl_env, "", "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
}

int main(int argc, char *argv[])
{
    MalVal *exp;
    char *output;
    char prompt[100];

    MAL_GC_SETUP();

    // Set the initial prompt and environment
    snprintf(prompt, sizeof(prompt), "user> ");
    init_repl_env(argc, argv);

    if (argc > 1) {
        char *cmd = g_strdup_printf("(load-file \"%s\")", argv[1]);
        RE(repl_env, "", cmd);
        return 0;
    }

    // repl loop
    for(;;) {
        exp = RE(repl_env, prompt, NULL);
        if (mal_error && strcmp("EOF", mal_error->val.string) == 0) {
            return 0;
        }
        output = PRINT(exp);

        if (mal_error) {
            fprintf(stderr, "Error: %s\n", _pr_str(mal_error,1));
            malval_free(mal_error);
            mal_error = NULL;
        } else if (output) {
            puts(output);
            MAL_GC_FREE(output);        // Free output string
        }

        //malval_free(exp);    // Free evaluated expression
    }
}
