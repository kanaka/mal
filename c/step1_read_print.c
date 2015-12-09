#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "types.h"
#include "readline.h"
#include "reader.h"

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
MalVal *EVAL(MalVal *ast, GHashTable *env) {
    if (!ast || mal_error) return NULL;
    return ast;
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

int main()
{
    MalVal *exp;
    char *output;
    char prompt[100];

    MAL_GC_SETUP();

    // Set the initial prompt
    snprintf(prompt, sizeof(prompt), "user> ");
 
    // repl loop
    for(;;) {
        exp = RE(NULL, prompt, NULL);
        if (mal_error && strcmp("EOF", mal_error->val.string) == 0) {
            return 0;
        }
        output = PRINT(exp);

        if (output) { 
            puts(output);
            MAL_GC_FREE(output);        // Free output string
        }

        //malval_free(exp);    // Free evaluated expression
    }
}
