#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#ifdef USE_READLINE
  #include <readline/readline.h>
  #include <readline/history.h>
#else
  #include <editline/readline.h>
#endif

char *READ(char prompt[]) {
    char *line;
    line = readline(prompt);
    if (!line) return NULL; // EOF
    add_history(line); // Add input to history.
    return line;
}

char *EVAL(char *ast, void *env) {
    return ast;
}

char *PRINT(char *exp) {
    return exp;
}

int main()
{
    char *ast, *exp;
    char prompt[100];

    // Set the initial prompt
    snprintf(prompt, sizeof(prompt), "user> ");
 
    for(;;) {
        ast = READ(prompt);
        if (!ast) return 0;
        exp = EVAL(ast, NULL);
        puts(PRINT(exp));
 
        free(ast); // Free input string
    }
}
