#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include <editline/history.h>

#define PROMPT_STRING "user> "

const char* READ(const char* str) {

  return str;
}

const char* EVAL(const char* str) {

  return str;
}

void PRINT(const char* str) {

  printf("%s\n", str);
}

void rep(const char* str) {

  PRINT(EVAL(READ(str)));
}

int main(int, char**) {

    char* input;
    while((input = readline(PROMPT_STRING))) {

      /* print prompt and get input*/
      /* readline allocates memory for input */
      /* Check for EOF (Ctrl-D) */
      /* add input to history */
      add_history(input);

      /* call Read-Eval-Print */
      rep(input);

      /* have to release the memory used by readline */
      free(input);
    }
    printf("\n");
  return EXIT_SUCCESS;
}
