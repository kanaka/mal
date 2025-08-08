#include <stdio.h>
#include <stdlib.h>

#include "readline.h"

#define PROMPT_STRING "user> "

const char* READ(const char* str) {

  return str;
}

const char* EVAL(const char* ast) {

  return ast;
}

void PRINT(const char* str) {

  printf("%s\n", str);
}

void rep(const char* str) {

  PRINT(EVAL(READ(str)));
}

int main() {

    const char* input;
    while((input = readline_gc(PROMPT_STRING))) {

      /* print prompt and get input*/
      /* Check for EOF (Ctrl-D) */

      /* call Read-Eval-Print */
      rep(input);
    }
    printf("\n");

  return EXIT_SUCCESS;
}
