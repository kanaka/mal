#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "types.h"
#include "reader.h"
#include "printer.h"

#define PROMPT_STRING "user> "

MalType* READ(char* str) {

  return read_str(str);
}

MalType* EVAL(MalType* val) {

  return val;
}

void PRINT(MalType* val) {

  char* output = pr_str(val, READABLY);
  printf("%s\n", output);
}

void rep(char* str) {

  PRINT(EVAL(READ(str)));
}

int main(int argc, char** argv) {

  /* Greeting message */
  puts("Make-a-lisp version 0.0.2\n");
  puts("Press Ctrl+d to exit\n");

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
    rep(input);

    /* have to release the memory used by readline */
    free(input);
  }

  return 0;
}
