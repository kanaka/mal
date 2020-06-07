#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "reader.h"

#define PROMPT_STRING "user> "


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
      printf("\n\n");
      return 0;
    }

    /* add input to history */
    add_history(input);

    /* call Read-Eval-Print */
    Reader* r = tokenize(input);

    reader_print(r);
    printf("\n");


    /* have to release the memory used by readline */
    free(input);
  }

  return 0;
}
