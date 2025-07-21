#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include <editline/history.h>

#include "types.h"
#include "reader.h"
#include "printer.h"

#define PROMPT_STRING "user> "

MalType READ(const char* str) {

  return read_str(str);
}

MalType EVAL(MalType val) {

  return val;
}

void PRINT(MalType val) {

  printf("%M\n", val);
}

void rep(const char* str) {

  PRINT(EVAL(READ(str)));
}

int main(int, char**) {

  printer_init();

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
