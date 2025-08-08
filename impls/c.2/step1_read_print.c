#include <stdio.h>
#include <stdlib.h>

#include "types.h"
#include "reader.h"
#include "printer.h"
#include "error.h"
#include "readline.h"

#define PROMPT_STRING "user> "

MalType READ(const char* str) {

  return read_str(str);
  // Implicit error propagation
}

MalType EVAL(MalType ast) {

  return ast;
}

void PRINT(MalType val) {

  printf("%M\n", val);
}

void rep(const char* str) {

  MalType a = READ(str);
  if (!mal_error) {
    PRINT(EVAL(a));
    return;
  }
  MalType e = mal_error;
  mal_error = NULL; // before printing
  printf("Uncaught error: %M\n", e);
}

int main() {

  types_init();
  printer_init();

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
