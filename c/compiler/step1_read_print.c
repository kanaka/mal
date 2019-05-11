#include <editline/readline.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "printer.h"
#include "reader.h"

MalType* READ(char *str) {
  return read_str(str);
}

MalType* EVAL(MalType *ast) {
  return ast;
}

char* PRINT(MalType *ast) {
  return pr_str(ast, 2);
}

char* rep(char *str) {
  return PRINT(EVAL(READ(str)));
}

int main() {
  char *buffer;

  read_history("history.txt");
  while ((buffer = readline("user> ")) != NULL) {
    printf("%s\n", rep(buffer));
    if (strlen(buffer) > 0) {
      add_history(buffer);
    }
    free(buffer);
  }
  write_history("history.txt");

  return 0;
}
