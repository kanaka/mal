#include <editline/readline.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* READ(char *str) {
  return str;
}

char* EVAL(char *str) {
  return str;
}

char* PRINT(char *str) {
  return str;
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
