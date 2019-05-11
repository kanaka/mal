#include <assert.h>
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

char* long_long_to_string(long long num) {
  char* str;
  size_t len;
  if (num == 0) {
    return string("0");
  } else {
    len = num_char_len(num);
    str = GC_MALLOC(len + 1);
    snprintf(str, len + 1, "%lli", num);
    return str;
  }
}

// note: there is a formula using log10 to calculate a number length,
// but it uses the math lib which yelled at me for using TinyCC :-/
size_t num_char_len(long long num) {
  if (num < 0) {
    return 1 + num_char_len(llabs(num));
  } else if (num < 10) {
    return 1;
  } else if (num < 100) {
    return 2;
  } else if (num < 1000) {
    return 3;
  } else if (num < 10000) {
    return 4;
  } else if (num < 100000) {
    return 5;
  } else if (num < 1000000) {
    return 6;
  } else if (num < 1000000000) {
    return 9;
  } else if (num < 1000000000000) {
    return 12;
  } else if (num < 1000000000000000) {
    return 15;
  } else if (num < 1000000000000000000) {
    return 18;
  } else { // up to 128 bits
    return 40;
  }
}

char* string(char *str) {
  size_t len = strlen(str);
  char *copy = GC_MALLOC(len + 1);
  snprintf(copy, len + 1, "%s", str);
  return copy;
}

char* substring(char *orig, size_t start, size_t len) {
  size_t orig_len = strlen(orig);
  assert(start < orig_len);
  assert(start + len <= orig_len);
  char *buffer = GC_MALLOC(len + 1);
  snprintf(buffer, len + 1, "%s", orig + start);
  return buffer;
}

MalType* program_arguments_as_vector(int argc, char *argv[]) {
  MalType *arg_vec = mal_vector();
  for (int i=1; i<argc; i++) {
    mal_vector_push(arg_vec, mal_string(argv[i]));
  }
  return arg_vec;
}

MalType* read_file(char *filename) {
  MalType *content = mal_string("");
  FILE *fp = fopen(filename, "r");
  if(!fp) {
    printf("Error opening file %s\n", filename);
    exit(1);
  }
  char buffer[100];
  while (fgets(buffer, 100, fp)) {
    mal_string_append(content, buffer);
  }
  fclose(fp);
  return content;
}
