#include <stdlib.h>
#include <string.h>

#include <gc.h>
#if USE_READLINE
#  include <readline/readline.h>
#  include <readline/history.h>
#else
#  include <readline.h>
#  include <history.h>
#endif

const char* readline_gc(const char* prompt) {

  char* str = readline(prompt);
  if (!str) {
    return NULL;
  }
  add_history(str);
  /* Copy the input into an area managed by libgc. */
  size_t n = strlen(str) + 1;
  char* result = GC_MALLOC(n);
  memcpy(result, str, n);
  free(str);
  return result;
}
