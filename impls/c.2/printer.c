#include <assert.h>
#include <printf.h>
#include <stdio.h>

#include <gc.h>

#include "libs/linked_list/linked_list.h"
#include "printer.h"

#define PRINT_NIL "nil"
#define PRINT_TRUE "true"
#define PRINT_FALSE "false"

int pr_str(FILE *stream, MalType val, bool readably);
int escape_string(FILE *stream, const char* str);
int pr_str_list(FILE* stream, list lst, bool readably, bool spaced);
int pr_list(FILE* stream, list lst, const char* start, const char* stop,
            bool readably);
int closure(FILE* stream, MalClosure closure, bool readably,
            const char* kind);

// Execute count once.
#define ADD(count) {  \
    int more = count; \
    if(more < 0)      \
      return more;    \
    written += more;  \
  }

int wrap(FILE* stream, MalType val, bool readably, const char* start,
         const char* stop) {

  int written = 0;
  ADD(fprintf(stream, start));
  ADD(pr_str(stream, val, readably));
  ADD(fprintf(stream, stop));
  return written;
}

int pr_str(FILE *stream, MalType val, bool readably) {

  switch(val->type) {

  case MALTYPE_SYMBOL:

    return fprintf(stream, "%s", val->value.mal_string);

  case MALTYPE_KEYWORD:

    return fprintf(stream, ":%s", val->value.mal_string);

  case MALTYPE_INTEGER:

    return fprintf(stream, "%ld", val->value.mal_integer);

  case MALTYPE_FLOAT:

    return fprintf(stream, "%lf", val->value.mal_float);

  case MALTYPE_STRING:

    if (readably) {
      return escape_string(stream, val->value.mal_string);
    }
    else {
      return fprintf(stream, "%s", val->value.mal_string);
    }

  case MALTYPE_TRUE:

    return fprintf(stream, PRINT_TRUE);

  case MALTYPE_FALSE:

    return fprintf(stream, PRINT_FALSE);

  case MALTYPE_NIL:

    return fprintf(stream, PRINT_NIL);

  case MALTYPE_LIST:

    return pr_list(stream, val->value.mal_list, "(", ")", readably);

  case MALTYPE_VECTOR:

    return pr_list(stream, val->value.mal_list, "[", "]", readably);

  case MALTYPE_HASHMAP:

    return pr_list(stream, val->value.mal_list, "{", "}", readably);

  case MALTYPE_FUNCTION:

    return fprintf(stream, "#<function::native>");

  case MALTYPE_CLOSURE:

    return closure(stream, val->value.mal_closure, readably, "closure");

  case MALTYPE_MACRO:

    return closure(stream, val->value.mal_closure, readably, "macro");

  case MALTYPE_ATOM:

    return wrap(stream, *val->value.mal_atom, readably, "(atom ", ")");

  case MALTYPE_ERROR:

    return wrap(stream, val->value.mal_error, readably, "Uncaught error: ", "");

  }
  // Silent 'control reaches end of non-void function'.
  // It is a false positive thanks to -Wswitch.
  return -1;
}

int closure(FILE* stream, MalClosure closure, bool readably,
            const char* kind) {
  int written = 0;
  ADD(fprintf(stream, "#<function::%s: (fn* (", kind));
  if(closure->param_len) {
    size_t i = 0;
    while(true) {
      ADD(fprintf(stream, "%s", closure->parameters[i]));
      i++;
      if(closure->param_len <= i)
        break;
      ADD(fprintf(stream, " "));
    }
  }
  if(closure->more_symbol) {
    if(closure->param_len)
      ADD(fprintf(stream, " "));
    ADD(fprintf(stream, "& %s", closure->more_symbol));
  }
  ADD(fprintf(stream, ") "));
  ADD(pr_str(stream, closure->definition, readably));
  ADD(fprintf(stream, ")>"));
  return written;
}


int pr_str_list(FILE* stream, list lst, bool readably, bool spaced) {

  int written = 0;
  if(lst != NULL) {
    while(true) {
      ADD(pr_str(stream, lst->data, readably));
      lst = lst->next;
      if(lst == NULL)
        break;
      if(spaced)
        ADD(fprintf(stream, " "));
    }
  }
  return written;
}

int pr_list(FILE* stream, list lst, const char* start, const char* stop,
            bool readably) {

  int written = 0;
  ADD(fprintf(stream, start));
  ADD(pr_str_list(stream, lst, readably, true));

  /* add the end delimiter */
  ADD(fprintf(stream, stop));

  return written;
}


int escape_string(FILE *stream, const char* str) {

  int written = 0;

  ADD(fprintf(stream, "\""));

  const char* curr = str;
  while(*curr != '\0') {

    switch (*curr) {

    case 0x0A:

      ADD(fprintf(stream, "\\n"));
      break;

    case '"':
    case '\\':
      ADD(fprintf(stream, "\\"));
      // fall through

    default:
      ADD(fprintf(stream, "%c", *curr));
    }
    curr++;
  }
  ADD(fprintf(stream, "\""));
  return written;
}

int print_M(FILE *stream, const struct printf_info *i, const void *const *a) {

  return pr_str(stream, *((const MalType*)(*a)), !i->alt);
}

int print_L(FILE *stream, const struct printf_info *i, const void *const *a) {

  return pr_str_list(stream, *((const list *) (a[0])), !i->alt, i->space);
}

int one_arg(const struct printf_info*, size_t n, int *argtypes, int *size) {

  if(n < 1)
    return -1;

  argtypes[0] = PA_POINTER;
  *size = sizeof(MalType);
  return 1;
}

void printer_init() {

  assert(!register_printf_specifier('N', print_L, one_arg));
  assert(!register_printf_specifier('M', print_M, one_arg));
}

const char* mal_printf(const char* fmt, ...) {

  va_list argptr;

  va_start(argptr, fmt);
  int n = vsnprintf(NULL, 0, fmt, argptr);
  assert(0 <= n);
  va_end(argptr);

  char* buffer = GC_MALLOC(n + 1);

  va_start(argptr, fmt);
  assert(n == vsnprintf(buffer, n + 1, fmt, argptr));
  va_end(argptr);

  return buffer;
}
