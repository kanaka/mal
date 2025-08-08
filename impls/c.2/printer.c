#include <assert.h>
#include <printf.h>
#include <stdio.h>

#include <gc.h>

#include "linked_list.h"
#include "printer.h"
#include "hashmap.h"
#include "vector.h"

#define PRINT_NIL "nil"
#define PRINT_TRUE "true"
#define PRINT_FALSE "false"

int escape_string(FILE *stream, const char* str);
int pr_str_vector(FILE* stream, const struct printf_info *i, vector_t v);

// Execute count once.
#define ADD(count) {  \
    int more = count; \
    if(more < 0)      \
      return more;    \
    written += more;  \
  }

int print_M(FILE *stream, const struct printf_info *i, const void *const *a) {
  MalType val = *((const MalType*)(*a));

  int written = 0;

  switch(type(val)) {

  case MALTYPE_SYMBOL:

    ADD(fprintf(stream, "%s", is_symbol(val)));
    break;

  case MALTYPE_KEYWORD:

    ADD(fprintf(stream, ":%s", is_keyword(val)));
    break;

  case MALTYPE_INTEGER:
    {
      long mal_integer;
      is_integer(val, &mal_integer);
      ADD(fprintf(stream, "%ld", mal_integer));
      break;
    }
  case MALTYPE_FLOAT:
    {
      double mal_float;
      is_float(val, &mal_float);
      ADD(fprintf(stream, "%lf", mal_float));
      break;
    }
  case MALTYPE_STRING:

    if (!i->alt) {
      ADD(escape_string(stream, is_string(val)));
    }
    else {
      ADD(fprintf(stream, "%s", is_string(val)));
    }
    break;

  case MALTYPE_TRUE:

    ADD(fprintf(stream, PRINT_TRUE));
    break;

  case MALTYPE_FALSE:

    ADD(fprintf(stream, PRINT_FALSE));
    break;

  case MALTYPE_NIL:

    ADD(fprintf(stream, PRINT_NIL));
    break;

  case MALTYPE_LIST:
    {
      list mal_list;
      is_list(val, &mal_list);
      ADD(fprintf(stream, i->alt ? "(%#N)" : "(%N)", mal_list));
      break;
    }
  case MALTYPE_VECTOR:

    ADD(pr_str_vector(stream, i, is_vector(val)));
    break;

  case MALTYPE_HASHMAP:

    ADD(fprintf(stream, i->alt ? "{%#H}" : "{%H}", is_hashmap(val)));
    break;

  case MALTYPE_FUNCTION:

    ADD(fprintf(stream, "#<core>"));
    break;

  case MALTYPE_CLOSURE:

    ADD(fprintf(stream, i->alt ? "#<fn* %#N>" : "#<fn* %N>",
                is_closure(val)->fnstar_args));
    break;

  case MALTYPE_MACRO:

    ADD(fprintf(stream, i->alt ? "#<macro %#N>" : "#<macro %N>",
                is_macro(val)->fnstar_args));
    break;

  case MALTYPE_ATOM:

    ADD(fprintf(stream, i->alt ? "(atom %#M)" : "(atom %M)", *is_atom(val)));

  }

  if (written < i->width) {
    ADD(fprintf(stream, "%*s", i->width - written, ""));
  }
  return written;
}


int print_L(FILE* stream, const struct printf_info *i, const void *const *a) {

  int written = 0;
  for (list lst = *((const list*)(*a)); lst;  lst = lst->next) {
    ADD(fprintf(stream, i->alt ? "%s%#M" : "%s%M",
                !i->space && written ? " " : "", lst->data));
  }
  return written;
}

int pr_str_vector(FILE* stream, const struct printf_info *i, vector_t v) {
  int written = 0;
  ADD(fprintf(stream, "["));
  for (int j = 0; j < v->count; j++) {
    ADD(fprintf(stream,
                i->alt ? "%s%#M" : "%s%M",
                j ? " " : "",
                v->nth[j]));
  }
  ADD(fprintf(stream, "]"));
  return written;
}

int pr_str_map(FILE* stream, const struct printf_info *i, const void *const *a) {
  hashmap map = *((const hashmap*)(*a));
  int written = 0;
  for (map_cursor c = map_iter(map); map_cont(map, c); c = map_next(map, c)) {
    ADD(fprintf(stream, i->alt ? "%s%#M %#M" : "%s%M %M", written ? " " : "",
                map_key(map, c), map_val(map, c)));
  }
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

// The order must match the one in types.c.
const char* print_T_table[] = {
  "a symbol",
  "a keyword",
  "an integer",
  "a float",
  "a string",
  "true",
  "false",
  "nil",
  "a list",
  "a vector",
  "a map",
  "a function",
  "a closure",
  "an atom",
  "a macro",
};

int print_T(FILE *stream, const struct printf_info *, const void *const *a) {
  enum mal_type_t mask = *((const enum mal_type_t*)(*a));
  assert(0 < mask);
  int written = 0;
  const char** p = print_T_table;
  while (mask) {
    assert(p < print_T_table + sizeof(print_T_table) / sizeof(*print_T_table));
    if (mask & 1) {
      ADD(fprintf(stream, "%s%s", written ? " or " : "", *p));
    }
    p++;
    mask >>= 1;
  }
  return written;
}

#define generic_arg(specifier, type)                       \
  int arg_##specifier(const struct printf_info*,           \
                      size_t n,int *argtypes, int *size) { \
    if(n < 1) return -1;                                   \
    argtypes[0] = PA_POINTER;                              \
    *size = sizeof(type);                                  \
    return 1;                                              \
  }
generic_arg(M, MalType);
generic_arg(N, list);
generic_arg(T, enum mal_type_t);
generic_arg(H, hashmap);

void printer_init() {

  int ret1 = register_printf_specifier('N', print_L, arg_N);
  int ret2 = register_printf_specifier('M', print_M, arg_M);
  int ret3 = register_printf_specifier('T', print_T, arg_T);
  int ret4 = register_printf_specifier('H', pr_str_map, arg_H);
  assert(!ret1);
  assert(!ret2);
  assert(!ret3);
  assert(!ret4);
#ifdef NDEBUG
  (void)ret1;
  (void)ret2;
  (void)ret3;
  (void)ret4;
#endif
}

const char* mal_printf(const char* fmt, ...) {

  va_list argptr;

  va_start(argptr, fmt);
  int n = vsnprintf(NULL, 0, fmt, argptr);
  assert(0 <= n);
  va_end(argptr);

  char* buffer = GC_MALLOC(n + 1);

  va_start(argptr, fmt);
  int again = vsnprintf(buffer, n + 1, fmt, argptr);
  assert(n == again);
#ifdef NDEBUG
  (void)again;
#endif
  va_end(argptr);

  return buffer;
}
