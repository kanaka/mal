#ifndef MAL_ERROR_H
#define MAL_ERROR_H

#include "types.h"

extern MalType mal_error;

#define make_error(...) {                             \
    mal_error = make_string(mal_printf(__VA_ARGS__)); \
    return 0;                                         \
  }

#define bad_type(context, mask, form)                     \
  make_error("'%s': bad argument type: expected %T, got %M", context, mask, form)

#define check_type(context, mask, form) \
  if (type(form) & ~(mask))             \
    bad_type(context, mask, form)

#define bad_arg_count(context, expected, args)                 \
  make_error("'" context "': bad argument count: expected %s, got [%N]", expected, args)

#define explode0(context, args)                 \
  if (args)                                     \
    bad_arg_count(context, "no argument", args)

#define explode1(context, args, var1)             \
  if (!args || args->next)                        \
    bad_arg_count(context, "one argument", args); \
  MalType var1 = args->data

#define explode2(context, args, var1, var2)        \
  list _a;                                         \
  if (!args || !(_a = args->next) || _a->next)     \
    bad_arg_count(context, "two arguments", args); \
  MalType var1 = args->data;                       \
  MalType var2 = _a->data

#define explode3(context, args, var1, var2, var3)                  \
  list _a, _b;                                                     \
  if (!args || !(_a = args->next) || !(_b = _a->next) || _b->next) \
    bad_arg_count(context, "three arguments", args);               \
  MalType var1 = args->data;                                       \
  MalType var2 = _a->data;                                         \
  MalType var3 = _b->data

#endif
