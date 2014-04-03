#ifndef __MAL_PRINTER__
#define __MAL_PRINTER__

#include "types.h"

char *_pr_str_args(MalVal *args, char *sep, int print_readably);
char *_pr_str(MalVal *obj, int print_readably);

#endif
