#ifndef __MAL_PRINTER__
#define __MAL_PRINTER__

#include "hashmap.h"
#include "types.h"

char *BLANK_LINE;

char* pr_atom(MalType *val, int print_readably);
char* pr_str(MalType *val, int print_readably);
char* pr_list(MalType *val, int print_readably);
char* pr_vector(MalType *val, int print_readably);
char* pr_hashmap(MalType *val, int print_readably);
char* pr_string(MalType *val, int print_readably);

#endif
