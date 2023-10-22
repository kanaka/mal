#ifndef _MAL_PRINTER_H
#define _MAL_PRINTER_H
#include <stdbool.h>
#include <stdio.h>
#include "types.h"
#include "libs/hashmap/hashmap.h"

void print(FILE *stream, MalValue *value, bool readably);
char *pr_str(MalValue *value, bool readably);
MalValue *print_values_readably(MalCell *values, MalEnvironment *environment);
MalValue *print_values(MalCell *values, MalEnvironment *environment);
MalValue *println(MalCell *values, MalEnvironment *environment);
#endif