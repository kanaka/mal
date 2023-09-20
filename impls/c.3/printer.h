#ifndef _MAL_PRINTER_H
#define _MAL_PRINTER_H
#include <stdbool.h>
#include <stdio.h>
#include "types.h"

void print(FILE *stream, MalValue *value, bool readably);
#endif