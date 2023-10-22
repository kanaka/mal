#ifndef _MAL_GC_H
#define _MAL_GC_H
#include <stdlib.h>

void *mal_malloc(size_t size);
void *mal_calloc(size_t count, size_t size);
void *mal_realloc(void *ptr, size_t size);
#endif