#include "gc.h"

void *mal_malloc(size_t size)
{
    return malloc(size);
}

void *mal_calloc(size_t count, size_t size)
{
    return calloc(count, size);
}

void *mal_realloc(void *ptr, size_t size) {
    return realloc(ptr, size);
}