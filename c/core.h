#ifndef __MAL_CORE__
#define __MAL_CORE__

#include <glib.h>

// namespace of type functions
typedef struct {
    char *name;
    void *(*func)(void*);
    int arg_cnt;
} core_ns_entry;

extern core_ns_entry core_ns[58];

#endif
