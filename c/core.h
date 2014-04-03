#ifndef __MAL_CORE__
#define __MAL_CORE__

#include <glib.h>

// These are just used by step2 and step3 before then core_ns environment is
// imported

MalVal *int_plus(MalVal *a, MalVal *b);
MalVal *int_minus(MalVal *a, MalVal *b);
MalVal *int_multiply(MalVal *a, MalVal *b);
MalVal *int_divide(MalVal *a, MalVal *b);

// Useful for step implementation
MalVal *first(MalVal *seq);
MalVal *rest(MalVal *seq);
MalVal *last(MalVal *seq);
MalVal *hash_map(MalVal *args);

// namespace of type functions
typedef struct {
    char *name;
    void *(*func)(void*);
    int arg_cnt;
} core_ns_entry;

extern core_ns_entry core_ns[50];

#endif
