#ifndef MAL_VECTOR_H
#define MAL_VECTOR_H

#include <stdbool.h>

#include "types.h"
// typedef const struct vector* vector_t;

struct vector {
  size_t count;
  MalType nth[];
};

struct vector* vector_new(size_t capacity);
//  The capacity first additions cause no reallocation.

void vector_append(size_t* capacity, struct vector** v, MalType new_item);

// Convenient way to iterate either on a list or a vector.
// The same (unmodified) container must be be provided to each
// function during iteration.
// It must be a list or a vector.
typedef union seq_cursor {
  list l;
  size_t i;
} seq_cursor;
seq_cursor seq_iter(MalType);
bool seq_cont(MalType, seq_cursor);
seq_cursor seq_next(MalType, seq_cursor);
MalType seq_item(MalType, seq_cursor);

#endif
