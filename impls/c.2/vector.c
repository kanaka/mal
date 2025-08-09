#include <assert.h>

#include <gc.h>

#include "linked_list.h"
#include "vector.h"

struct vector* vector_new(size_t capacity) {
  struct vector* v = GC_MALLOC(sizeof(*v) + capacity*sizeof(MalType));
  v->count = 0;
  return v;
}

void vector_append(size_t* capacity, struct vector** v, MalType new_item) {
  if ((*v)->count == *capacity) {
    // + 1 in case capacity is 0.
    *capacity = (*capacity + 1) << 1;
    *v = GC_REALLOC(*v, sizeof(**v) + *capacity * sizeof(MalType));
  }
  (*v)->nth[(*v)->count++] = new_item;
}

seq_cursor seq_iter(MalType container) {
  list l;
  if (is_list(container, &l)) {
    return (seq_cursor){.l=l};
  }
  else {
    assert(type(container) == MALTYPE_VECTOR);
    return (seq_cursor){.i=0};
  }
}

bool seq_cont(MalType container, seq_cursor position) {
  assert(type(container) & (MALTYPE_LIST | MALTYPE_VECTOR));
  vector_t v;
  if ((v = is_vector(container))) {
    return position.i < v->count;
  }
  else {
    return position.l != NULL;
  }
}

seq_cursor seq_next(MalType container, seq_cursor position) {
  assert(type(container) & (MALTYPE_LIST | MALTYPE_VECTOR));
  vector_t v;
  if ((v = is_vector(container))) {
    assert(position.i < v->count);
    return (seq_cursor){.i=position.i + 1};
  }
  else {
    return (seq_cursor){.l=position.l->next};
  }
}

MalType seq_item(MalType container, seq_cursor position) {
  assert(type(container) & (MALTYPE_LIST | MALTYPE_VECTOR));
  vector_t v;
  if ((v = is_vector(container))) {
    assert(position.i < v->count);
    return v->nth[position.i];
  }
  else {
    return position.l->data;
  }
}
