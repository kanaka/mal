#ifndef _MAL_LINKED_LIST_H
#define _MAL_LINKED_LIST_H

#include "types.h"

/* linked list is constructed of pairs */
/* a list is just a pointer to the pair at the head of the list */
struct pair_s {

  MalType data;
  list next;

};

/* interface */
list list_push(list lst, MalType data_ptr);
size_t list_count(list lst);

#endif
