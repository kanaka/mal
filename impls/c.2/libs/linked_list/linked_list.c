#include <gc.h>

#include "linked_list.h"

list list_push(list lst, MalType data_ptr) {

  struct pair_s* new_head = GC_malloc(sizeof(*new_head));
  new_head->data = data_ptr;
  new_head->next = lst;

  return new_head;
}

size_t list_count(list lst) {

  size_t counter = 0;

  while(lst) {

    counter++;
    lst = lst->next;
  }

  return counter;
}
