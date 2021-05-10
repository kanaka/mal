#ifndef _MAL_LINKED_LIST_H
#define _MAL_LINKED_LIST_H

/* simplify references to void pointers */
typedef void* gptr;

/* linked list is constructed of pairs */
typedef struct pair_s {

  gptr data;
  struct pair_s *next;

} pair;

/* a list is just a pointer to the pair at the head of the list */
typedef pair* list;

/* interface */
list list_make(gptr data_ptr);
list list_push(list lst, gptr data_ptr);
gptr list_peek(list lst);
gptr list_nth(list lst, int n);
gptr list_first(list lst);
list list_rest(list lst);
list list_pop(list lst);
list list_reverse(list lst);
long list_count(list lst);
list list_concatenate(list lst1, list lst2);
list list_copy(list lst);
long list_findf(list lst, char* keystring, char*(*fn)(gptr));

#endif
