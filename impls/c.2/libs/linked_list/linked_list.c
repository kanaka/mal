#include <stdio.h>
#include <string.h>
#include <gc.h>
#include "linked_list.h"

list list_make(gptr data_ptr) {

  return list_push(NULL, data_ptr);
}

list list_push(list lst, gptr data_ptr) {

  pair* new_head = GC_malloc(sizeof(pair));
  new_head->data = data_ptr;
  new_head->next = lst;

  return new_head;
}

gptr list_peek(list lst) {

  return (lst ? lst->data : NULL);
}

list list_pop(list lst) {
  return (lst ? lst->next : NULL);
}

long list_count(list lst) {

  /* handle empty case */
  if (!lst) {
    return 0;
  }

  long counter = 1;

  while(lst->next) {

    counter++;
    lst = lst->next;
  }

  return counter;
}

list list_reverse(list lst) {

  /* list is not empty and has more than one element */
  if (lst && lst->next) {

    pair *prev = NULL, *next = NULL, *current = lst;

    while (current) {

      /* stash current value of next pointer --> */
      next = current->next;

      /* reverse the next pointer on current pair <-- */
      current->next = prev;

      /* move on to next pair and repeat --> */
      prev = current;
      current = next;

    }

    lst = prev; /* head of list is in prev when current = NULL */
  }

  return lst;
}

list list_concatenate(list lst1, list lst2) {

  list new_lst = NULL;
  list iterator = NULL;

  while (lst2) {

    gptr val = lst2->data;
    new_lst = list_push(new_lst, val);
    lst2 = lst2->next;
  }
  new_lst = list_reverse(new_lst);

  lst1 = list_reverse(lst1);

  iterator = lst1;
  while (iterator) {

    gptr val = iterator->data;
    new_lst = list_push(new_lst, val);
    iterator = iterator->next;
  }

  lst1 = list_reverse(lst1);
  return new_lst;
}

gptr list_nth(list lst, int n) {

  int idx = 0;
  while (lst) {

    if (n == idx) {
      return lst->data;
    }
    idx++;
    lst = lst->next;
  }
  return NULL;
}

gptr list_first(list lst) {

  if (lst) {
    return lst->data;
  }
  else {
    return NULL;
  }
}

list list_rest(list lst) {

  if (lst) {
    return lst->next;
  }
  else {
    return NULL;
  }
}

list list_copy(list lst) {

  if(!lst) {
    return NULL;
  }

  list new_lst = NULL;
  while(lst) {

    new_lst = list_push(new_lst, lst->data);
    lst = lst->next;
  }
  return new_lst;
}

long list_findf(list lst, char* keystring, char*(*fn)(gptr)) {

  /* handle empty case */
  if (!lst) {
    return -1;
  }

  list current = lst;
  while(current) {

    /* apply fn to the data to get a string */
    char* item = fn(current->data);

    if (strcmp(keystring, item) == 0) {
      return (current - lst); /* return the index of the first match */
    }
    else {
      current = current->next; /* skip the next item in the list to*/
    }
  }
  return -1; /* not found */
}
