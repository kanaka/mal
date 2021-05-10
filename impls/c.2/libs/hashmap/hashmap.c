#include <stdio.h>
#include <string.h>
#include <gc.h>

#include "hashmap.h"

hashmap hashmap_make(char* keystring, gptr data_ptr) {

  list map = list_make(data_ptr);
  map = list_push(map, keystring);

  return map;
}

hashmap hashmap_put(hashmap map, char* keystring, gptr data_ptr) {

  map = list_push(map, data_ptr);
  map = list_push(map, keystring);

  return map;
}

gptr hashmap_get(hashmap map, char* keystring) {

  /* handle empty case */
  if (!map) {
    return NULL;
  }

  list lst = map;

  while(lst) {

    if (strcmp(keystring, (char*)lst->data) == 0) {
        return (lst->next)->data; /* return next item in list which is the value */
      }
      else {
        lst = (lst->next)->next; /* skip the next item in the list to get to the next key */
      }
  }
  return NULL; /* not found */
}


gptr hashmap_getf(hashmap map, char* keystring, char*(*fn)(gptr)) {

  /* handle empty case */
  if (!map) {
    return NULL;
  }

  list lst = map;

  while(lst) {

    /* apply fn to the data to get a string */
    char* item = fn(lst->data);

    if (strcmp(keystring, item) == 0) {
        return (lst->next)->data; /* return next item in list which is the value */
      }
      else {
        lst = (lst->next)->next; /* skip the next item in the list to get to the next key */
      }
  }
  return NULL; /* not found */
}

hashmap hashmap_updatef(hashmap map, char* keystring, gptr value, char*(*fn)(gptr)) {

  /* handle empty case */
  if (!map) {
    return NULL;
  }

  list lst = map;

  while(lst) {

    /* apply fn to the data to get a string */
    char* item = fn(lst->data);

    if (strcmp(keystring, item) == 0) {
      (lst->next)->data = value; /* update the next item in list which is the value */
      return map; /* update made */
    }
    else {
      lst = (lst->next)->next; /* skip the next item in the list to get to the next key */
    }
  }

  return NULL; /* no update */
}
