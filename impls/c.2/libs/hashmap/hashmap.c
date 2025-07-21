#include <string.h>

#include "hashmap.h"
#include "../../printer.h"

hashmap search(hashmap map, MalType keystring);

hashmap hashmap_put(hashmap map, MalType keystring, MalType data_ptr) {

  const hashmap position = search(map, keystring);
  if(position) {
    //  Remove the key from map.
    //  Recreate the part before the removed key, reuse the rest.
    hashmap p = map;
    map = position->next->next;
    while(p != position) {
      MalType k = p->data;
      p = p->next;
      map = list_push(map, p->data);
      map = list_push(map, k);
      p = p->next;
    }
  }

  map = list_push(map, data_ptr);
  map = list_push(map, keystring);

  return map;
}

hashmap search(hashmap lst, MalType keystring) {

  while(lst) {

    MalType val = lst->data;
    if((keystring->type == val->type)
       && (strcmp(keystring->value.mal_string, val->value.mal_string) == 0)) {
      return lst;
    }
    else {
      lst = (lst->next)->next; /* skip the next item in the list to get to the next key */
    }
  }
  return NULL; /* not found */
}

MalType hashmap_get(hashmap map, MalType keystring) {

  hashmap result = search(map, keystring);
  return result ? result->next->data : NULL;
}

MalType map_assoc(hashmap map, list args) {

  while(args) {
    MalType val = args->data;
    if (!is_keyword(val) && !is_string(val))
      return bad_type(val, "a keyword or string");
    args = args->next;
    if(!args)
      return make_error_fmt("odd number in map bindings, expected key/value pairs");
    map = hashmap_put(map, val, args->data);
    args = args->next;
  }
  return make_hashmap(map);
}

MalType mal_hash_map(list args) {
  return map_assoc(NULL, args);
}
