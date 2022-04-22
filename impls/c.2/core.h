#ifndef _MAL_CORE_H
#define _MAL_CORE_H

#include "libs/hashmap/hashmap.h"
#include "types.h"

typedef struct ns_s ns;

struct ns_s {

  hashmap mappings;

};

ns* ns_make_core();
MalType* as_str(list args, int readably, char* separator);
MalType* print(list args, int readably, char* separator);
char* get_fn(gptr data);
MalType* equal_lists(MalType* lst1, MalType* lst2);
MalType* equal_hashmaps(MalType* map1, MalType* map2);

#endif
