#ifndef _MAL_HASHMAP_H
#define _MAL_HASHMAP_H

#include "../linked_list/linked_list.h"

/* a hashmap is just a list with alternating key/value pairs */
typedef list hashmap;

hashmap hashmap_make(char* keystring, gptr data_ptr);
hashmap hashmap_put(hashmap map, char* keystring, gptr data_ptr);
gptr hashmap_get(hashmap map, char* keystring);
gptr hashmap_getf(hashmap map, char* keystring, char*(*fn)(gptr));
hashmap hashmap_updatef(hashmap map, char* keystring, gptr value, char*(*fn)(gptr));

#endif
