#ifndef _MAL_HASHMAP_H
#define _MAL_HASHMAP_H

#include "../linked_list/linked_list.h"
#include "../../types.h"

/* a hashmap is just a list with alternating key/value pairs */
//  Keys are strings or keywords.
//  Each key appears only once.
typedef list hashmap;

hashmap hashmap_put(hashmap, MalType, MalType);
// Check the key type
// Remove duplicates.

MalType hashmap_get(hashmap, MalType);

MalType map_assoc(hashmap, list);
// Deliberately reverse the bindings so that the last key takes
// precedence.  Remove duplicates.

MalType mal_hash_map(list args);

#endif
