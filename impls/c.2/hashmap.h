#ifndef MAL_HASHMAP_H
#define MAL_HASHMAP_H

#include <stdbool.h>

#include "types.h"

// Keys must be keywords, strings or symbols.

struct map* map_empty();
// not NULL

struct map* map_copy(hashmap);

struct map* hashmap_put(struct map* map, MalType key, void* value);
// Value must not be NULL.
// May reallocate.

void* hashmap_get(hashmap map, MalType key);
// Returns NULL if the map does not contain the key.

void map_dissoc_mutate(struct map* map, MalType key);

size_t map_count(hashmap);

typedef size_t map_cursor;
// The same (unmodified) container must be be provided to each
// function during iteration.

map_cursor map_iter(hashmap);
bool map_cont(hashmap, map_cursor);
map_cursor map_next(hashmap, map_cursor);
MalType map_key(hashmap, map_cursor);
void* map_val(hashmap, map_cursor);

#endif
