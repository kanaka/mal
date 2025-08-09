#include <assert.h>
#include <string.h>

#include <gc.h>

#include "hashmap.h"

#ifdef DEBUG_HASHMAP
#  include <stdio.h>
#  include "printer.h"
#endif
#ifdef DEBUG_HASH_COLLISIONS
#  include <stdio.h>
#  include "printer.h"
#endif

// Removals or redefinitions are rare.
// Most structures are quite small, except two ones.
//   the REPL environment and
//   the map representing the hosted REPL environment.
// Most maps are built once, then constant.
// MAL spends a lot of its time searching DEBUG-EVAL in environments.

// Either a map has less than 3 keys, or be generous.
// After changing this, try "make debug_hash_collisions=1",
// because a collison for DEBUG-EVAL in REPL is costly.
#define MIN_BUCKETS 7
#define GROW_FACTOR 25

struct map {
  // Invariants:
  //   table contains size buckets.
  //   0 <= 2*used < size
  // A bucket may have three states:
  //   unused        (both key and value == NULL)
  //   used normally (both key and value != NULL)
  //   used deleted  (key != NULL but value == NULL)
  // In case of collision, search after the intended one.
  size_t used;
  size_t size;
  struct bucket {
    MalType key;
    void*   value;
  } buckets[];
};

struct map* map_empty() {
  struct map* m = GC_MALLOC(sizeof(*m) + MIN_BUCKETS*sizeof(struct bucket));
  // GC_MALLOC sets all the allocated space to zero.
  m->size = MIN_BUCKETS;
  return m;
}

struct map* map_copy(hashmap map) {
  size_t bytes = sizeof(*map) + map->size * sizeof(struct bucket);
  struct map* m = GC_MALLOC(bytes);
  memcpy(m, map, bytes);
  return m;
}

size_t search(hashmap map, MalType key) {
  // The key of the returned index is either NULL or equal to key.

  size_t index = get_hash(key) % map->size;
  while (true) {
    MalType current = map->buckets[index].key;
    if (!current || equal_forms(key, current)) break;
#ifdef DEBUG_HASH_COLLISIONS
    printf("collision   %M(h:%u i:%u)    %M(h:%u i:%u)\n",
           key,     get_hash(key),     get_hash(key)     % map->size,
           current, get_hash(current), get_hash(current) % map->size);
#endif
    index++;
    if (index == map->size) index = 0;
  }
#ifdef DEBUG_HASH_COLLISIONS
  if (index != get_hash(key) % map->size) {
    printf("collision (%.1f%% of %u) key %M stored in bucket %d instead of %d\n",
           (float)(100*map->used) / (float)(map->size), map->size,
           key, index, get_hash(key) % map->size);
  }
#endif
#ifdef DEBUG_HASHMAP
  printf("HASHMAP: search:%M  hash:%u  index:%u\n", key, get_hash(key), index);
  for (size_t i = 0; i < map->size; i++) {
    if (map->buckets[i].key) {
      if (map->buckets[i].value) {
        printf("  bucket:%u/%u  key:%M  val:%M\n",
               i, map->size, map->buckets[i].key, map->buckets[i].value);
      }
      else {
        printf("  bucket:%u/%u  key:%M  (removed)\n",
               i, map->size, map->buckets[i].key);
      }
    }
    else {
      assert(!map->buckets[i].value);
    }
    printf("");
  }
#endif
  return index;
}

void put(struct map* map, MalType key, void* value) {
  size_t i = search(map, key);
  if (!map->buckets[i].key) {
    map->used++;
    map->buckets[i].key = key;
  }
  // else replace the existing/deleted value
  map->buckets[i].value = value;
}

struct map* hashmap_put(struct map* map, MalType key, void* value) {
  assert(value);
  if (map->size <= 2 * (map->used + 1)) {
    // Reallocate.
    size_t size = map->size * GROW_FACTOR;
    struct map* m = GC_MALLOC(sizeof(*m) + size*sizeof(struct bucket));
    // GC_MALLOC sets all the allocated space to zero.
    m->size = size;
    for (size_t i = 0; i < map->size; i++) {
      if (map->buckets[i].key && map->buckets[i].value) {
        put(m, map->buckets[i].key, map->buckets[i].value);
      }
    }
    map = m;
  }
  put(map, key, value);
  return map;
}

inline void* hashmap_get(hashmap map, MalType key) {
  return map->buckets[search(map, key)].value; // may be null
}

void map_dissoc_mutate(struct map* map, MalType key) {
  size_t i = search(map, key);
  if (map->buckets[i].key) {
    map->buckets[i].value = NULL;
  }
}

inline size_t map_count(hashmap map) {
  return map->used;
}

map_cursor next_valid(hashmap map, size_t i) {
  while ((i < map->size) && !(map->buckets[i].key && map->buckets[i].value)) {
    i++;
  }
  return i;
}

inline map_cursor map_iter(hashmap map) {
  return next_valid(map, 0);
}

inline bool map_cont(hashmap map, map_cursor position) {
  return position < map->size;
}

inline MalType map_key(hashmap map, map_cursor position) {
  assert(position < map->size);
  assert(map->buckets[position].key);
  assert(map->buckets[position].value);

  return map->buckets[position].key;
}

inline void* map_val(hashmap map, map_cursor position) {
  assert(position < map->size);
  assert(map->buckets[position].key);
  assert(map->buckets[position].value);

  return map->buckets[position].value;
}

inline map_cursor map_next(hashmap map, map_cursor position) {
  assert(position < map->size);
  assert(map->buckets[position].key);
  assert(map->buckets[position].value);

  return next_valid(map, position + 1);
}
