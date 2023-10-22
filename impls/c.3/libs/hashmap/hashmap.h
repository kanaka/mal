#ifndef _MAL_HASHMAP_H
#define _MAL_HASHMAP_H

#include <stdbool.h>
#include <stddef.h>

typedef struct MapEntry
{
    const char *key;
    void *value;
} MapEntry;

typedef struct HashMap
{
    MapEntry *entries;
    size_t capacity;
    size_t length;
} HashMap;

// Hash table iterator: create with ht_iterator, iterate with ht_next.
typedef struct
{
    const char *key; // current key
    void *value;     // current value

    // Don't use these fields directly.
    HashMap *_map; // reference to hash table being iterated
    size_t _index; // current index into ht._entries
} HashMapIterator;

HashMap *make_hashmap();
void free_hashmap(HashMap *);
const char *hashmap_put(HashMap *hashMap, const char *key, void *value);
void *hashmap_get(HashMap *hashMap, const char *key);

// Return new hash table iterator (for use with ht_next).
HashMapIterator hashmap_iterator(HashMap *table);

// Move iterator to next item in hash table, update iterator's key
// and value to current item, and return true. If there are no more
// items, return false. Don't call ht_set during iteration.
bool hashmap_next(HashMapIterator *it);
#endif