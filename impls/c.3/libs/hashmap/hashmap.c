#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "hashmap.h"

/**
 * Base on https://benhoyt.com/writings/hash-table-in-c/
 */

#define INITIAL_CAPACITY 16;

HashMap *make_hashmap()
{
    HashMap *map = (HashMap *)malloc(sizeof(HashMap));

    if (map == NULL)
    {
        return NULL;
    }

    map->length = 0;
    map->capacity = INITIAL_CAPACITY;

    map->entries = calloc(map->capacity, sizeof(MapEntry));

    if (map->entries == NULL)
    {
        free(map);
    }

    return map;
}

void free_hashmap(HashMap *map)
{
    free(map->entries);
    free(map);
}

#define FNV_OFFSET 14695981039346656037UL
#define FNV_PRIME 1099511628211UL

// Return 64-bit FNV-1a hash for key (NUL-terminated). See description:
// https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
static uint64_t hash_key(const char *key)
{
    uint64_t hash = FNV_OFFSET;

    for (const char *p = key; *p; p++)
    {
        hash ^= (uint64_t)(unsigned char)(*p);
        hash *= FNV_PRIME;
    }

    return hash;
}

// Internal function to set an entry (without expanding table).
static const char *hashmap_set_entry(MapEntry *entries, size_t capacity,
                                     const char *key, void *value, size_t *plength)
{
    // AND hash with capacity-1 to ensure it's within entries array.
    uint64_t hash = hash_key(key);
    size_t index = (size_t)(hash & (uint64_t)(capacity - 1));

    // Loop till we find an empty entry.
    while (entries[index].key != NULL)
    {
        if (strcmp(key, entries[index].key) == 0)
        {
            // Found key (it already exists), update value.
            entries[index].value = value;
            return entries[index].key;
        }
        // Key wasn't in this slot, move to next (linear probing).
        index++;
        if (index >= capacity)
        {
            // At end of entries array, wrap around.
            index = 0;
        }
    }

    // Didn't find key, allocate+copy if needed, then insert it.
    if (plength != NULL)
    {
        (*plength)++;
    }

    entries[index].key = (char *)key;
    entries[index].value = value;

    return key;
}

// Expand hash table to twice its current size. Return true on success,
// false if out of memory.
static bool expand_hashmap(HashMap *map)
{
    // Allocate new entries array.
    size_t new_capacity = map->capacity * 2;

    if (new_capacity < map->capacity)
    {
        return false; // overflow (capacity would be too big)
    }

    MapEntry *new_entries = calloc(new_capacity, sizeof(MapEntry));

    if (new_entries == NULL)
    {
        return false;
    }

    // Iterate entries, move all non-empty ones to new table's entries.
    for (size_t i = 0; i < map->capacity; i++)
    {
        MapEntry entry = map->entries[i];

        if (entry.key != NULL)
        {
            hashmap_set_entry(new_entries, new_capacity, entry.key,
                              entry.value, NULL);
        }
    }

    // Free old entries array and update this table's details.
    free(map->entries);
    map->entries = new_entries;
    map->capacity = new_capacity;

    return true;
}

const char *hashmap_put(HashMap *map, const char *key, void *value)
{
    assert(value != NULL);

    if (value == NULL)
    {
        return NULL;
    }

    // If length will exceed half of current capacity, expand it.
    if (map->length >= map->capacity / 2)
    {
        if (!expand_hashmap(map))
        {
            return NULL;
        }
    }

    // Set entry and update length.
    return hashmap_set_entry(map->entries, map->capacity, key, value,
                             &map->length);
}

void *hashmap_get(HashMap *map, const char *key)
{
    // AND hash with capacity-1 to ensure it's within entries array.
    uint64_t hash = hash_key(key);
    size_t index = (size_t)(hash & (uint64_t)(map->capacity - 1));

    // Loop till we find an empty entry.
    while (map->entries[index].key != NULL)
    {
        // Found key, return value.
        if (strcmp(key, map->entries[index].key) == 0)
        {
            return map->entries[index].value;
        }

        // Key wasn't in this slot, move to next (linear probing).
        index++;

        if (index >= map->capacity)
        {
            // At end of entries array, wrap around.
            index = 0;
        }
    }

    return NULL;
}

HashMapIterator hashmap_iterator(HashMap *map)
{
    HashMapIterator it;
    it._map = map;
    it._index = 0;

    return it;
}

bool hashmap_next(HashMapIterator *it)
{
    // Loop till we've hit end of entries array.
    HashMap *map = it->_map;

    while (it->_index < map->capacity)
    {
        size_t i = it->_index;
        it->_index++;

        if (map->entries[i].key != NULL)
        {
            // Found next non-empty item, update iterator key and value.
            MapEntry entry = map->entries[i];
            it->key = entry.key;
            it->value = entry.value;
            return true;
        }
    }

    return false;
}