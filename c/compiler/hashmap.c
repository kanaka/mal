/*
 * Copyright (c) 2016-2018 David Leeds <davidesleeds@gmail.com>
 *
 * Hashmap is free software; you can redistribute it and/or modify
 * it under the terms of the MIT license. See LICENSE for details.
 *
 * Updated 2018-02-16 by Tim Morgan to use GC_MALLOC and friends.
 */

#include <gc.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "hashmap.h"

#ifndef HASHMAP_NOASSERT
#include <assert.h>
#define HASHMAP_ASSERT(expr)            assert(expr)
#else
#define HASHMAP_ASSERT(expr)
#endif

/* Table sizes must be powers of 2 */
#define HASHMAP_SIZE_MIN                (1 << 5)    /* 32 */
#define HASHMAP_SIZE_DEFAULT            (1 << 8)    /* 256 */
#define HASHMAP_SIZE_MOD(map, val)      ((val) & ((map)->table_size - 1))

/* Limit for probing is 1/2 of table_size */
#define HASHMAP_PROBE_LEN(map)          ((map)->table_size >> 1)
/* Return the next linear probe index */
#define HASHMAP_PROBE_NEXT(map, index)  HASHMAP_SIZE_MOD(map, (index) + 1)

/* Check if index b is less than or equal to index a */
#define HASHMAP_INDEX_LE(map, a, b)     \
    ((a) == (b) || (((b) - (a)) & ((map)->table_size >> 1)) != 0)


struct hashmap_entry {
    void *key;
    void *data;
#ifdef HASHMAP_METRICS
    size_t num_collisions;
#endif
};


/*
 * Enforce a maximum 0.75 load factor.
 */
static inline size_t hashmap_table_min_size_calc(size_t num_entries)
{
    return num_entries + (num_entries / 3);
}

/*
 * Calculate the optimal table size, given the specified max number
 * of elements.
 */
static size_t hashmap_table_size_calc(size_t num_entries)
{
    size_t table_size;
    size_t min_size;

    table_size = hashmap_table_min_size_calc(num_entries);

    /* Table size is always a power of 2 */
    min_size = HASHMAP_SIZE_MIN;
    while (min_size < table_size) {
        min_size <<= 1;
    }
    return min_size;
}

/*
 * Get a valid hash table index from a key.
 */
static inline size_t hashmap_calc_index(const struct hashmap *map,
    const void *key)
{
    return HASHMAP_SIZE_MOD(map, map->hash(key));
}

/*
 * Return the next populated entry, starting with the specified one.
 * Returns NULL if there are no more valid entries.
 */
static struct hashmap_entry *hashmap_entry_get_populated(
    const struct hashmap *map, struct hashmap_entry *entry)
{
    for (; entry < &map->table[map->table_size]; ++entry) {
        if (entry->key) {
            return entry;
        }
    }
    return NULL;
}

/*
 * Find the hashmap entry with the specified key, or an empty slot.
 * Returns NULL if the entire table has been searched without finding a match.
 */
static struct hashmap_entry *hashmap_entry_find(const struct hashmap *map,
    const void *key, bool find_empty)
{
    size_t i;
    size_t index;
    size_t probe_len = HASHMAP_PROBE_LEN(map);
    struct hashmap_entry *entry;

    index = hashmap_calc_index(map, key);

    /* Linear probing */
    for (i = 0; i < probe_len; ++i) {
        entry = &map->table[index];
        if (!entry->key) {
            if (find_empty) {
#ifdef HASHMAP_METRICS
                entry->num_collisions = i;
#endif
                return entry;
            }
            return NULL;
        }
        if (map->key_compare(key, entry->key) == 0) {
            return entry;
        }
        index = HASHMAP_PROBE_NEXT(map, index);
    }
    return NULL;
}

/*
 * Removes the specified entry and processes the proceeding entries to reduce
 * the load factor and keep the chain continuous.  This is a required
 * step for hash maps using linear probing.
 */
static void hashmap_entry_remove(struct hashmap *map,
    struct hashmap_entry *removed_entry)
{
    size_t i;
#ifdef HASHMAP_METRICS
    size_t removed_i = 0;
#endif
    size_t index;
    size_t entry_index;
    size_t removed_index = (removed_entry - map->table);
    struct hashmap_entry *entry;

    /* Free the key */
    if (map->key_free) {
        map->key_free(removed_entry->key);
    }
    --map->num_entries;

    /* Fill the free slot in the chain */
    index = HASHMAP_PROBE_NEXT(map, removed_index);
    for (i = 1; i < map->table_size; ++i) {
        entry = &map->table[index];
        if (!entry->key) {
            /* Reached end of chain */
            break;
        }
        entry_index = hashmap_calc_index(map, entry->key);
        /* Shift in entries with an index <= to the removed slot */
        if (HASHMAP_INDEX_LE(map, removed_index, entry_index)) {
#ifdef HASHMAP_METRICS
            entry->num_collisions -= (i - removed_i);
            removed_i = i;
#endif
            memcpy(removed_entry, entry, sizeof(*removed_entry));
            removed_index = index;
            removed_entry = entry;
        }
        index = HASHMAP_PROBE_NEXT(map, index);
    }
    /* Clear the last removed entry */
    memset(removed_entry, 0, sizeof(*removed_entry));
}

/*
 * Reallocates the hash table to the new size and rehashes all entries.
 * new_size MUST be a power of 2.
 * Returns 0 on success and -errno on allocation or hash function failure.
 */
static int hashmap_rehash(struct hashmap *map, size_t new_size)
{
    size_t old_size;
    struct hashmap_entry *old_table;
    struct hashmap_entry *new_table;
    struct hashmap_entry *entry;
    struct hashmap_entry *new_entry;

    HASHMAP_ASSERT(new_size >= HASHMAP_SIZE_MIN);
    HASHMAP_ASSERT((new_size & (new_size - 1)) == 0);

    new_table = (struct hashmap_entry *)GC_MALLOC(new_size * sizeof(struct hashmap_entry));
    if (!new_table) {
        return -ENOMEM;
    }
    /* Backup old elements in case of rehash failure */
    old_size = map->table_size;
    old_table = map->table;
    map->table_size = new_size;
    map->table = new_table;
    /* Rehash */
    for (entry = old_table; entry < &old_table[old_size]; ++entry) {
        if (!entry->data) {
            /* Only copy entries with data */
            continue;
        }
        new_entry = hashmap_entry_find(map, entry->key, true);
        if (!new_entry) {
            /*
             * The load factor is too high with the new table
             * size, or a poor hash function was used.
             */
            goto revert;
        }
        /* Shallow copy (intentionally omits num_collisions) */
        new_entry->key = entry->key;
        new_entry->data = entry->data;
    }
    GC_FREE(old_table);
    return 0;
revert:
    map->table_size = old_size;
    map->table = old_table;
    GC_FREE(new_table);
    return -EINVAL;
}

/*
 * Iterate through all entries and free all keys.
 */
static void hashmap_free_keys(struct hashmap *map)
{
    struct hashmap_iter *iter;

    if (!map->key_free) {
        return;
    }
    for (iter = hashmap_iter(map); iter;
        iter = hashmap_iter_next(map, iter)) {
        map->key_free((void *)hashmap_iter_get_key(iter));
    }
}

/*
 * Initialize an empty hashmap.
 *
 * hash_func should return an even distribution of numbers between 0
 * and SIZE_MAX varying on the key provided.  If set to NULL, the default
 * case-sensitive string hash function is used: hashmap_hash_string
 *
 * key_compare_func should return 0 if the keys match, and non-zero otherwise.
 * If set to NULL, the default case-sensitive string comparator function is
 * used: hashmap_compare_string
 *
 * initial_size is optional, and may be set to the max number of entries
 * expected to be put in the hash table.  This is used as a hint to
 * pre-allocate the hash table to the minimum size needed to avoid
 * gratuitous rehashes.  If initial_size is 0, a default size will be used.
 *
 * Returns 0 on success and -errno on failure.
 */
int hashmap_init(struct hashmap *map, size_t (*hash_func)(const void *),
    int (*key_compare_func)(const void *, const void *),
    size_t initial_size)
{
    HASHMAP_ASSERT(map != NULL);

    if (!initial_size) {
        initial_size = HASHMAP_SIZE_DEFAULT;
    } else {
        /* Convert init size to valid table size */
        initial_size = hashmap_table_size_calc(initial_size);
    }
    map->table_size_init = initial_size;
    map->table_size = initial_size;
    map->num_entries = 0;
    map->table = (struct hashmap_entry *)GC_MALLOC(initial_size * sizeof(struct hashmap_entry));
    if (!map->table) {
        return -ENOMEM;
    }
    map->hash = hash_func ?
            hash_func : hashmap_hash_string;
    map->key_compare = key_compare_func ?
            key_compare_func : hashmap_compare_string;
    map->key_alloc = NULL;
    map->key_free = NULL;
    return 0;
}

/*
 * Free the hashmap and all associated memory.
 */
void hashmap_destroy(struct hashmap *map)
{
    if (!map) {
        return;
    }
    hashmap_free_keys(map);
    GC_FREE(map->table);
    memset(map, 0, sizeof(*map));
}

/*
 * Enable internal memory management of hash keys.
 */
void hashmap_set_key_alloc_funcs(struct hashmap *map,
    void *(*key_alloc_func)(const void *),
    void (*key_free_func)(void *))
{
    HASHMAP_ASSERT(map != NULL);

    map->key_alloc = key_alloc_func;
    map->key_free = key_free_func;
}

/*
 * Add an entry to the hashmap.  If an entry with a matching key already
 * exists and has a data pointer associated with it, the existing data
 * pointer is returned, instead of assigning the new value.  Compare
 * the return value with the data passed in to determine if a new entry was
 * created.  Returns NULL if memory allocation failed.
 */
void *hashmap_put(struct hashmap *map, const void *key, void *data)
{
    struct hashmap_entry *entry;

    HASHMAP_ASSERT(map != NULL);
    HASHMAP_ASSERT(key != NULL);

    /* Rehash with 2x capacity if load factor is approaching 0.75 */
    if (map->table_size <= hashmap_table_min_size_calc(map->num_entries)) {
        hashmap_rehash(map, map->table_size << 1);
    }
    entry = hashmap_entry_find(map, key, true);
    if (!entry) {
        /*
         * Cannot find an empty slot.  Either out of memory, or using
         * a poor hash function.  Attempt to rehash once to reduce
         * chain length.
         */
        if (hashmap_rehash(map, map->table_size << 1) < 0) {
            return NULL;
        }
        entry = hashmap_entry_find(map, key, true);
        if (!entry) {
            return NULL;
        }
    }
    if (!entry->key) {
        /* Allocate copy of key to simplify memory management */
        if (map->key_alloc) {
            entry->key = map->key_alloc(key);
            if (!entry->key) {
                return NULL;
            }
        } else {
            entry->key = (void *)key;
        }
        ++map->num_entries;
    } else if (entry->data) {
        /* Do not overwrite existing data */
        return entry->data;
    }
    entry->data = data;
    return data;
}

/*
 * Return the data pointer, or NULL if no entry exists.
 */
void *hashmap_get(const struct hashmap *map, const void *key)
{
    struct hashmap_entry *entry;

    HASHMAP_ASSERT(map != NULL);
    HASHMAP_ASSERT(key != NULL);

    entry = hashmap_entry_find(map, key, false);
    if (!entry) {
        return NULL;
    }
    return entry->data;
}

/*
 * Remove an entry with the specified key from the map.
 * Returns the data pointer, or NULL, if no entry was found.
 */
void *hashmap_remove(struct hashmap *map, const void *key)
{
    struct hashmap_entry *entry;
    void *data;

    HASHMAP_ASSERT(map != NULL);
    HASHMAP_ASSERT(key != NULL);

    entry = hashmap_entry_find(map, key, false);
    if (!entry) {
        return NULL;
    }
    data = entry->data;
    /* Clear the entry and make the chain contiguous */
    hashmap_entry_remove(map, entry);
    return data;
}

/*
 * Remove all entries.
 */
void hashmap_clear(struct hashmap *map)
{
    HASHMAP_ASSERT(map != NULL);

    hashmap_free_keys(map);
    map->num_entries = 0;
    memset(map->table, 0, sizeof(struct hashmap_entry) * map->table_size);
}

/*
 * Remove all entries and reset the hash table to its initial size.
 */
void hashmap_reset(struct hashmap *map)
{
    struct hashmap_entry *new_table;

    HASHMAP_ASSERT(map != NULL);

    hashmap_clear(map);
    if (map->table_size == map->table_size_init) {
        return;
    }
    new_table = (struct hashmap_entry *)GC_REALLOC(map->table,
        sizeof(struct hashmap_entry) * map->table_size_init);
    if (!new_table) {
        return;
    }
    map->table = new_table;
    map->table_size = map->table_size_init;
}

/*
 * Return the number of entries in the hash map.
 */
size_t hashmap_size(const struct hashmap *map)
{
    HASHMAP_ASSERT(map != NULL);

    return map->num_entries;
}

/*
 * Get a new hashmap iterator.  The iterator is an opaque
 * pointer that may be used with hashmap_iter_*() functions.
 * Hashmap iterators are INVALID after a put or remove operation is performed.
 * hashmap_iter_remove() allows safe removal during iteration.
 */
struct hashmap_iter *hashmap_iter(const struct hashmap *map)
{
    HASHMAP_ASSERT(map != NULL);

    if (!map->num_entries) {
        return NULL;
    }
    return (struct hashmap_iter *)hashmap_entry_get_populated(map,
        map->table);
}

/*
 * Return an iterator to the next hashmap entry.  Returns NULL if there are
 * no more entries.
 */
struct hashmap_iter *hashmap_iter_next(const struct hashmap *map,
    const struct hashmap_iter *iter)
{
    struct hashmap_entry *entry = (struct hashmap_entry *)iter;

    HASHMAP_ASSERT(map != NULL);

    if (!iter) {
        return NULL;
    }
    return (struct hashmap_iter *)hashmap_entry_get_populated(map,
        entry + 1);
}

/*
 * Remove the hashmap entry pointed to by this iterator and return an
 * iterator to the next entry.  Returns NULL if there are no more entries.
 */
struct hashmap_iter *hashmap_iter_remove(struct hashmap *map,
    const struct hashmap_iter *iter)
{
    struct hashmap_entry *entry = (struct hashmap_entry *)iter;

    HASHMAP_ASSERT(map != NULL);

    if (!iter) {
        return NULL;
    }
    if (!entry->key) {
        /* Iterator is invalid, so just return the next valid entry */
        return hashmap_iter_next(map, iter);
    }
    hashmap_entry_remove(map, entry);
    return (struct hashmap_iter *)hashmap_entry_get_populated(map, entry);
}

/*
 * Return the key of the entry pointed to by the iterator.
 */
const void *hashmap_iter_get_key(const struct hashmap_iter *iter)
{
    if (!iter) {
        return NULL;
    }
    return (const void *)((struct hashmap_entry *)iter)->key;
}

/*
 * Return the data of the entry pointed to by the iterator.
 */
void *hashmap_iter_get_data(const struct hashmap_iter *iter)
{
    if (!iter) {
        return NULL;
    }
    return ((struct hashmap_entry *)iter)->data;
}

/*
 * Set the data pointer of the entry pointed to by the iterator.
 */
void hashmap_iter_set_data(const struct hashmap_iter *iter, void *data)
{
    if (!iter) {
        return;
    }
    ((struct hashmap_entry *)iter)->data = data;
}

/*
 * Invoke func for each entry in the hashmap.  Unlike the hashmap_iter_*()
 * interface, this function supports calls to hashmap_remove() during iteration.
 * However, it is an error to put or remove an entry other than the current one,
 * and doing so will immediately halt iteration and return an error.
 * Iteration is stopped if func returns non-zero.  Returns func's return
 * value if it is < 0, otherwise, 0.
 */
int hashmap_foreach(const struct hashmap *map,
    int (*func)(const void *, void *, void *), void *arg)
{
    struct hashmap_entry *entry;
    size_t num_entries;
    const void *key;
    int rc;

    HASHMAP_ASSERT(map != NULL);
    HASHMAP_ASSERT(func != NULL);

    entry = map->table;
    for (entry = map->table; entry < &map->table[map->table_size];
        ++entry) {
        if (!entry->key) {
            continue;
        }
        num_entries = map->num_entries;
        key = entry->key;
        rc = func(entry->key, entry->data, arg);
        if (rc < 0) {
            return rc;
        }
        if (rc > 0) {
            return 0;
        }
        /* Run this entry again if func() deleted it */
        if (entry->key != key) {
            --entry;
        } else if (num_entries != map->num_entries) {
            /* Stop immediately if func put/removed another entry */
            return -1;
        }
    }
    return 0;
}

/*
 * Default hash function for string keys.
 * This is an implementation of the well-documented Jenkins one-at-a-time
 * hash function.
 */
size_t hashmap_hash_string(const void *key)
{
    const char *key_str = (const char *)key;
    size_t hash = 0;

    for (; *key_str; ++key_str) {
        hash += *key_str;
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

/*
 * Default key comparator function for string keys.
 */
int hashmap_compare_string(const void *a, const void *b)
{
    return strcmp((const char *)a, (const char *)b);
}

/*
 * Default key allocation function for string keys.  Use free() for the
 * key_free_func.
 */
void *hashmap_alloc_key_string(const void *key)
{
    return (void *)strdup((const char *)key);
}

/*
 * Case insensitive hash function for string keys.
 */
size_t hashmap_hash_string_i(const void *key)
{
    const char *key_str = (const char *)key;
    size_t hash = 0;

    for (; *key_str; ++key_str) {
        hash += tolower(*key_str);
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

/*
 * Case insensitive key comparator function for string keys.
 */
int hashmap_compare_string_i(const void *a, const void *b)
{
    return strcasecmp((const char *)a, (const char *)b);
}


#ifdef HASHMAP_METRICS
/*
 * Return the load factor.
 */
double hashmap_load_factor(const struct hashmap *map)
{
    HASHMAP_ASSERT(map != NULL);

    if (!map->table_size) {
        return 0;
    }
    return (double)map->num_entries / map->table_size;
}

/*
 * Return the average number of collisions per entry.
 */
double hashmap_collisions_mean(const struct hashmap *map)
{
    struct hashmap_entry *entry;
    size_t total_collisions = 0;

    HASHMAP_ASSERT(map != NULL);

    if (!map->num_entries) {
        return 0;
    }
    for (entry = map->table; entry < &map->table[map->table_size];
        ++entry) {
        if (!entry->key) {
            continue;
        }
        total_collisions += entry->num_collisions;
    }
    return (double)total_collisions / map->num_entries;
}

/*
 * Return the variance between entry collisions.  The higher the variance,
 * the more likely the hash function is poor and is resulting in clustering.
 */
double hashmap_collisions_variance(const struct hashmap *map)
{
    struct hashmap_entry *entry;
    double mean_collisions;
    double variance;
    double total_variance = 0;

    HASHMAP_ASSERT(map != NULL);

    if (!map->num_entries) {
        return 0;
    }
    mean_collisions = hashmap_collisions_mean(map);
    for (entry = map->table; entry < &map->table[map->table_size];
        ++entry) {
        if (!entry->key) {
            continue;
        }
        variance = (double)entry->num_collisions - mean_collisions;
        total_variance += variance * variance;
    }
    return total_variance / map->num_entries;
}
#endif
