/*
 * Copyright (c) 2016-2018 David Leeds <davidesleeds@gmail.com>
 *
 * Hashmap is free software; you can redistribute it and/or modify
 * it under the terms of the MIT license. See LICENSE for details.
 */

#ifndef __HASHMAP_H__
#define __HASHMAP_H__

#include <stddef.h>

/*
 * Define HASHMAP_METRICS to compile in performance analysis
 * functions for use in assessing hash function performance.
 */
/* #define HASHMAP_METRICS */

/*
 * Define HASHMAP_NOASSERT to compile out all assertions used internally.
 */
/* #define HASHMAP_NOASSERT */

/*
 * Macros to declare type-specific versions of hashmap_*() functions to
 * allow compile-time type checking and avoid the need for type casting.
 */
#define HASHMAP_FUNCS_DECLARE(name, key_type, data_type)                \
    data_type *name##_hashmap_put(struct hashmap *map,                  \
            const key_type *key, data_type *data);                      \
    data_type *name##_hashmap_get(const struct hashmap *map,            \
            const key_type *key);                                       \
    data_type *name##_hashmap_remove(struct hashmap *map,               \
            const key_type *key);                                       \
    const key_type *name##_hashmap_iter_get_key(                        \
            const struct hashmap_iter *iter);                           \
    data_type *name##_hashmap_iter_get_data(                            \
            const struct hashmap_iter *iter);                           \
    void name##_hashmap_iter_set_data(const struct hashmap_iter *iter,  \
            data_type *data);                                           \
    int name##_hashmap_foreach(const struct hashmap *map,               \
            int (*func)(const key_type *, data_type *, void *), void *arg);

#define HASHMAP_FUNCS_CREATE(name, key_type, data_type)                 \
    data_type *name##_hashmap_put(struct hashmap *map,                  \
            const key_type *key, data_type *data)                       \
    {                                                                   \
        return (data_type *)hashmap_put(map, (const void *)key,         \
                (void *)data);                                          \
    }                                                                   \
    data_type *name##_hashmap_get(const struct hashmap *map,            \
            const key_type *key)                                        \
    {                                                                   \
        return (data_type *)hashmap_get(map, (const void *)key);        \
    }                                                                   \
    data_type *name##_hashmap_remove(struct hashmap *map,               \
            const key_type *key)                                        \
    {                                                                   \
        return (data_type *)hashmap_remove(map, (const void *)key);     \
    }                                                                   \
    const key_type *name##_hashmap_iter_get_key(                        \
            const struct hashmap_iter *iter)                            \
    {                                                                   \
        return (const key_type *)hashmap_iter_get_key(iter);            \
    }                                                                   \
    data_type *name##_hashmap_iter_get_data(                            \
            const struct hashmap_iter *iter)                            \
    {                                                                   \
        return (data_type *)hashmap_iter_get_data(iter);                \
    }                                                                   \
    void name##_hashmap_iter_set_data(const struct hashmap_iter *iter,  \
            data_type *data)                                            \
    {                                                                   \
        hashmap_iter_set_data(iter, (void *)data);                      \
    }                                                                   \
    struct __##name##_hashmap_foreach_state {                           \
        int (*func)(const key_type *, data_type *, void *);             \
        void *arg;                                                      \
    };                                                                  \
    static inline int __##name##_hashmap_foreach_callback(              \
            const void *key, void *data, void *arg)                     \
    {                                                                   \
        struct __##name##_hashmap_foreach_state *s =                    \
            (struct __##name##_hashmap_foreach_state *)arg;             \
        return s->func((const key_type *)key,                           \
                (data_type *)data, s->arg);                             \
    }                                                                   \
    int name##_hashmap_foreach(const struct hashmap *map,               \
            int (*func)(const key_type *, data_type *, void *),         \
            void *arg)                                                  \
    {                                                                   \
        struct __##name##_hashmap_foreach_state s = { func, arg };      \
        return hashmap_foreach(map,                                     \
            __##name##_hashmap_foreach_callback, &s);                   \
    }


struct hashmap_iter;
struct hashmap_entry;

/*
 * The hashmap state structure.
 */
struct hashmap {
    size_t table_size_init;
    size_t table_size;
    size_t num_entries;
    struct hashmap_entry *table;
    size_t (*hash)(const void *);
    int (*key_compare)(const void *, const void *);
    void *(*key_alloc)(const void *);
    void (*key_free)(void *);
};

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
    size_t initial_size);

/*
 * Free the hashmap and all associated memory.
 */
void hashmap_destroy(struct hashmap *map);

/*
 * Enable internal memory allocation and management of hash keys.
 */
void hashmap_set_key_alloc_funcs(struct hashmap *map,
    void *(*key_alloc_func)(const void *),
    void (*key_free_func)(void *));

/*
 * Add an entry to the hashmap.  If an entry with a matching key already
 * exists and has a data pointer associated with it, the existing data
 * pointer is returned, instead of assigning the new value.  Compare
 * the return value with the data passed in to determine if a new entry was
 * created.  Returns NULL if memory allocation failed.
 */
void *hashmap_put(struct hashmap *map, const void *key, void *data);

/*
 * Return the data pointer, or NULL if no entry exists.
 */
void *hashmap_get(const struct hashmap *map, const void *key);

/*
 * Remove an entry with the specified key from the map.
 * Returns the data pointer, or NULL, if no entry was found.
 */
void *hashmap_remove(struct hashmap *map, const void *key);

/*
 * Remove all entries.
 */
void hashmap_clear(struct hashmap *map);

/*
 * Remove all entries and reset the hash table to its initial size.
 */
void hashmap_reset(struct hashmap *map);

/*
 * Return the number of entries in the hash map.
 */
size_t hashmap_size(const struct hashmap *map);

/*
 * Get a new hashmap iterator.  The iterator is an opaque
 * pointer that may be used with hashmap_iter_*() functions.
 * Hashmap iterators are INVALID after a put or remove operation is performed.
 * hashmap_iter_remove() allows safe removal during iteration.
 */
struct hashmap_iter *hashmap_iter(const struct hashmap *map);

/*
 * Return an iterator to the next hashmap entry.  Returns NULL if there are
 * no more entries.
 */
struct hashmap_iter *hashmap_iter_next(const struct hashmap *map,
    const struct hashmap_iter *iter);

/*
 * Remove the hashmap entry pointed to by this iterator and returns an
 * iterator to the next entry.  Returns NULL if there are no more entries.
 */
struct hashmap_iter *hashmap_iter_remove(struct hashmap *map,
    const struct hashmap_iter *iter);

/*
 * Return the key of the entry pointed to by the iterator.
 */
const void *hashmap_iter_get_key(const struct hashmap_iter *iter);

/*
 * Return the data of the entry pointed to by the iterator.
 */
void *hashmap_iter_get_data(const struct hashmap_iter *iter);

/*
 * Set the data pointer of the entry pointed to by the iterator.
 */
void hashmap_iter_set_data(const struct hashmap_iter *iter, void *data);

/*
 * Invoke func for each entry in the hashmap.  Unlike the hashmap_iter_*()
 * interface, this function supports calls to hashmap_remove() during iteration.
 * However, it is an error to put or remove an entry other than the current one,
 * and doing so will immediately halt iteration and return an error.
 * Iteration is stopped if func returns non-zero.  Returns func's return
 * value if it is < 0, otherwise, 0.
 */
int hashmap_foreach(const struct hashmap *map,
    int (*func)(const void *, void *, void *), void *arg);

/*
 * Default hash function for string keys.
 * This is an implementation of the well-documented Jenkins one-at-a-time
 * hash function.
 */
size_t hashmap_hash_string(const void *key);

/*
 * Default key comparator function for string keys.
 */
int hashmap_compare_string(const void *a, const void *b);

/*
 * Default key allocation function for string keys.  Use free() for the
 * key_free_func.
 */
void *hashmap_alloc_key_string(const void *key);

/*
 * Case insensitive hash function for string keys.
 */
size_t hashmap_hash_string_i(const void *key);

/*
 * Case insensitive key comparator function for string keys.
 */
int hashmap_compare_string_i(const void *a, const void *b);


#ifdef HASHMAP_METRICS
/*
 * Return the load factor.
 */
double hashmap_load_factor(const struct hashmap *map);

/*
 * Return the average number of collisions per entry.
 */
double hashmap_collisions_mean(const struct hashmap *map);

/*
 * Return the variance between entry collisions.  The higher the variance,
 * the more likely the hash function is poor and is resulting in clustering.
 */
double hashmap_collisions_variance(const struct hashmap *map);
#endif


#endif /* __HASHMAP_H__ */

