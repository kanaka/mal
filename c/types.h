#ifndef __MAL_TYPES__
#define __MAL_TYPES__

#include <glib.h>

// State

struct MalVal; // pre-declare
extern struct MalVal *mal_error;

#define abort(format, ...) \
    { _error(format, ##__VA_ARGS__); return NULL; }

#define assert(test, format, ...) \
    if (!(test)) { \
        _error(format, ##__VA_ARGS__); \
        return NULL; \
    }

#define assert_type(mv, typ, format, ...) \
    if (!(mv->type & (typ))) { \
        _error(format, ##__VA_ARGS__); \
        return NULL; \
    }

typedef enum {
    MAL_NIL = 1,
    MAL_TRUE = 2,
    MAL_FALSE = 4,
    MAL_INTEGER = 8,
    MAL_FLOAT = 16,
    MAL_SYMBOL = 32,
    MAL_STRING = 64,
    MAL_LIST = 128,
    MAL_VECTOR = 256,
    MAL_HASH_MAP = 512,
    MAL_ATOM = 1024,
    MAL_FUNCTION_C = 2048,
    MAL_FUNCTION_MAL = 4096,
} MalType;


// Predeclare Env
typedef struct Env Env;

typedef struct MalVal {
    MalType type;
    struct MalVal *metadata;
    union {
        gint64 intnum;
        gdouble floatnum;
        char *string;
        GArray *array;
        GHashTable *hash_table;
        struct MalVal *atom_val;
        void *(*f0) ();
        void *(*f1) (void*);
        void *(*f2) (void*,void*);
        void *(*f3) (void*,void*,void*);
        void *(*f4) (void*,void*,void*,void*);
        void *(*f5) (void*,void*,void*,void*,void*);
        void *(*f6) (void*,void*,void*,void*,void*,void*);
        void *(*f7) (void*,void*,void*,void*,void*,void*,void*);
        void *(*f8) (void*,void*,void*,void*,void*,void*,void*,void*);
        void *(*f9) (void*,void*,void*,void*,void*,void*,void*,void*,void*);
        void *(*f10)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);
        void *(*f11)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*);
        void *(*f12)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*);
        void *(*f13)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*);
        void *(*f14)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*);
        void *(*f15)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*);
        void *(*f16)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*,void*);
        void *(*f17)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*,void*,void*);
        void *(*f18)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*,void*,void*,void*);
        void *(*f19)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*,void*,void*,void*,void*);
        void *(*f20)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,
                     void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);
        struct {
            struct MalVal *(*evaluator)(struct MalVal *, Env *);
            struct MalVal *args;
            struct MalVal *body;
            struct Env    *env;
        } func;
    } val;
    int func_arg_cnt;
    int ismacro;
} MalVal;

// Constants

extern MalVal mal_nil;
extern MalVal mal_true;
extern MalVal mal_false;


// Declare functions used internally (by other C code).
// Mal visible functions are "exported" in types_ns

MalVal *malval_new(MalType type, MalVal *metadata);
int malval_free(MalVal *mv);
MalVal *malval_new_integer(gint64 val);
MalVal *malval_new_float(gdouble val);
MalVal *malval_new_string(char *val);
MalVal *malval_new_symbol(char *val);
MalVal *malval_new_list(MalType type, GArray *val);
MalVal *malval_new_function(void *(*func)(void *), int arg_cnt, MalVal* metadata);

MalVal *hash_map(MalVal *args);
void _error(const char *fmt, ...);
MalVal *_list(int count, ...);

MalVal *apply(MalVal *f, MalVal *el);

char *_pr_str(MalVal *args, int print_readably);

MalVal *first(MalVal* seq);
MalVal *last(MalVal* seq);
MalVal *_slice(MalVal *seq, int start, int end);
MalVal *_nth(MalVal *seq, int idx);
MalVal *rest(MalVal *seq);

MalVal *_map2(MalVal *(*func)(void*, void*), MalVal *lst, void *arg2);

// These are just used by step2 and step3 before then type_ns environment is
// imported

MalVal *int_plus(MalVal *a, MalVal *b);
MalVal *int_minus(MalVal *a, MalVal *b);
MalVal *int_multiply(MalVal *a, MalVal *b);
MalVal *int_divide(MalVal *a, MalVal *b);

// Env

typedef struct Env {
    struct Env *outer;
    GHashTable *table;
} Env;

Env *new_env(Env *outer, MalVal* binds, MalVal *exprs);
Env *env_find(Env *env, char *key);
MalVal *env_get(Env *env, char *key);
Env *env_set(Env *env, char *key, MalVal *val);

// namespace of type functions
typedef struct {
    char *name;
    void *(*func)(void*);
    int arg_cnt;
} types_ns_entry;

extern types_ns_entry types_ns[49];

#endif
