#include <stdlib.h>
#include "types.h"

// Env

Env *new_env(Env *outer, MalVal* binds, MalVal *exprs) {
    Env *e = MAL_GC_MALLOC(sizeof(Env));
    e->table = g_hash_table_new(g_str_hash, g_str_equal);
    e->outer = outer;

    if (binds && exprs) {
        assert_type(binds, MAL_LIST|MAL_VECTOR,
                    "new_env called with non-sequential bindings");
        assert_type(exprs, MAL_LIST|MAL_VECTOR,
                    "new_env called with non-sequential expressions");
        int binds_len = _count(binds),
            exprs_len = _count(exprs),
            varargs = 0, i;
        for (i=0; i<binds_len; i++) {
            if (i > exprs_len) { break; }
            if (_nth(binds, i)->val.string[0] == '&') {
                varargs = 1;
                env_set(e, _nth(binds, i+1)->val.string,
                        _slice(exprs, i, _count(exprs)));
                break;
            } else {
              env_set(e, _nth(binds, i)->val.string, _nth(exprs, i));
            }
        }
        assert(varargs || (binds_len == exprs_len),
               "Arity mismatch: %d formal params vs %d actual params",
               binds_len, exprs_len);

    }
    return e;
}

MalVal *env_get(Env *env, const char *key) {
    MalVal *val = g_hash_table_lookup(env->table, key);
    if (val) {
        return val;
    } else if (env->outer) {
        return env_get(env->outer, key);
    } else {
        return NULL;
    }
}

void env_set(Env *env, char *key, MalVal *val) {
    g_hash_table_insert(env->table, key, val);
}
