#include <assert.h>
#include "env.h"
#include "gc.h"

void free_environment(MalEnvironment *environment)
{
    if (environment->map)
    {
        free_hashmap(environment->map);
    }

    environment->parent = NULL;
    environment->map = NULL;

    free(environment);
}

MalEnvironment *find_environment(MalEnvironment *start, MalValue *symbol)
{
    assert(symbol->valueType == MAL_SYMBOL);

    MalEnvironment *env = start;
    MalValue *value = NULL;

    while (env != NULL && value == NULL)
    {
        value = hashmap_get(env->map, symbol->value);

        if (value)
        {
            break;
        }

        env = env->parent;
    }

    return env;
}

MalValue *lookup_in_environment(MalEnvironment *environment, MalValue *symbol)
{
    assert(symbol->valueType == MAL_SYMBOL);
    MalEnvironment *env = environment;
    MalValue *value = NULL;

    while (env != NULL && value == NULL)
    {
        value = hashmap_get(env->map, symbol->value);
        env = env->parent;
    }

    return value;
}

bool set_in_environment(MalEnvironment *environment, MalValue *symbol, MalValue *value)
{
    assert(symbol->valueType == MAL_SYMBOL);
    MalValue *oldValue = hashmap_get(environment->map, symbol->value);

    hashmap_put(environment->map, symbol->value, value);

    return oldValue != NULL;
}

MalEnvironment *make_environment(MalEnvironment *parent, MalCell *binds, MalCell *exprs, MalValue *rest_symbol)
{
    MalEnvironment *environment = mal_malloc(sizeof(MalEnvironment));

    if (environment == NULL)
    {
        return NULL;
    }

    HashMap *map = make_hashmap();

    if (map == NULL)
    {
        free_environment(environment);

        return NULL;
    }

    environment->map = map;
    environment->parent = parent;

    MalCell *_binds = binds;
    MalCell *_exprs = exprs;

    while (_binds && _exprs && _binds->value && _exprs && _exprs->value)
    {
        assert(_binds->value->valueType == MAL_SYMBOL);
        set_in_environment(environment, _binds->value, _exprs->value);
        _binds = _binds->cdr;
        _exprs = _exprs->cdr;
    }

    if (rest_symbol)
    {
        set_in_environment(environment, rest_symbol, make_list(_exprs));
    }

    return environment;
}
