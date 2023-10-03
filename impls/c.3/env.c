#include <assert.h>
#include "env.h"

MalEnvironment *make_environment(MalEnvironment *parent)
{
    MalEnvironment *environment = malloc(sizeof(MalEnvironment));

    if (environment == NULL)
    {
        return NULL;
    }

    HashMap *map = make_hashmap();

    if (map == NULL)
    {
        free(environment);

        return NULL;
    }

    environment->map = map;
    environment->parent = parent;

    return environment;
}

void free_environment(MalEnvironment *environment)
{
    free_hashmap(environment->map);
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
