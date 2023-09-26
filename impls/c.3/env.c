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

MalEnvironment *free_environment(MalEnvironment *environment)
{
    free_hashmap(environment->map);
    environment->parent = NULL;
    environment->map = NULL;

    free(environment);
}

MalValue *lookup_in_environment(MalEnvironment *environment, char *key)
{
    MalEnvironment *env = environment;
    MalValue *value = NULL;

    while (env != NULL && value == NULL)
    {
        value = hashmap_get(env->map, key);
        env = env->parent;
    }

    return value;
}