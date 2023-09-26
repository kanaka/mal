#include "core.h"
#include "env.h"

MalValue *add(MalValue *left, MalValue *right)
{
}

MalEnvironment *make_initial_environment()
{
    MalEnvironment *environment = make_environment(NULL);

    return environment;
}