#include "core.h"
#include "env.h"

MalValue *add(MalCell *values)
{
    int64_t result = 0;
    MalCell *cdr = values->cdr;

    if (values->value)
    {
        result = values->value->fixnum;
    }

    while (cdr != NULL && cdr->value != NULL)
    {
        result += cdr->value->fixnum;
        cdr = cdr->cdr;
    }

    return make_fixnum(result);
}

MalValue *subtract(MalCell *values)
{
    int64_t result = 0;
    MalCell *cdr = values->cdr;

    if (values->value)
    {
        result = values->value->fixnum;
    }

    while (cdr != NULL && cdr->value != NULL)
    {
        result -= cdr->value->fixnum;
        cdr = cdr->cdr;
    }

    return make_fixnum(result);
}

MalValue *multiply(MalCell *values)
{
    int64_t result = 0;
    MalCell *cdr = values->cdr;

    if (values->value)
    {
        result = values->value->fixnum;
    }

    while (cdr != NULL && cdr->value != NULL)
    {
        result *= cdr->value->fixnum;
        cdr = cdr->cdr;
    }

    return make_fixnum(result);
}

MalValue *divide(MalCell *values)
{
    int64_t result = 0;
    MalCell *cdr = values->cdr;

    if (values->value)
    {
        result = values->value->fixnum;
    }

    while (cdr != NULL && cdr->value != NULL)
    {
        result /= cdr->value->fixnum;
        cdr = cdr->cdr;
    }

    return make_fixnum(result);
}

MalEnvironment *make_initial_environment()
{
    MalEnvironment *environment = make_environment(NULL);

    hashmap_put(environment->map, "+", new_function(add));
    hashmap_put(environment->map, "-", new_function(subtract));
    hashmap_put(environment->map, "*", new_function(multiply));
    hashmap_put(environment->map, "/", new_function(divide));

    return environment;
}