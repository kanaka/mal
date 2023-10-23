#include <assert.h>
#include <string.h>
#include "core.h"
#include "env.h"
#include "printer.h"
#include "types.h"

extern FILE *output_stream;

bool is_equal_by_type(MalValue *left, MalValue *right);
bool is_equal(MalValue *left, MalValue *right);
bool are_lists_equal(MalCell *left, MalCell *right);

MalValue *add(MalCell *values, MalEnvironment *environment)
{
    MalCell *current = values;
    int64_t result = 0;

    if (current)
    {
        MalCell *cdr = current->cdr;

        if (current->value)
        {
            result = current->value->fixnum;
        }

        while (cdr != NULL && cdr->value != NULL)
        {
            result += cdr->value->fixnum;
            cdr = cdr->cdr;
        }
    }

    return make_fixnum(result);
}

MalValue *subtract(MalCell *values, MalEnvironment *environment)
{
    MalCell *current = values;

    if (!current)
    {
        return NULL;
    }

    int64_t result = 0;
    MalCell *cdr = current->cdr;

    if (current->value)
    {
        result = current->value->fixnum;
    }

    while (cdr != NULL && cdr->value != NULL)
    {
        result -= cdr->value->fixnum;
        cdr = cdr->cdr;
    }

    return make_fixnum(result);
}

MalValue *multiply(MalCell *values, MalEnvironment *environment)
{
    MalCell *current = values;
    int64_t result = 1;

    while (current != NULL && current->value != NULL)
    {
        result *= current->value->fixnum;
        current = current->cdr;
    }

    return make_fixnum(result);
}

MalValue *divide(MalCell *values, MalEnvironment *environment)
{
    MalCell *current = values;
    int64_t result = 1;

    while (current != NULL && current->value != NULL)
    {
        result /= current->value->fixnum;
        current = current->cdr;
    }

    return make_fixnum(result);
}

MalValue *prn(MalCell *values, MalEnvironment *environment)
{
    print(output_stream, print_values_readably(values, environment), false);
    fprintf(output_stream, "\n");
    return &MAL_NIL;
}

MalValue *list(MalCell *values, MalEnvironment *environment)
{
    MalValue *result = new_value(MAL_LIST);

    if (values)
    {
        result->list = values;
    }

    return result;
}

MalValue *list_p(MalCell *value, MalEnvironment *environment)
{
    return (value->value->valueType == MAL_LIST) ? &MAL_TRUE : &MAL_FALSE;
}

MalValue *empty_p(MalCell *value, MalEnvironment *environment)
{
    if (value->value->valueType == MAL_LIST || value->value->valueType == MAL_VECTOR)
    {
        if (value->value->list && value->value->list->value)
        {
            return &MAL_FALSE;
        }

        return &MAL_TRUE;
    }

    return &MAL_FALSE;
}

int64_t _count(MalCell *value, MalEnvironment *environment)
{
    int64_t count = 0;
    MalCell *current = value;

    if (value->value->valueType == MAL_LIST || value->value->valueType == MAL_VECTOR)
    {
        current = value->value->list;
    }
    else if (current->value == &MAL_NIL)
    {
        current = NULL;
    }

    while (current && current->value)
    {
        count++;
        current = current->cdr;
    }

    return count;
}

MalValue *count(MalCell *value, MalEnvironment *environment)
{
    return make_fixnum(_count(value, environment));
}

MalValue *greater_than(MalCell *values, MalEnvironment *environment)
{
    if (_count(values, environment) != 2)
    {
        return make_error("Invalid count of arguments. Two arguments expected: '%s'!", print_values_readably(values, environment)->value);
    }

    MalValue *first = values->value;
    MalValue *second = values->cdr->value;

    if (MAL_FIXNUM != first->valueType || MAL_FIXNUM != second->valueType)
    {
        return make_error("Invalid argument. Can only compare fixnums: '%s'!", print_values_readably(values, environment)->value);
    }

    return first->fixnum > second->fixnum ? &MAL_TRUE : &MAL_FALSE;
}

MalValue *less_than(MalCell *values, MalEnvironment *environment)
{
    if (_count(values, environment) != 2)
    {
        return make_error("Invalid count of arguments. Two arguments expected: '%s'!", print_values_readably(values, environment)->value);
    }

    MalValue *first = values->value;
    MalValue *second = values->cdr->value;

    if (MAL_FIXNUM != first->valueType || MAL_FIXNUM != second->valueType)
    {
        return make_error("Invalid argument. Can only compare fixnums: '%s'!", print_values_readably(values, environment)->value);
    }

    return first->fixnum < second->fixnum ? &MAL_TRUE : &MAL_FALSE;
}

MalValue *less_than_or_equal_to(MalCell *values, MalEnvironment *environment)
{
    if (_count(values, environment) != 2)
    {
        return make_error("Invalid count of arguments. Two arguments expected: '%s'!", print_values_readably(values, environment)->value);
    }

    MalValue *first = values->value;
    MalValue *second = values->cdr->value;

    if (MAL_FIXNUM != first->valueType || MAL_FIXNUM != second->valueType)
    {
        return make_error("Invalid argument. Can only compare fixnums: '%s'!", print_values_readably(values, environment)->value);
    }

    return first->fixnum <= second->fixnum ? &MAL_TRUE : &MAL_FALSE;
}

MalValue *greater_than_or_equal_to(MalCell *values, MalEnvironment *environment)
{
    if (_count(values, environment) != 2)
    {
        return make_error("Invalid count of arguments. Two arguments expected: '%s'!", print_values_readably(values, environment)->value);
    }

    MalValue *first = values->value;
    MalValue *second = values->cdr->value;

    if (MAL_FIXNUM != first->valueType || MAL_FIXNUM != second->valueType)
    {
        return make_error("Invalid argument. Can only compare fixnums: '%s'!", print_values_readably(values, environment)->value);
    }

    return first->fixnum >= second->fixnum ? &MAL_TRUE : &MAL_FALSE;
}

bool is_equal_by_type(MalValue *left, MalValue *right)
{
    assert(left->valueType == right->valueType);

    bool result = false;
    switch (left->valueType)
    {
    case MAL_FIXNUM:
        result = left->fixnum == right->fixnum;
        break;

    case MAL_STRING:
    case MAL_KEYWORD:
    case MAL_SYMBOL:
        result = strcmp(left->value, right->value) == 0;
        break;

    case MAL_LIST:
    case MAL_VECTOR:
        result = are_lists_equal(left->list, right->list);
        break;

    default:
        break;
    }

    return result;
}

bool is_equal(MalValue *left, MalValue *right)
{
    if (left->valueType == right->valueType)
    {
        return is_equal_by_type(left, right);
    }

    if (((left->valueType == MAL_LIST && right->valueType == MAL_VECTOR) || (left->valueType == MAL_VECTOR && right->valueType == MAL_LIST)))
    {
        return are_lists_equal(left->list, right->list);
    }

    return false;
}

bool are_lists_equal(MalCell *left, MalCell *right)
{
    if ((left && !right) || (!left && right))
    {
        return false;
    }

    bool result = true;

    while (left)
    {
        if (!right || !is_equal(left->value, right->value))
        {
            result = false;
            break;
        }

        left = left->cdr;
        right = right->cdr;
    }

    return result;
}

MalValue *equals(MalCell *values, MalEnvironment *environment)
{
    if (!values || (!values->value && !values->cdr->value))
    {
        return make_error("Invalid count of arguments. Two arguments expected: '%s'!", print_values_readably(values, environment)->value);
    }

    MalCell *left = values;
    MalCell *right = values->cdr;

    if (left->value == right->value)
    {
        return &MAL_TRUE;
    }

    if (is_equal(left->value, right->value))
    {
        return &MAL_TRUE;
    }

    if ((empty_p(left, environment) == &MAL_TRUE && &MAL_NIL == right->value) || (empty_p(right, environment) == &MAL_TRUE && &MAL_NIL == left->value))
    {
        return &MAL_FALSE;
    }

    return &MAL_FALSE;
}

HashMap *core_namespace()
{

    HashMap *ns = make_hashmap();

    hashmap_put(ns, "+", new_function(add));
    hashmap_put(ns, "-", new_function(subtract));
    hashmap_put(ns, "*", new_function(multiply));
    hashmap_put(ns, "/", new_function(divide));

    return ns;
}

MalEnvironment *make_initial_environment()
{
    MalEnvironment *environment = make_environment(NULL, NULL, NULL, NULL);
    HashMap *ns = core_namespace();

    HashMapIterator it = hashmap_iterator(ns);

    while (hashmap_next(&it))
    {
        set_in_environment(environment, make_value(MAL_SYMBOL, it.key), it.value);
    }

    set_in_environment(environment, make_value(MAL_SYMBOL, "prn"), new_function(prn));
    set_in_environment(environment, make_value(MAL_SYMBOL, "println"), new_function(println));
    set_in_environment(environment, make_value(MAL_SYMBOL, "pr-str"), new_function(print_values_readably));
    set_in_environment(environment, make_value(MAL_SYMBOL, "str"), new_function(print_values));
    set_in_environment(environment, make_value(MAL_SYMBOL, "list"), new_function(list));
    set_in_environment(environment, make_value(MAL_SYMBOL, "list?"), new_function(list_p));
    set_in_environment(environment, make_value(MAL_SYMBOL, "empty?"), new_function(empty_p));
    set_in_environment(environment, make_value(MAL_SYMBOL, "count"), new_function(count));
    set_in_environment(environment, make_value(MAL_SYMBOL, ">"), new_function(greater_than));
    set_in_environment(environment, make_value(MAL_SYMBOL, ">="), new_function(greater_than_or_equal_to));
    set_in_environment(environment, make_value(MAL_SYMBOL, "<"), new_function(less_than));
    set_in_environment(environment, make_value(MAL_SYMBOL, "<="), new_function(less_than_or_equal_to));
    set_in_environment(environment, make_value(MAL_SYMBOL, "="), new_function(equals));
    set_in_environment(environment, &MAL_NIL, &MAL_NIL);
    set_in_environment(environment, &MAL_TRUE, &MAL_TRUE);
    set_in_environment(environment, &MAL_FALSE, &MAL_FALSE);

    return environment;
}