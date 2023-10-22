#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gc.h"
#include "env.h"
#include "error.h"
#include "types.h"

MalValue MAL_NIL = {
    .valueType = MAL_SYMBOL,
    .value = "nil"};

MalValue MAL_FALSE = {
    .valueType = MAL_SYMBOL,
    .value = "false"};

MalValue MAL_TRUE = {
    .valueType = MAL_SYMBOL,
    .value = "true"};

MalValue MAL_EOF = {
    .valueType = MAL_SYMBOL,
    .value = "EOF"};

MalValue *new_value(enum MalValueType valueType)
{
    MalValue *value = mal_calloc(1, sizeof(MalValue));
    value->valueType = valueType;

    return value;
}

// FIXME: determine function signature for registering functions as MalValue-objects
MalValue *new_function(MalValue *(*function)(MalCell *, MalEnvironment *))
{
    MalValue *fn = new_value(MAL_FUNCTION);
    fn->function = function;

    return fn;
}

MalValue *make_value(enum MalValueType valueType, const char *value)
{
    MalValue *mal_value = new_value(valueType);
    mal_value->value = value;

    return mal_value;
}

MalValue *make_fixnum(int64_t number)
{
    MalValue *value = new_value(MAL_FIXNUM);
    value->fixnum = number;

    return value;
}

MalValue *make_closure(MalEnvironment *outer, MalCell *context)
{
    MalValue *value = new_value(MAL_CLOSURE);
    MalClosure *closure = mal_calloc(1, sizeof(MalClosure));
    closure->environment = make_environment(outer, NULL, NULL, NULL);
    closure->ast = context->cdr->value;
    closure->bindings = context->value;

    MalCell *args = context->value->list;
    MalCell *marker = NULL;

    while (args)
    {
        if (strcmp("&", args->value->value) == 0)
        {
            marker = args;

            if (args->cdr)
            {
                closure->rest_symbol = args->cdr->value;
            }
            else
            {
                register_error(INVALID_ARGUMENT_COUNT, "expected a symbol to receive rest of argument list");

                return NULL;
            }

            break;
        }

        args = args->cdr;
    }

    args = context->value->list;

    if (marker)
    {
        closure->bindings = make_list(NULL);

        while (args != marker)
        {
            push(closure->bindings, args->value);
            args = args->cdr;
        }
    }
    value->closure = closure;

    return value;
}

MalValue *make_string(char *value, bool unescape)
{
    MalValue *_value = new_value(MAL_STRING);

    if (!unescape)
    {
        _value->value = value;
        return _value;
    }
    
    size_t len = strlen(value);
    char *result = mal_calloc(len, sizeof(char));

    for (int i = 0, j = 0; i < len; i++, j++)
    {
        if (value[i] == '\\')
        {
            switch (value[i + 1])
            {
            case '\\':
                result[j] = '\\';
                i++;
                break;

            case 'n':
                result[j] = '\n';
                i++;
                break;

            case '"':
                result[j] = '"';
                i++;
                break;

            default:
                result[j] = value[i + 1];
                break;
            }
        }
        else
        {
            result[j] = value[i];
        }
    }

    _value->value = result;
    return _value;
}

MalValue *make_list(MalCell *values)
{
    MalValue *result = new_value(MAL_LIST);

    if (values && values->value)
    {
        result->list = values;
    }

    return result;
}

void push(MalValue *list, MalValue *value)
{
    assert(list->valueType == MAL_LIST || list->valueType == MAL_VECTOR);

    if (list->list == NULL)
    {
        list->list = mal_malloc(sizeof(MalCell));
        list->list->cdr = NULL;
        list->list->value = value;
        return;
    }

    MalCell *cell = list->list;

    while (cell->cdr != NULL)
    {
        cell = cell->cdr;
    }

    cell->cdr = mal_malloc(sizeof(MalCell));
    cell->cdr->cdr = NULL;
    cell->cdr->value = value;
}

const char *put(MalValue *map, const char *key, MalValue *value)
{
    assert(map->valueType == MAL_HASHMAP);

    return hashmap_put(map->hashMap, key, value);
}

void setMetadata(MalValue *value, HashMap *metadata)
{
    value->metadata = metadata;
}