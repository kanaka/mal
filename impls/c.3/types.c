#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "gc.h"
#include "env.h"
#include "types.h"
#include "printer.h"

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
MalValue *new_function(MalValue *(*function)(MalCell *))
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

MalValue *make_error(char *fmt, ...)
{
    va_list arg_ptr;
    int result = -1;
    size_t buf_len = 64;
    char *message = "";

    while (result < 0)
    {
        char buffer[buf_len];
        va_start(arg_ptr, fmt);
        result = vsprintf(buffer, fmt, arg_ptr);

        if (result > 0)
        {
            message = (char *)mal_malloc(result + 1);
            strcpy(message, buffer);
        }
        else
        {
            buf_len *= 2;
        }

        va_end(arg_ptr);
    }

    return make_value(MAL_ERROR, message);
}

MalValue *make_fixnum(int64_t number)
{
    MalValue *value = new_value(MAL_FIXNUM);
    value->fixnum = number;

    return value;
}

MalValue *make_closure(MalEnvironment *outer, MalCell *context)
{
    if (!context->cdr)
    {
        return make_error("missing closure body: '%s'", print_values_readably(context)->value);
    }

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
                if (args->cdr->cdr)
                {
                    return make_error("only one symbol to receive the rest of the argument list is allowed: '(%s)'", print_values_readably(args)->value);
                }

                closure->rest_symbol = args->cdr->value;
                break;
            }

            return make_error("expected a symbol to receive the rest of the argument list: '(%s)'", print_values_readably(args)->value);
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