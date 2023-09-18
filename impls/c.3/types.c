#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "types.h"

MalValue *new_value(enum MalValueType valueType)
{
    MalValue *value = malloc(sizeof(MalValue));
    value->valueType = valueType;
    value->value = NULL;

    return value;
}

MalValue *make_value(enum MalValueType valueType, char *value)
{
    MalValue *mal_value = new_value(valueType);
    mal_value->value = value;

    return mal_value;
}

MalValue *make_list()
{
    return new_value(MAL_LIST);
}

MalValue *make_vector()
{
    return new_value(MAL_VECTOR);
}

void push(MalValue *list, MalValue *value)
{
    assert(list->valueType == MAL_LIST||list->valueType == MAL_VECTOR);

    if (list->list == NULL)
    {
        list->list = malloc(sizeof(MalCell));
        list->list->cdr = NULL;
        list->list->value = value;
        return;
    }

    MalCell *cell = list->list;

    while (cell->cdr != NULL)
    {
        cell = cell->cdr;
    }

    cell->cdr = malloc(sizeof(MalCell));
    cell->cdr->cdr = NULL;
    cell->cdr->value = value;
}