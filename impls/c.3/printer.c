#include <string.h>
#include "printer.h"

void print_list_like(FILE *stream, MalValue *value, char *startToken, char *endToken, bool readably)
{
    fprintf(stream, startToken);

    if (value->list != NULL)
    {
        print(stream, value->list->value, readably);
        MalCell *cdr = value->list->cdr;

        while (cdr != NULL)
        {
            fprintf(stream, " ");
            print(stream, cdr->value, readably);
            cdr = cdr->cdr;
        }
    }

    fprintf(stream, endToken);
}

void print_string(FILE *stream, const char *value, bool readably)
{
    if (readably)
    {
        char *start = value;
        fprintf(stream, "\"");

        while (*start != '\0')
        {
            switch (*start)
            {
            case '\"':
                fputs("\\\"", stream);
                break;
            case '\\':
                fputs("\\\\", stream);
                break;
            case '\n':
                fputs("\\n", stream);
                break;

            default:
                fputc(*start, stream);
            }

            start++;
        }

        fprintf(stream, "\"");
    }
    else
    {
        fprintf(stream, "\"%s\"", value);
    }
}

void print_hash_map(FILE *stream, HashMap *hashMap, bool readably)
{
    fprintf(stream, "{");
    HashMapIterator it = hashmap_iterator(hashMap);
    bool first = true;

    while (hashmap_next(&it))
    {
        if (!first)
        {
            fprintf(stream, " ");
        }

        // keys are strings, so one cannot differentiate between keywords and standard strings
        if (*it.key == ':')
        {
            fprintf(stream, "%s", it.key);
        }
        else
        {
            print_string(stream, it.key, false);
        }

        fprintf(stream, " ");
        print(stream, (MalValue *)it.value, readably);
        first = false;
    }

    fprintf(stream, "}");
}

void print(FILE *stream, MalValue *value, bool readably)
{
    if (value->metadata != NULL)
    {
        fprintf(stream, "(with-meta ");
    }

    switch (value->valueType)
    {
    case MAL_LIST:
        print_list_like(stream, value, "(", ")", readably);
        break;

    case MAL_VECTOR:
        print_list_like(stream, value, "[", "]", readably);
        break;

    case MAL_STRING:
        print_string(stream, value->value, readably);
        break;

    case MAL_HASHMAP:
        print_hash_map(stream, value->hashMap, readably);
        break;

    case MAL_FIXNUM:
        fprintf(stream, "%ld", value->fixnum);
        break;

    default:
        fprintf(stream, "%s", value->value);
        break;
    }

    if (value->metadata != NULL)
    {
        fprintf(stream, " ");
        print_hash_map(stream, value->metadata, false);
        fprintf(stream, ")");
    }
}
