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

void print_quoted_form(FILE *stream, char *op, MalValue *value, bool readably)
{
    fprintf(stream, "(%s ", op);
    print(stream, value, readably);
    fprintf(stream, ")");
}

void print_hash_map(FILE *stream, MalValue *value, bool readably)
{
    fprintf(stream, "{");
    //    print(stream, value, readably);
    fprintf(stream, "}");
}

void print_string(FILE *stream, MalValue *value, bool readably)
{
    if (readably)
    {
        char *start = value->value;
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
        fprintf(stream, "\"%s\"", value->value);
    }
}

void print(FILE *stream, MalValue *value, bool readably)
{
    switch (value->valueType)
    {
    case MAL_LIST:
        print_list_like(stream, value, "(", ")", readably);
        break;

    case MAL_VECTOR:
        print_list_like(stream, value, "[", "]", readably);
        break;

    case MAL_STRING:
        print_string(stream, value, readably);
        break;

    case MAL_QUOTED_FORM:
        print_quoted_form(stream, "quote", value->malValue, readably);
        break;

    case MAL_QUASI_QUOTED_FORM:
        print_quoted_form(stream, "quasiquote", value->malValue, readably);
        break;

    case MAL_UNQUOTE_FORM:
        print_quoted_form(stream, "unquote", value->malValue, readably);
        break;

    case MAL_SPLICE_UNQUOTE_FORM:
        print_quoted_form(stream, "splice-unquote", value->malValue, readably);
        break;

    case MAL_HASHMAP:
        print_hash_map(stream, value, readably);
        break;

    default:
        fprintf(stream, "%s", value->value);
        break;
    }
}
