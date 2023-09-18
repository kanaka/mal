#include "printer.h"

void print_list_like(FILE *stream, MalValue *value, char *startToken, char *endToken)
{
    fprintf(stream, startToken);

    if (value->list != NULL)
    {
        print(stream, value->list->value);
        MalCell *cdr = value->list->cdr;

        while (cdr != NULL)
        {
            fprintf(stream, " ");
            print(stream, cdr->value);
            cdr = cdr->cdr;
        }
    }

    fprintf(stream, endToken);
}

void print_quoted_form(FILE *stream, char *op, MalValue *value)
{
    fprintf(stream, "(%s ", op);
    print(stream, value);
    fprintf(stream, ")");
}

void print_hash_map(FILE *stream, MalValue *value) {
    fprintf(stream, "{");
//    print(stream, value);
    fprintf(stream, "}");

}

void print(FILE *stream, MalValue *value)
{
    switch (value->valueType)
    {
    case MAL_LIST:
        print_list_like(stream, value, "(", ")");
        break;

    case MAL_VECTOR:
        print_list_like(stream, value, "[", "]");
        break;

    case MAL_STRING:
        fprintf(stream, "\"%s\"", value->value);
        break;

    case MAL_QUOTED_FORM:
        print_quoted_form(stream, "quote", value->malValue);
        break;

    case MAL_QUASI_QUOTED_FORM:
        print_quoted_form(stream, "quasiquote", value->malValue);
        break;

    case MAL_UNQUOTE_FORM:
        print_quoted_form(stream, "unquote", value->malValue);
        break;

    case MAL_SPLICE_UNQUOTE_FORM:
        print_quoted_form(stream, "splice-unquote", value->malValue);
        break;

    case MAL_HASHMAP:
        print_hash_map(stream, value);
        break;

    default:
        fprintf(stream, "%s", value->value);
        break;
    }
}
