#include <inttypes.h>
#include <string.h>
#include "printer.h"
#include <ctype.h>
#include "gc.h"
#include <math.h>

#define INITIAL_LIST_BUFFER_SIZE 64
extern FILE *output_stream;
typedef struct MalPrintBuf
{
    char *buffer;
    size_t buf_size;
    size_t position;
} MalPrintBuf;

MalPrintBuf *make_print_buffer(size_t size)
{
    MalPrintBuf *buffer = mal_malloc(sizeof(MalPrintBuf));
    buffer->buf_size = size;
    buffer->position = 0;
    buffer->buffer = mal_calloc(buffer->buf_size, sizeof(char));

    return buffer;
}

void mal_strcat(MalPrintBuf *buffer, const char *value)
{
    size_t len = strlen(value);

    if (buffer->position + len + 1 > buffer->buf_size)
    {
        buffer->buf_size += (len + 1 > buffer->buf_size ? len + 1 : buffer->buf_size);
        buffer->buffer = mal_realloc(buffer->buffer, buffer->buf_size);
    }

    strcat(buffer->buffer + buffer->position, value);
    buffer->position += len;
}

void print_list_like(MalPrintBuf *buffer, MalCell *value, char *startToken, char *endToken, bool readably);
void print_string(MalPrintBuf *buffer, const char *value, bool readably);
void print_hash_map(MalPrintBuf *buffer, HashMap *hashMap, bool readably);

void pr_str_internal(MalPrintBuf *buffer, MalValue *value, bool readably)
{
    if (!value)
    {
        return;
    }

    if (value->metadata != NULL)
    {
        mal_strcat(buffer, "(with-meta ");
    }

    switch (value->valueType)
    {
    case MAL_LIST:
        print_list_like(buffer, value->list, "(", ")", readably);
        break;

    case MAL_VECTOR:
        print_list_like(buffer, value->list, "[", "]", readably);
        break;

    case MAL_STRING:
        print_string(buffer, value->value, readably);
        break;

    case MAL_HASHMAP:
        print_hash_map(buffer, value->hashMap, readably);
        break;

    case MAL_FIXNUM:
    {
        char tmp[32];
        sprintf(tmp, "%" PRId64 "", value->fixnum);
        mal_strcat(buffer, tmp);
    }
    break;

    case MAL_FUNCTION:
        mal_strcat(buffer, "#<function>");
        break;

    case MAL_CLOSURE:
    {
        MalClosure *closure = value->closure;
        mal_strcat(buffer, "#<function>:closure (fn* ");

        if (closure->rest_symbol)
        {
            mal_strcat(buffer, "( ");
            MalCell *binding = closure->bindings->list;

            while (binding)
            {
                pr_str_internal(buffer, binding->value, readably);

                if (binding->cdr)
                {
                    mal_strcat(buffer, " ");
                }

                binding = binding->cdr;
            }

            mal_strcat(buffer, "& ");
            pr_str_internal(buffer, closure->rest_symbol, readably);
        }
        else
        {
            print_list_like(buffer, value->closure->bindings->list, "(", ")", readably);
        }

        mal_strcat(buffer, ") ");
        pr_str_internal(buffer, value->closure->ast, readably);
        mal_strcat(buffer, ")");
    }
    break;

    default:
        mal_strcat(buffer, value->value);
        break;
    }

    if (value->metadata != NULL)
    {
        mal_strcat(buffer, " ");
        print_hash_map(buffer, value->metadata, false);
        mal_strcat(buffer, ")");
    }
}

void print_list_like(MalPrintBuf *buffer, MalCell *value, char *startToken, char *endToken, bool readably)
{
    mal_strcat(buffer, startToken);

    if (value != NULL)
    {
        pr_str_internal(buffer, value->value, readably);

        MalCell *cdr = value->cdr;

        while (cdr != NULL)
        {
            mal_strcat(buffer, " ");
            pr_str_internal(buffer, cdr->value, readably);
            cdr = cdr->cdr;
        }
    }

    mal_strcat(buffer, endToken);
}

void print_string(MalPrintBuf *buffer, const char *value, bool readably)
{
    if (readably)
    {
        mal_strcat(buffer, "\"");
        char str[2];
        str[1] = '\0';
        size_t position = 0;
        char current;

        while ((current = *(value + position)) != '\0')
        {
            switch (current)
            {
            case '\"':
                mal_strcat(buffer, "\\\"");
                break;
            case '\\':
                mal_strcat(buffer, "\\\\");
                break;
            case '\n':
                mal_strcat(buffer, "\\n");
                break;

            default:
                str[0] = current;
                mal_strcat(buffer, str);
            }

            position++;
        }
        
        mal_strcat(buffer, "\"");
    }
    else
    {
        mal_strcat(buffer, value);
    }
}

void print_hash_map(MalPrintBuf *buffer, HashMap *hashMap, bool readably)
{
    mal_strcat(buffer, "{");
    HashMapIterator it = hashmap_iterator(hashMap);
    bool first = true;

    while (hashmap_next(&it))
    {
        if (!first)
        {
            mal_strcat(buffer, " ");
        }

        // keys are strings, so one cannot differentiate between keywords and standard strings
        if (*it.key == ':')
        {
            mal_strcat(buffer, it.key);
        }
        else
        {
            print_string(buffer, it.key, false);
        }

        mal_strcat(buffer, " ");
        pr_str_internal(buffer, (MalValue *)it.value, readably);
        first = false;
    }

    mal_strcat(buffer, "}");
}

char *pr_str(MalValue *value, bool readably)
{
    MalPrintBuf *buffer = NULL;
    size_t buf_size = 32;

    if (!value)
    {
        return "";
    }

    switch (value->valueType)
    {
    case MAL_LIST:
    case MAL_VECTOR:
        buf_size = value->value != NULL ? INITIAL_LIST_BUFFER_SIZE : 3;
        break;

    case MAL_STRING:
        buf_size = strlen(value->value) + 2;
        break;

    case MAL_HASHMAP:
        buf_size = 2 * 10 * value->hashMap->length;
        break;

    case MAL_CLOSURE:
        buf_size = 64;
        break;

    case MAL_FIXNUM:
    case MAL_FUNCTION:
    default:
        break;
    }

    buffer = make_print_buffer(buf_size);

    pr_str_internal(buffer, value, readably);

    return buffer->buffer;
}

/*
TEST: '(pr-str "\\"")' -> ['',"\"\\\"\""] -> FAIL (line 316):
    Expected : '.*\n"\\\\"\\\\\\\\\\\\"\\\\""'
    Got      : '(pr-str "\\"")\n"\\"\\"\\""'
*/
MalValue *print_values_internal(MalCell *values, char *separator, bool readably)
{
    if (!values)
    {
        return make_string("", false);
    }

    MalCell *current = values;
    MalPrintBuf *buffer = make_print_buffer(64);

    while (current)
    {
        mal_strcat(buffer, pr_str(current->value, readably));
        current = current->cdr;

        if (current)
        {
            mal_strcat(buffer, separator);
        }
    }

    return make_string(buffer->buffer, false);
}

MalValue *print_values_readably(MalCell *values)
{
    return print_values_internal(values, " ", true);
}

MalValue *print_values(MalCell *values)
{
    return print_values_internal(values, "", false);
}

void print(FILE *stream, MalValue *value, bool readably)
{
    fprintf(stream, pr_str(value, readably));
}

MalValue *println(MalCell *values)
{
    MalValue *result = print_values_internal(values, " ", false);
    fprintf(output_stream, "%s\n", result->value);

    return &MAL_NIL;
}