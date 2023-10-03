#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "env.h"
#include "error.h"
#include "libs/readline/readline.h"
#include "printer.h"
#include "reader.h"
#include "token.h"
#include "types.h"

static const char *HISTORY_FILENAME = ".mal_history";

MalValue *READ(Reader *reader)
{
    return read_str(reader);
}

MalValue *EVAL(MalValue *value, MalEnvironment *environment, MalError *error);

MalValue *eval_ast(MalValue *value, MalEnvironment *environment, MalError *error)
{
    MalValue *result = NULL;

    switch (value->valueType)
    {
    case MAL_SYMBOL:
        MalValue *symbol = lookup_in_environment(environment, value);

        if (symbol)
        {
            result = symbol;
        }
        else
        {
            error->errno = SYMBOL_NOT_FOUND;
            error->args = malloc(sizeof(char **[1]));
            error->args[0] = value->value;
        }
        break;

    case MAL_LIST:
    case MAL_VECTOR:
    {
        MalCell *cdr = value->list->cdr;
        MalValue *tmp = EVAL(value->list->value, environment, error);

        if (tmp == NULL)
        {
            return NULL;
        }

        MalValue *list = new_value(value->valueType);
        push(list, tmp);

        while (cdr != NULL)
        {
            tmp = EVAL(cdr->value, environment, error);

            if (tmp == NULL)
            {
                return NULL;
            }

            push(list, tmp);
            cdr = cdr->cdr;
        }

        result = list;
    }
    break;

    case MAL_HASHMAP:
    {
        MalValue *h = new_value(MAL_HASHMAP);
        h->hashMap = make_hashmap();
        MalValue *r = NULL;
        HashMapIterator it = hashmap_iterator(value->hashMap);

        while (hashmap_next(&it))
        {
            r = EVAL(it.value, environment, error);

            if (r)
            {
                put(h, it.key, r);
            }
            else
            {
                free_hashmap(h->hashMap);
                return NULL;
            }
        }

        result = h;
    }
    break;

    default:
        result = value;
        break;
    }

    return result;
}

MalValue *EVAL(MalValue *value, MalEnvironment *environment, MalError *error)
{
    MalValue *result = NULL;

    switch (value->valueType)
    {
    case MAL_LIST:
    case MAL_VECTOR:
        MalCell *head = value->list;

        // ast is a empty list: return ast unchanged.
        if (head == NULL)
        {
            result = value;
        }
        else
        {
            if (value->valueType == MAL_LIST)
            {
                if (head->value->valueType == MAL_SYMBOL)
                {
                    if (strcmp("def!", head->value->value) == 0)
                    {
                        MalValue *t = EVAL(head->cdr->cdr->value, environment, error);

                        if (set_in_environment(environment, head->cdr->value, t))
                        {
                            // FIXME: Unterscheidung zwischen Fehler und Warnung
                            error->errno = VALUE_REDEFINED;
                            error->args = malloc(sizeof(char **[1]));
                            error->args[0] = head->cdr->value->value;
                        }
                        result = t;
                        break;
                    }
                    else if (strcmp("let*", head->value->value) == 0)
                    {
                        break;
                    }
                }
            }

            // ast is a list: call eval_ast to get a new evaluated list.
            // Take the first item of the evaluated list and call it as
            // function using the rest of the evaluated list as its arguments.
            MalValue *tmp = eval_ast(value, environment, error);

            if (tmp)
            {
                head = tmp->list;
                result = head->value->valueType == MAL_FUNCTION ? head->value->function(head->cdr) : tmp;
            }
        }

        break;

    default:
        // ast is not a list: then return the result of calling eval_ast on it.
        result = eval_ast(value, environment, error);
        break;
    }

    return result;
}

void PRINT(MalValue *value)
{
    print(stdout, value, true);
    fprintf(stdout, "\n");
}

void rep(char *input, MalEnvironment *environment)
{
    Reader reader = {.input = input};
    Token token = {};
    reader.token = &token;

    MalValue *value = READ(&reader);

    if (reader.errno == SUCCESS)
    {
        MalError error = {.errno = SUCCESS};
        MalValue *result = EVAL(value, environment, &error);

        if (result == NULL || error.errno != SUCCESS)
        {
            fprintf(stdout, "%s\n", get_error_message(error.errno));
        }
        else
        {
            PRINT(result);
        }
    }
    else
    {
        fprintf(stdout, "%s\n", get_error_message(reader.errno));
    }
}

char *get_history_filename()
{
    char *home_folder = getenv("HOME");
    char *history_file = malloc(strlen(home_folder) + strlen(HISTORY_FILENAME) + 1 + 1);
    sprintf(history_file, "%s/%s", home_folder, HISTORY_FILENAME);

    return history_file;
}

int main(int argc, char **argv)
{
    char *input = NULL;
    char *history_file = get_history_filename();
    _read_history(history_file);

    MalEnvironment *environment = make_initial_environment();

    while (1)
    {
        input = _readline("user> ");

        if (input && *input && *input != 0x0)
        {
            _add_history(input);
            rep(input, environment);
        }
        else
        {
            break;
        }
    }

    _save_history(history_file);
    free(history_file);
}