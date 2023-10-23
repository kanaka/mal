#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "env.h"
#include "libs/readline/readline.h"
#include "printer.h"
#include "reader.h"
#include "token.h"
#include "types.h"

static const char *HISTORY_FILENAME = ".mal_history";
FILE *output_stream;

MalValue *READ(Reader *reader)
{
    return read_str(reader);
}

MalValue *EVAL(MalValue *value, MalEnvironment *environment);

MalValue *eval_ast(MalValue *value, MalEnvironment *environment)
{
    MalValue *result = NULL;

    switch (value->valueType)
    {
    case MAL_SYMBOL:
    {
        MalValue *symbol = lookup_in_environment(environment, value);

        if (symbol)
        {
            result = symbol;
        }
        else
        {
            result = make_error("'%s' not found", value->value);
        }
    }
    break;

    case MAL_LIST:
    case MAL_VECTOR:
    {
        MalCell *cdr = value->list->cdr;
        MalValue *tmp = EVAL(value->list->value, environment);

        if (tmp == NULL)
        {
            return NULL;
        }

        MalValue *list = new_value(value->valueType);
        push(list, tmp);

        while (cdr != NULL)
        {
            tmp = EVAL(cdr->value, environment);

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
            r = EVAL(it.value, environment);

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

MalValue *def_exclamation_mark(MalCell *head, MalEnvironment *environment)
{
    MalValue *t = EVAL(head->cdr->cdr->value, environment);

    // !t means symbol not found and should already be recorded in struct error
    if (t && set_in_environment(environment, head->cdr->value, t))
    {
        // FIXME: Report to repl that a value has been redefined.
        //        register_error(VALUE_REDEFINED, head->cdr->value->value);
    }

    return t;
}

/**
 *  create a new environment using the current environment as the outer value
 *  and then use the first parameter as a list of new bindings in the "let*" environment.
 *  Take the second element of the binding list, call EVAL using the new "let*" environment
 *  as the evaluation environment, then call set on the "let*" environment using the first
 *  binding list element as the key and the evaluated second element as the value. This is
 *  repeated for each odd/even pair in the binding list. Note in particular, the bindings
 * earlier in the list can be referred to by later bindings. Finally, the second parameter
 * (third element) of the original let* form is evaluated using the new "let*" environment
 *  and the result is returned as the result of the let* (the new let environment is discarded
 *  upon completion).
 */
MalValue *let_star(MalCell *head, MalEnvironment *environment)
{
    if (!head->cdr || !head->cdr->value || (head->cdr->value->valueType != MAL_LIST && head->cdr->value->valueType != MAL_VECTOR))
    {
        return make_error("expected a list of bindings for '(%s)', got: '%s'", head->value->value, print_values_readably(head->cdr, environment)->value);
    }

    MalEnvironment *nested_environment = make_environment(environment, NULL, NULL, NULL);

    MalCell *current = head->cdr->value->list;
    MalValue *value = NULL;
    MalValue *symbol = NULL;

    while (current && current->cdr)
    {
        symbol = current->value;
        value = EVAL(current->cdr->value, nested_environment);

        // FIXME: EVAL should'nt return NULL but MAL_ERROR so we can handle and bubble up the error
        if (!value)
        {
            free_environment(nested_environment);
            return NULL;
        }

        set_in_environment(nested_environment, symbol, value);
        current = current->cdr->cdr;
    }

    MalValue *result = EVAL(head->cdr->cdr->value, nested_environment);
    free_environment(nested_environment);

    return result;
}

MalValue *do_(MalCell *head, MalEnvironment *environment)
{
    MalCell *current = head;
    MalValue *result = NULL;

    while (current)
    {
        result = EVAL(current->value, environment);
        current = current->cdr;
    }

    return result;
}

MalValue *if_(MalCell *head, MalEnvironment *environment)
{
    MalValue *condition = EVAL(head->value, environment);

    if (condition != &MAL_NIL && condition != &MAL_FALSE)
    {
        return EVAL(head->cdr->value, environment);
    }

    if (head->cdr->cdr && head->cdr->cdr->value)
    {
        return EVAL(head->cdr->cdr->value, environment);
    }

    return &MAL_NIL;
}

MalValue *fn_star(MalCell *context, MalEnvironment *environment)
{
    return make_closure(environment, context);
}

MalValue *EVAL(MalValue *value, MalEnvironment *environment)
{
    MalValue *result = NULL;

    switch (value->valueType)
    {
    case MAL_LIST:
    case MAL_VECTOR:
        MalCell *head = value->list;

        // ast is an empty list: return ast unchanged.
        if (head == NULL)
        {
            return value;
        }

        if (value->valueType == MAL_LIST)
        {
            if (head->value->valueType == MAL_SYMBOL)
            {
                // FIXME: Prüfen, ob alle special forms dieselbe signatur haben können --> ggf. in Map registrieren...
                if (strcmp("def!", head->value->value) == 0)
                {
                    result = def_exclamation_mark(head, environment);
                    break;
                }
                else if (strcmp("let*", head->value->value) == 0)
                {
                    result = let_star(head, environment);
                    break;
                }
                else if (strcmp("do", head->value->value) == 0)
                {
                    result = do_(head->cdr, environment);
                    break;
                }
                else if (strcmp("if", head->value->value) == 0)
                {
                    result = if_(head->cdr, environment);
                    break;
                }
                else if (strcmp("fn*", head->value->value) == 0)
                {
                    result = fn_star(head->cdr, environment);
                    break;
                }
            }
        }

        // ast is a list: call eval_ast to get a new evaluated list.
        // Take the first item of the evaluated list and call it as
        // function using the rest of the evaluated list as its arguments.
        MalValue *tmp = eval_ast(value, environment);

        if (!tmp)
        {
            result = tmp;
            break;
        }

        switch (tmp->valueType)
        {
        case MAL_LIST:
            head = tmp->list;

            switch (head->value->valueType)
            {
            case MAL_FUNCTION:
                result = head->value->function(head->cdr, environment);
                break;

            case MAL_CLOSURE:
            {
                MalClosure *closure = head->value->closure;
                MalEnvironment *env = make_environment(closure->environment, closure->bindings->list, head->cdr, closure->rest_symbol);
                result = EVAL(closure->ast, env);
            }
            break;

            default:
                result = EVAL(tmp, environment);
                break;
            }

            break;

        default:
            result = tmp;
            break;
        }

        break;

    default:
        // ast is not a list: then return the result of calling eval_ast on it.
        result = eval_ast(value, environment);
        break;
    }

    return result;
}

void PRINT(MalValue *value)
{
    print(output_stream, value, true);
    fprintf(output_stream, "\n");
}

void rep(char *input, MalEnvironment *environment)
{
    Reader reader = {.input = input};
    Token token = {};
    reader.token = &token;

    MalValue *value = READ(&reader);

    if (value->valueType != MAL_ERROR)
    {
        MalValue *result = EVAL(value, environment);

        PRINT(result);
    }
    else
    {
        PRINT(value);
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
    output_stream = stdout;
    char *history_file = get_history_filename();
    _read_history(history_file);

    MalEnvironment *environment = make_initial_environment();
    rep("(def! not (fn* (a) (if a false true)))", environment);

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