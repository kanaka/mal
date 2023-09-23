#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "token.h"
#include "types.h"
#include "libs/hashmap/hashmap.h"
typedef struct Reader
{
    char *input;
    Token *token;
} Reader;

MalValue MAL_NIL = {
    .valueType = MAL_SYMBOL,
    .value = "nil"};

MalValue MAL_TRUE = {
    .valueType = MAL_SYMBOL,
    .value = "true"};

MalValue MAL_EOF = {
    .valueType = MAL_SYMBOL,
    .value = "EOF"};

MalValue *read_form(Reader *reader, bool readNextToken);

char peek(char *input)
{
    return *(input + 1);
}

void fill_token(Token *token, enum TokenType tokenType, char *value, unsigned int len)
{
    token->tokenType = tokenType;

    if (value != NULL)
    {
        token->value = malloc(len + 1);
        token->value[len] = '\0';
        strncpy(token->value, value, len);
    }
    else
    {
        token->value = NULL;
    }
}

enum TokenType next_token(Reader *reader)
{
    while (isspace(*reader->input) || ',' == *reader->input)
    {
        reader->input++;
    }

    char *start = reader->input;
    char ch;

    switch (*start)
    {
    case '\0':
        fill_token(reader->token, TOKEN_EOF, start, 1);
        break;
    case '~':
        if ('@' == peek(start))
        {
            reader->input++;
            fill_token(reader->token, TOKEN_TILDE_AT, start, 2);
        }
        else
        {
            fill_token(reader->token, TOKEN_TILDE, NULL, 1);
        }
        reader->input++;
        break;
    case '[':
        fill_token(reader->token, TOKEN_LEFT_BRACKET, NULL, 1);
        reader->input++;
        break;
    case ']':
        fill_token(reader->token, TOKEN_RIGHT_BRACKET, NULL, 1);
        reader->input++;
        break;
    case '(':
        fill_token(reader->token, TOKEN_LEFT_PAREN, NULL, 1);
        reader->input++;
        break;
    case ')':
        fill_token(reader->token, TOKEN_RIGHT_PAREN, NULL, 1);
        reader->input++;
        break;
    case '{':
        fill_token(reader->token, TOKEN_LEFT_BRACE, NULL, 1);
        reader->input++;
        break;
    case '}':
        fill_token(reader->token, TOKEN_RIGHT_BRACE, NULL, 1);
        reader->input++;
        break;
    case '\'':
        fill_token(reader->token, TOKEN_SINGLE_QUOTE, NULL, 1);
        reader->input++;
        break;
    case '`':
        fill_token(reader->token, TOKEN_BACK_TICK, NULL, 1);
        reader->input++;
        break;
    case '^':
        fill_token(reader->token, TOKEN_CARET, start, 1);
        reader->input++;
        break;
    case '@':
        fill_token(reader->token, TOKEN_AT, NULL, 1);
        reader->input++;
        break;
    case '"':
        reader->input++;

        while ((ch = *reader->input) && '\0' != ch)
        {
            if ('\\' == ch)
            {
                char next = peek(reader->input);

                if ('"' == next || '\\' == next)
                {
                    reader->input++;
                }
            }
            else if ('"' == ch)
            {
                break;
            }

            reader->input++;
        }

        if ('"' == ch)
        {
            // do not include leading and trailing '"'
            fill_token(reader->token, TOKEN_STRING, start + 1, (reader->input - 1) - start);
            reader->input++;
        }
        else
        {
            fill_token(reader->token, TOKEN_UNBALANCED_STRING, start + 1, (reader->input) - start);
        }
        break;
    case '-':
    case '+':
    {
        if (isdigit(peek(reader->input)))
        {
            reader->input++;

            while (isdigit(*reader->input))
            {
                reader->input++;
            }

            fill_token(reader->token, TOKEN_NUMBER, start, reader->input - start);
        }
        else
        {
            while ((ch = peek(reader->input)) && ch != '\0' && !isspace(ch))
            {
                reader->input++;
            }
            reader->input++;
            fill_token(reader->token, TOKEN_SYMBOL, start, reader->input - start);
        }
        break;
    }
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        while (isdigit(*reader->input))
        {
            reader->input++;
        }

        fill_token(reader->token, TOKEN_NUMBER, start, reader->input - start);
        break;
    case ';':
        while ((ch = *++reader->input) && ch != '\0' && ch != '\n')
            ;
        fill_token(reader->token, TOKEN_COMMENT, start, reader->input - start);
        break;
    default:
        bool finish = false;

        while ((ch = peek(reader->input)) && ch != '\0' && !isspace(ch) && !finish)
        {
            switch (ch)
            {
            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
            case '\'':
            case '`':
            case '"':
            case ',':
            case ';':
                finish = true;
                break;
            default:
                reader->input++;
                break;
            }
        }

        reader->input++;
        fill_token(reader->token, TOKEN_SYMBOL, start, reader->input - start);
        break;
    }

    return reader->token->tokenType;
}

MalValue *read_list_like(Reader *reader, MalValue *list_like, enum TokenType endToken)
{
    MalValue *value = NULL;
    enum TokenType tokenType;

    while ((tokenType = next_token(reader)) != endToken)
    {
        if (tokenType == TOKEN_EOF)
        {
            return &MAL_EOF;
        }

        value = read_form(reader, false);

        if (value != NULL)
        {
            push(list_like, value);
        }
    }

    return list_like;
}

MalValue *read_list(Reader *reader)
{
    return read_list_like(reader, make_list(), TOKEN_RIGHT_PAREN);
}

MalValue *read_vector(Reader *reader)
{
    return read_list_like(reader, make_vector(), TOKEN_RIGHT_BRACKET);
}

MalValue *read_atom(Token *token)
{
    MalValue *value = NULL;

    switch (token->tokenType)
    {
    case TOKEN_TILDE:
        value = make_value(MAL_SYMBOL, token->value);
        break;
    case TOKEN_UNBALANCED_STRING:
        // FIXME: choose better error handling strategy
        value = &MAL_EOF;
        break;
    case TOKEN_STRING:
        value = make_string(token->value);
        break;
    case TOKEN_SEMI_COLON:
        value = make_value(MAL_COMMENT, token->value);
        break;
    case TOKEN_NUMBER:
        value = make_value(MAL_NUMBER, token->value);
        break;
    case TOKEN_EOF:
        break;
    case TOKEN_SYMBOL:
    default:
        if (':' == *token->value)
        {
            value = make_value(MAL_KEYWORD, token->value);
        }
        else
        {
            value = make_value(MAL_SYMBOL, token->value);
        }
        break;
    }

    return value;
}

MalValue *read_reader_macro(Reader *reader, char *symbol)
{
    MalValue *quote = make_list();

    push(quote, make_value(MAL_SYMBOL, symbol));
    push(quote, read_form(reader, true));

    return quote;
}

MalValue *read_hash_map(Reader *reader)
{
    MalValue *map = new_value(MAL_HASHMAP);
    map->hashMap = make_hashmap();
    MalValue *key = NULL;
    MalValue *value = NULL;
    enum TokenType tokenType;

    while ((tokenType = next_token(reader)) != TOKEN_RIGHT_BRACE)
    {
        if (tokenType == TOKEN_EOF)
        {
            free_hashmap(map->hashMap);
            free(map);

            return &MAL_EOF;
        }

        key = read_form(reader, false);
        assert(key->valueType == MAL_STRING || key->valueType == MAL_SYMBOL || key->valueType == MAL_KEYWORD);

        value = read_form(reader, true);

        put(map, key->value, value);
    }

    return map;
}

MalValue *read_form(Reader *reader, bool readNextToken)
{
    if (readNextToken)
    {
        next_token(reader);
    }

    MalValue *value = NULL;

    switch (reader->token->tokenType)
    {
    case TOKEN_LEFT_PAREN:
        value = read_list(reader);
        break;
    case TOKEN_LEFT_BRACKET:
        value = read_vector(reader);
        break;
    case TOKEN_BACK_TICK:
        value = read_reader_macro(reader, "quasiquote");
        break;
    case TOKEN_SINGLE_QUOTE:
        value = read_reader_macro(reader, "quote");
        break;
    case TOKEN_TILDE:
        value = read_reader_macro(reader, "unquote");
        break;
    case TOKEN_TILDE_AT:
        value = read_reader_macro(reader, "splice-unquote");
        break;
    case TOKEN_AT:
        value = read_reader_macro(reader, "deref");
        break;
    case TOKEN_LEFT_BRACE:
        value = read_hash_map(reader);
        break;
    case TOKEN_RIGHT_PAREN:
    case TOKEN_RIGHT_BRACKET:
    case TOKEN_RIGHT_BRACE:
    case TOKEN_KOMMA:
    case TOKEN_SEMI_COLON:
    case TOKEN_CARET:
        break;
    default:
        value = read_atom(reader->token);
    }

    return value;
}

MalValue *read_str(char *input)
{
    Reader *reader = malloc(sizeof(Reader));
    reader->input = input;
    reader->token = malloc(sizeof(Token));
    MalValue *value = read_form(reader, true);
    reader->input = NULL;
    free(reader);

    return value;
}
