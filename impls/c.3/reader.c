#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "error.h"
#include "reader.h"
#include "token.h"
#include "types.h"
#include "libs/hashmap/hashmap.h"

MalValue *read_form(Reader *reader, bool readNextToken);

char peek(char *input)
{
    return *(input + 1);
}

int64_t parse_fixnum(char *value, bool negative)
{
    int64_t result = 0;
    unsigned int len = strlen(value);

    for (int i = 0; i < len; i++)
    {
        result = 10 * result + (value[i] - '0');
    }

    return negative ? -result : result;
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
            fill_token(reader->token, TOKEN_STRING, start + 1, (reader->input) - start);
            reader->error->errno = UNBALANCED_STRING;
            reader->error->args = malloc(sizeof(char **[1]));
            reader->error->args[0] = reader->token->value;
        }
        break;
    case '-':
    case '+':
    {
        if (isdigit(peek(reader->input)))
        {
            bool negative = *start == '-';
            reader->input++;

            while (isdigit(*reader->input))
            {
                reader->input++;
            }

            fill_token(reader->token, TOKEN_NUMBER, start + 1, reader->input - (start + 1));
            reader->token->fixnum = parse_fixnum(reader->token->value, negative);
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
        reader->token->fixnum = parse_fixnum(reader->token->value, false);

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
            switch (endToken)
            {
            case TOKEN_RIGHT_BRACKET:
                reader->error->errno = MISSING_CLOSING_BRACKET;
                break;
            case TOKEN_RIGHT_PAREN:
                reader->error->errno = MISSING_CLOSING_PAREN;
                break;
            case TOKEN_RIGHT_BRACE:
                reader->error->errno = MISSING_CLOSING_BRACE;
                break;

            default:
                reader->error->errno = UNEXPECTED_EOF;
                break;
            }

            return NULL;
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
    return read_list_like(reader, new_value(MAL_LIST), TOKEN_RIGHT_PAREN);
}

MalValue *read_vector(Reader *reader)
{
    return read_list_like(reader, new_value(MAL_VECTOR), TOKEN_RIGHT_BRACKET);
}

MalValue *read_atom(Token *token)
{
    MalValue *value = NULL;

    switch (token->tokenType)
    {
    case TOKEN_TILDE:
        value = make_value(MAL_SYMBOL, token->value);
        break;
    case TOKEN_STRING:
        value = make_string(token->value);
        break;
    case TOKEN_SEMI_COLON:
        value = make_value(MAL_COMMENT, token->value);
        break;
    case TOKEN_NUMBER:
        value = make_fixnum(token->fixnum);
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
    MalValue *quote = new_value(MAL_LIST);

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

            reader->error->errno = MISSING_CLOSING_BRACE;

            return NULL;
        }

        key = read_form(reader, false);
        assert(key->valueType == MAL_STRING || key->valueType == MAL_SYMBOL || key->valueType == MAL_KEYWORD);

        value = read_form(reader, true);

        put(map, key->value, value);
    }

    return map;
}

MalValue *read_with_metadata(Reader *reader)
{
    MalValue *metadata = read_form(reader, true);
    assert(metadata->valueType == MAL_HASHMAP);
    MalValue *value = read_form(reader, true);
    setMetadata(value, metadata->hashMap);
    free(metadata);
    MalValue *list = new_value(MAL_LIST);
    push(list, make_value(MAL_SYMBOL, "with-meta"));

    return value;
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
        break;
    case TOKEN_CARET:
        value = read_with_metadata(reader);
        break;
    default:
        value = read_atom(reader->token);
    }

    return value;
}

MalValue *read_str(Reader *reader)
{
    return read_form(reader, true);
}
