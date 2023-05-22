#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "reader.h"
#include "types.h"
#include "token_types.h"
#include "exceptions.h"
#include "parse_numbers.h"
#include "parse_sequences.h"
#include "parse_reader_macros.h"


TokenVector read_form(std::string input_stream);


unsigned int paren_count = 0;
unsigned int square_bracket_count = 0;
unsigned int hm_count = 0;
unsigned int s_index = 0;


TokenVector read_str(std::string s)
{
    paren_count = 0;
    square_bracket_count = 0;
    hm_count = 0;
    s_index = 0;
    TokenVector tokens = tokenize(s);

    if (paren_count > 0)
    {
        throw new UnbalancedParenthesesException();
    }
    if (square_bracket_count > 0)
    {
        throw new UnbalancedVectorException();
    }
    if (hm_count > 0)
    {
        throw new UnbalancedHashmapException();
    }

    return tokens;
}



TokenVector tokenize(std::string input_stream)
{
    TokenVector tokens;
    char ch;

    while (s_index < input_stream.length())
    {
        std::string s = "";

        ch = input_stream[s_index++];

        if (isspace(ch))
        {
            continue;
        }
        else
        {
            switch (ch)
            {
                case ';':
                    read_comment(input_stream);
                    break;
                case '(':
                    read_list(input_stream, tokens);
                    break;
                case ')':
                    close_list();
                    return tokens;
                case '[':
                    read_vector(input_stream, tokens);
                    break;
                case ']':
                    close_vector();
                    return tokens;
                case '{':
                    read_hashmap(input_stream, tokens);
                    return tokens;
                case '}':
                    close_hashmap();

                    if (hm_count == 0)
                    {
                        return tokens;
                    }
                    break;
                case '&':
                    tokens.append(std::make_shared<MalRestArg>());
                    break;
                case '.':
                    tokens.append(std::make_shared<MalPeriod>());
                    break;
                case '~':
                    read_unquote(input_stream, tokens);
                    break;
                case ',':
                    continue;
                    break;
                case '@':
                    read_reader_macro<MalDeref>(input_stream, tokens);
                    break;
                case '\'':
                    read_reader_macro<MalQuote>(input_stream, tokens);
                    break;
                case '`':
                    read_reader_macro<MalQuasiquote>(input_stream, tokens);
                    break;
                case '^':
                    read_meta(input_stream, tokens);
                    break;
                case '\"':
                    read_string(input_stream, tokens);
                    break;
                case '-':
                    if (isdigit(input_stream[s_index]))
                    {
                        read_number(input_stream, ch, tokens);
                    }
                    else
                    {
                        read_symbol(input_stream, ch, tokens);
                    }
                    break;
                default:
                    if (isdigit(ch))
                    {
                        read_number(input_stream, ch, tokens);
                    }
                    else
                    {
                        read_symbol(input_stream, ch, tokens);
                    }
            }
        }
    }

    return tokens;
}



TokenVector read_form(std::string input_stream)
{
    return tokenize(input_stream);
}



void read_whitespace(std::string input_stream, char leading)
{
    char ch = leading;
    if (!isspace(ch)) return;

    while (isspace(ch) && s_index < input_stream.length())
    {
        ch = input_stream[s_index++];
    }
    if (s_index < input_stream.length())
    {
        s_index--;
    }
}


void read_comment(std::string input_stream)
{
    for (char ch = input_stream[s_index]; ch != '\n' && s_index < input_stream.length(); s_index++)
    {
        ch = input_stream[s_index];
    }
}


void read_string(std::string input_stream, TokenVector& tokens)
{
    std::string s = "";
    char ch;

    ch = input_stream[s_index++];

    while ((ch != '\"') && s_index < input_stream.length())
    {
        if ((ch == '\\' ) && s_index < input_stream.length())
        {
            ch = input_stream[s_index++];
            if (s_index == input_stream.length())
            {
                throw new IncompleteEscapeException();
            }

            if (ch == '\"' || ch == '\\')
            {
                s += ch;
            }
            else if (ch == 'n')
            {
                s += '\n';
            }
            else if (ch == 't')
            {
                s += '\t';
            }
            else
            {
                throw new UnbalancedStringException();
            }
        }

        else
        {
            s += ch;
        }
        ch = input_stream[s_index++];
    }

    if (ch != '\"')
    {
        throw new UnbalancedStringException();
    }

    tokens.append(std::make_shared<MalString>(s));
}



void read_symbol(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s = "";

    char ch = leading;

    while ((!isspace(ch) && !is_syntax(ch)) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }
    if (s_index < input_stream.length() || is_right_balanced(ch))
    {
        s_index--;
    }
    else
    {
        s += ch;
    }

    if (s == "nil")
    {
        tokens.append(std::make_shared<MalNil>());
    }
    else if (s == "true" || s == "false")
    {
        tokens.append(std::make_shared<MalBoolean>(s));
    }
    else if (s[0] == ':')
    {
        tokens.append(std::make_shared<MalKeyword>(s));
    }
    else
    {
        tokens.append(std::make_shared<MalSymbol>(s));
    }
}

