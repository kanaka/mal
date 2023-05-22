
#include <iostream>
#include <memory>
#include "reader.h"
#include "types.h"
#include "token_types.h"
#include "exceptions.h"
#include "parse_numbers.h"
#include "parse_sequences.h"
#include "parse_reader_macros.h"


void read_unquote(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    if (ch == '@')
    {
        read_reader_macro<MalSpliceUnquote>(input_stream, tokens);
    }
    else
    {
        s_index--;
        read_reader_macro<MalUnquote>(input_stream, tokens);
    }
}


void read_meta(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    TokenVector seq_argument, main_argument;

    if (is_left_balanced(ch))
    {
        switch(ch)
        {
            case '(':
                read_list(input_stream, seq_argument);
                break;
            case '[':
                read_vector(input_stream, seq_argument);
                break;
            case '{':
                read_hashmap(input_stream, seq_argument);
                break;
            case '\"':
                read_string(input_stream, seq_argument);
                break;
        }

        read_whitespace(input_stream, ch);
        if (s_index >= input_stream.length())
        {
            throw new InvalidMetaException();
        }

        s_index++;
        ch = input_stream[s_index++];

        if (is_left_balanced(ch))
        {
            switch(ch)
            {
                case '(':
                    read_list(input_stream, main_argument);
                    break;
                case '[':
                    read_vector(input_stream, main_argument);
                    break;
                case '{':
                    read_hashmap(input_stream, main_argument);
                    break;
                case '\"':
                    read_string(input_stream, main_argument);
                    break;
            }
        }
        else if (isdigit(ch))
        {
            read_number(input_stream, ch, main_argument);
        }
        else if (!is_syntax(ch))
        {
            read_symbol(input_stream, ch, main_argument);
        }
        else
        {
            throw new InvalidMetaException();
        }
    }
    else
    {
        throw new InvalidMetaException();
    }

    tokens.append(std::make_shared<MalMeta>(main_argument, seq_argument));
}
