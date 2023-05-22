
#ifndef PARSE_READER_MACROS_H
#define PARSE_READER_MACROS_H

#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "reader.h"
#include "types.h"


template<class RM> void read_reader_macro(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    TokenVector rm_argument;

    if (is_left_balanced(ch))
    {
        switch(ch)
        {
            case '(':
                read_list(input_stream, rm_argument);
                break;
            case '[':
                read_vector(input_stream, rm_argument);
                break;
            case '{':
                read_hashmap(input_stream, rm_argument);
                break;
            case '\"':
                read_string(input_stream, rm_argument);
                break;
        }
    }
    else if (isdigit(ch))
    {
        read_number(input_stream, ch, rm_argument);
    }
    else
    {
        read_symbol(input_stream, ch, rm_argument);
    }

    tokens.append(std::make_shared<RM>(rm_argument));
}



void read_unquote(std::string input_stream, TokenVector& tokens);
void read_meta(std::string input_stream, TokenVector& tokens);

#endif