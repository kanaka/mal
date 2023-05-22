#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "reader.h"
#include "types.h"
#include "token_types.h"
#include "exceptions.h"
#include "parse_sequences.h"


void read_list(std::string input_stream, TokenVector& tokens)
{
    paren_count++;
    TokenVector ml = tokenize(input_stream);

    if (ml.values() == "")
    {
        tokens.append(std::make_shared<MalNull>());
    }
    else
    {
        tokens.append(std::make_shared<MalList>(ml));
    }
}


void close_list()
{
    if (paren_count > 0)
    {
        paren_count--;
    }
    else
    {
        throw new UnbalancedParenthesesException();
    }
}


void read_vector(std::string input_stream, TokenVector& tokens)
{
    square_bracket_count++;

    tokens.append(std::make_shared<MalVector>(tokenize(input_stream)));
}


void close_vector()
{
    if (square_bracket_count > 0)
    {
        square_bracket_count--;
    }
    else
    {
        throw new UnbalancedVectorException();
    }
}


void read_hashmap(std::string input_stream, TokenVector& tokens)
{
    hm_count++;

    tokens.append(std::make_shared<MalHashmap>(tokenize(input_stream)));
}


void close_hashmap()
{
    if (hm_count > 0)
    {
        hm_count--;
    }
    else
    {
        throw new UnbalancedHashmapException();
    }
}