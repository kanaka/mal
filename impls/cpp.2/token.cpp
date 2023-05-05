#include <memory>
#include <iostream>
#include <vector>

#include "token.h"


std::vector<std::string> token_type_names = {
    "LPAREN", "RPAREN", "PERIOD",
    "COMMA", "AT", "QUOTE", "QUASIQUOTE",
    "SYMBOL", "STRING", "CHAR",
    "INTEGER", "DECIMAL", "RATIONAL", "COMPLEX",
    "HEX", "OCTAL", "BINARY"
};


std::ostream& operator<<(std::ostream &os, Token const & token)
{
    os << "Type: " << token_type_names[token.type] << ", Value: " << token.token << '\n';

    return os;
}


std::ostream& operator<<(std::ostream &os, std::unique_ptr<Token> const & token)
{
    os << *token;

    return os;
}


std::ostream& operator<<(std::ostream &os, std::vector<std::unique_ptr<Token> > & tokens)
{
    for (std::vector<std::unique_ptr<Token> >::iterator it = tokens.begin();
         it != tokens.end();
         ++it)
    {
        os << **it;
    }

    return os;
}