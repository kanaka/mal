#include <string>
#include <iostream>
#include <memory>
#include "types.h"


size_t TokenVector::append(MalPtr token)
{
    tokens.push_back(token);

    return this->size();
}


size_t TokenVector::append(TokenVector& t)
{
    for (std::vector<MalPtr>::iterator it = t.tokens.begin(); it != t.tokens.end(); ++it)
    {
        this->tokens.push_back(*it);
    }

    return this->size();
}


std::string TokenVector::values()
{
    std::string s = "";

    for (std::vector<MalPtr>::iterator it = tokens.begin(); it != tokens.end(); ++it)
    {
        if (it != tokens.begin())
        {
            s += " ";
        }
        if (it->get()->type() == "List")
        {
            s += "(" + it->get()->value() + ")";
        }
        else if (it->get()->type() == "Vector")
        {
            s += "[" + it->get()->value() + "]";
        }
        else
        {
            s += it->get()->value();
        }
    }
    return s;
}


MalList::MalList(const TokenVector& l): MalType("{list}")
{
    list.append(const_cast<TokenVector&>(l));
}


std::string MalList::value()
{
    return list.values();
}


MalVector::MalVector(const TokenVector& v): MalType("{vector}")
{
    vec.append(const_cast<TokenVector&>(v));
}


std::string MalVector::value()
{
    return vec.values();
}