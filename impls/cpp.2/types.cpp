#include <string>
#include <memory>
#include "types.h"

size_t TokenVector::append(MalPtr token)
{
    tokens.push_back(std::move(token));

    return this->size();
}

size_t TokenVector::append(TokenVector& t)
{
    for (std::vector<MalPtr>::iterator it = t.tokens.begin(); it != t.tokens.end(); ++it)
    {
        this->tokens.push_back(std::move(*it));
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
        s += it->get()->value();
    }
    return s;
}


MalList::MalList(TokenVector& l): MalType("")
{
    list.append(l);
}

std::string MalList::value()
{
    std::string s = "(" + list.values() + ")";
    return s;
}