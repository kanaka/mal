#include <string>
#include <iostream>
#include <memory>
#include <map>
#include "types.h"
#include "exceptions.h"


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


MalPtr TokenVector::operator[](unsigned int i)
{
    return tokens[i];
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
        else if (it->get()->type() == "Hash Map")
        {
            s += "{" + it->get()->value() + "}";
        }
        else
        {
            s += it->get()->value();
        }
    }
    return s;
}


std::string TokenVector::types()
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
            s += "List (" + it->get()->raw_value().types() + ")";
        }
        else if (it->get()->type() == "Vector")
        {
            s += "Vector [" + it->get()->raw_value().types() + "]";
        }
        else
        {
            s += it->get()->type();
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


TokenVector MalList::raw_value()
{
    return list;
}


MalVector::MalVector(const TokenVector& v): MalType("{vector}")
{
    vec.append(const_cast<TokenVector&>(v));
}


std::string MalVector::value()
{
    return vec.values();
}

TokenVector MalVector::raw_value()
{
    return vec;
}


MalHashmap::MalHashmap(TokenVector hm): MalType("{hash}")
{
    if (!(hm.size() % 2))
    {
        throw new InvalidHashmapException();
    }

    for (unsigned int i = 0;
         i < hm.size()-1 && (hm[i]->type() == "String" ||  hm[i]->type() == "Keyword");
         i+=2)
    {
        if (hm[i]->type() == "String" || hm[i]->type() == "Keyword")
        {
            hashmap.emplace(hm[i]->value(), hm[i+1]->value());
        }
        else
        {
            throw new InvalidHashmapException();
        }
    }
}


std::string MalHashmap::value()
{
    std::string s = "";

    for(auto it = hashmap.begin(); it != hashmap.end(); ++it)
    {
        if (it != hashmap.begin())
        {
            s += " ";
        }
        s += it->first + " " + it->second.value();
    }

    return s;
}