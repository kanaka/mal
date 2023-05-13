#include <string>
#include <iostream>
#include <memory>
#include <map>
#include <cctype>
#include <gmpxx.h>
#include "types.h"
#include "exceptions.h"


size_t TokenVector::append(MalPtr token)
{
    tokens.push_back(token);

    return this->size();
}


size_t TokenVector::append(const TokenVector& t)
{
    for (std::vector<MalPtr>::const_iterator it = t.tokens.cbegin(); it != t.tokens.cend(); ++it)
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
        if (it != tokens.begin() && !(it->get()->type() == MAL_COMMA))
        {
            s+= " ";
        }
        s += it->get()->value();
    }
    return s;
}


MalPtr TokenVector::peek()
{
    if (current_token >= tokens.size())
    {
        return nullptr;
    }
    else
    {
        return tokens[current_token];
    }
}

MalPtr TokenVector::next()
{
    if (current_token >= tokens.size())
    {
        return nullptr;
    }
    else
    {
        return tokens[current_token++];
    }
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
        if (it->get()->type() == MAL_LIST)
        {
            s += "List (" + it->get()->raw_value().types() + ")";
        }
        else if (it->get()->type() == MAL_VECTOR)
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



MalVector::MalVector(const TokenVector& v): MalType("{vector}")
{
    vec.append(v);
}


MalHashmap::MalHashmap(TokenVector hm): MalType("{hash}")
{
    if (hm.size() > 0)
    {
        if (hm.size() % 2)
        {
            throw new InvalidHashmapException();
        }
        for (unsigned int i = 0; i < hm.size()-1; i+=2)
        {
            if (hm[i]->type() == MAL_STRING || hm[i]->type() == MAL_KEYWORD)
            {
                hashmap.emplace(hm[i]->value(), hm[i+1]);
            }
            else
            {
                throw new InvalidHashmapException();
            }
        }
    }
}


std::string MalHashmap::value()
{
    std::string s = "{";

    for(auto it = hashmap.begin(); it != hashmap.end(); ++it)
    {
        if (it != hashmap.begin())
        {
            s += " ";
        }
        s += it->first + " ";

        if (it->second->type() == MAL_HASHMAP)
        {
            s += it->second->value();
        }
        else
        {
            s += it->second->value();
        }
    }
    s += '}';

    return s;
}


MalFractional::MalFractional(mpf_class f): MalNumber(""), internal_value(f)
{
    mp_exp_t exp;
    repr = f.get_str(exp);
}


MalComplex::MalComplex(std::complex<mpf_class> c): MalNumber(""), internal_value(c)
{
    mp_exp_t rexp, iexp;
    char imag_sign = (internal_value.imag() < 0) ? '-' : '+';
    repr = internal_value.real().get_str(rexp) + imag_sign + internal_value.imag().get_str(iexp) + 'i';
}


MalComplex::MalComplex(std::string r): MalNumber(r)
{
    std::string real_repr = "";
    std::string imag_repr = "";
    char ch;
    size_t curr = 0;

    // collect the real part, seeking for the plus or minus sign that indicates
    // the start of the imaginary part.
    do
    {
        ch = repr[curr++];
        real_repr += ch;
    }
    while ((isdigit(ch) || ch == '.') && curr < repr.length());

    // if there is no imaginary part, assume imaginary_part == 0
    if (curr == repr.length())
    {
        internal_value = std::complex<mpf_class>(mpf_class(real_repr), 0);
    }
    else
    {
        if (ch != '+' && ch != '-')
        {
            throw new InvalidComplexNumberException(repr);
        }
        else
        {
            if (ch == '-')
            {
                imag_repr += ch;
            }
            do
            {
                ch = repr[curr++];
                imag_repr += ch;
            }
            while ((isdigit(ch) || ch == '.') && curr < repr.length());

            if (ch != 'i')
            {
                throw new InvalidComplexNumberException(repr);
            }
        }
    }

    internal_value = std::complex<mpf_class>(mpf_class(real_repr), mpf_class(imag_repr));
}