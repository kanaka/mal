/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <string>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <cctype>
#include <gmpxx.h>
#include "types.h"
#include "exceptions.h"



TokenVector::TokenVector(const TokenVector& t)
{
    tokens.reserve(65535);
    current_token = t.current_token;

    for (std::vector<MalPtr>::const_iterator it = t.tokens.cbegin(); it != t.tokens.cend(); ++it)
    {
        this->tokens.push_back(*it);
    }
}


const TokenVector& TokenVector::operator=(const TokenVector& t)
{
    this->clear();
    for (std::vector<MalPtr>::const_iterator it = t.tokens.cbegin(); it != t.tokens.cend(); ++it)
    {
        this->tokens.push_back(*it);
    }

    return t;
}


size_t TokenVector::append(MalPtr token)
{
    if (token == nullptr)
    {
        throw new NullTokenException();
    }
 
    tokens.push_back(token);
    if (this->peek() == nullptr)
    {
        throw new NullTokenException();
    }
 
    return this->size();
}


size_t TokenVector::append(const TokenVector& t)
{
    for (std::vector<MalPtr>::const_iterator it = t.tokens.cbegin(); it != t.tokens.cend(); ++it)
    {
        this->tokens.push_back(*it);
    }

    return tokens.size();
}


MalPtr TokenVector::operator[](unsigned int i)
{
    return tokens[i];
}


std::string TokenVector::values()
{
    if (tokens.size() == 0) return "";

    std::string s = "";

    bool rest = false;
    for (auto elem : tokens)
    {
        if (rest && !(elem->type() == MAL_COMMA))
        {
            s += " ";
        }
        else
        {
            rest = true;
        }
        s += elem->value();
    }

    return s;
}


std::string TokenVector::values_remainder()
{
    std::string s = "";

    for (auto i = current_token; i < tokens.size(); ++i)
    {
        if (i > current_token && !(tokens[i]->type() == MAL_COMMA))
        {
            s += " ";
        }
        s += tokens[i]->value();
    }
    return s;
}



TokenVector TokenVector::cdr()
{
    TokenVector result;

    for (size_t i = 1; i < tokens.size(); ++i)
    {
        result.append(tokens[i]);
    }
    return result;
}



TokenVector TokenVector::rest()
{
    TokenVector result;

    for (size_t i = current_token; i < tokens.size(); ++i)
    {
        result.append(tokens[i]);
    }
    return result;
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
            s += "List(" + it->get()->raw_value().types() + ")";
        }
        else if (it->get()->type() == MAL_VECTOR)
        {
            s += "Vector[" + it->get()->raw_value().types() + "]";
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

// ugly hack that leaks the underlying representation, nasty
MalHashmap::MalHashmap(std::unordered_map<std::string, std::shared_ptr<MalType> > hm): MalType("{hash}")
{
    hashmap = hm;
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
    std::string mantissa = internal_value.get_str(exp);
    repr = std::to_string(exp) + '.' + mantissa;
}


std::string MalFractional::value()
{
    mp_exp_t decimal;
    std::string v = internal_value.get_str(decimal);
    if (internal_value < 0)
    {
        decimal++;
    }

    std::string s = v.substr(0, decimal) + '.' + v.substr(decimal);

    return s;
}



MalComplex::MalComplex(std::complex<mpf_class> c): MalNumber(""), internal_value(c)
{
    mp_exp_t rexp, iexp;
    char imag_sign = (internal_value.imag() < 0) ? '-' : '+';
    std::string real_mantissa = internal_value.real().get_str(rexp);
    std::string imag_mantissa = internal_value.imag().get_str(iexp);
    repr = std::to_string(rexp) + (real_mantissa == "0" ? "" : '.' + real_mantissa)
            + imag_sign + std::to_string(iexp) + (imag_mantissa == "0" ? "" : '.' + imag_mantissa) + 'i';
}



MalComplex::MalComplex(std::string r): MalNumber(r)
{
    std::string real_repr = "";
    std::string imag_repr = "";
    char ch;
    size_t curr = 0;

    // collect the real part, seeking for the plus or minus sign that indicates
    // the start of the imaginary part.

    ch = repr[curr++];

    if (ch == '-')
    {
        real_repr += ch;
        ch = repr[curr++];
    }
    else if (ch == '+')
    {
        ch = repr[curr++];
    }

    bool is_before_decimal = true;

    while ((isdigit(ch) || ch == '.') && curr < repr.length())
    {
        if (ch == '.')
        {
            if (is_before_decimal)
            {
                is_before_decimal = false;
            }
            else
            {
                throw new InvalidComplexNumberException(repr);
            }
        }
        real_repr += ch;
        ch = repr[curr++];
    }

    // if there is no imaginary part, assume imaginary_part == 0
    if (curr == repr.length())
    {
        imag_repr = "+0i";
    }
    else
    {
        if (ch != '+' && ch != '-')
        {
            throw new InvalidComplexNumberException(repr);
        }
        else
        {
            if (ch == '+')
            {
                ch = repr[curr++];
            }
            else if (ch == '-')
            {
                imag_repr += ch;
                ch = repr[curr++];
            }

            is_before_decimal = true;

            while ((isdigit(ch) || ch == '.') && curr < repr.length())
            {
                if (ch == '.')
                {
                    if (is_before_decimal)
                    {
                        is_before_decimal = false;
                    }
                    else
                    {
                        throw new InvalidComplexNumberException(repr);
                    }
                }
                imag_repr += ch;
                ch = repr[curr++];
            }


            if (ch != 'i')
            {
                throw new InvalidComplexNumberException(repr);
            }
        }
    }

    mpf_class real_value(real_repr);
    mpf_class imag_value(imag_repr);

    internal_value = std::complex<mpf_class>(real_value, imag_value);

    mp_exp_t rexp, iexp;
    char imag_sign = (internal_value.imag() < 0) ? '-' : '+';
    std::string real_mantissa = internal_value.real().get_str(rexp);
    std::string imag_mantissa = internal_value.imag().get_str(iexp);
    repr = std::to_string(rexp) + (real_mantissa == "0" ? "" : '.' + real_mantissa)
            + imag_sign + std::to_string(iexp) + (imag_mantissa == "0" ? "" : '.' + imag_mantissa) + 'i';
}


std::string MalComplex::value()
{
    mp_exp_t real_decimal;
    std::string v = internal_value.real().get_str(real_decimal);

    if (internal_value.real() < 0)
    {
        real_decimal++;
    }

    std::string real_mantissa = v.substr(real_decimal);

    if (real_mantissa.length() > 0)
    {
        real_mantissa = '.' + real_mantissa;
    }

    std::string real_repr = v.substr(0, real_decimal) + real_mantissa;

    mp_exp_t imag_decimal;
    v = internal_value.imag().get_str(imag_decimal);
    if (internal_value.imag() < 0)
    {
        imag_decimal++;
    }

    std::string imag_mantissa = v.substr(imag_decimal);

    if (imag_mantissa.length() > 0)
    {
        imag_mantissa = '.' + imag_mantissa;
    }

    std::string imag_repr = v.substr(0, imag_decimal) + imag_mantissa;
    return real_repr + (internal_value.imag() < 0 ? "" : "+") + imag_repr + 'i';
}


TokenVector MalProcedure::raw_value()
{
    TokenVector t;
    t.append(std::make_shared<MalSymbol>(repr));
    return t;
}

TokenVector MalPrimitive::raw_value()
{
    TokenVector t;
    t.append(std::make_shared<MalSymbol>(repr));
    return t;
}