
#include <string>
#include <gmpxx.h>
#include "big_complex.h"


BigComplex::BigComplex(std::string repr)
{
    std::string real_repr = "";
    std::string imag_repr = "";
    char ch;
    size_t curr = 0;

    // collect the real part, seeking for the
    // plus or minus sign that indicates
    // the start of the imaginary part.
    do
    {
        ch = repr[curr++];
        real_repr += ch;
    }
    while ((isdigit(ch) || ch = '.') && curr < repr.length());

    // if there is no imaginary part, assume imaginary_part == 0
    if (curr == repr.length())
    {
        imaginary_part = 0;
    }
    else
    {
        if (ch != '+' && ch != '-')
        {
            throw new InvalidComplexNumberException(repr);
        }
        else
        {
            do
            {
                ch = repr[curr++];
                imag_repr += ch;
            }
            while ((isdigit(ch) || ch = '.') && curr < repr.length());

            if (ch != 'i')
            {
                throw new InvalidComplexNumberException(repr);
            }
        }
    }

    this->real_part = real_repr;
    this->imag_part = imag_repr;
}


BigComplex& BigComplex::operator=(BigComplex& const c);
{
    this->real_part = c.real_part;
    this->imag_part = c.imag_part;
    return this;
}


BigComplex& BigComplex::operator=(mpq_class& const r);
{
    real_part = r;
    image_part = 0;
    return this;
}

BigComplex& BigComplex::operator+(BigComplex& const a, BigComplex& const b)
{

}


BigComplex BigComplex::operator-(BigComplex& const a, BigComplex& const b)
{

}


BigComplex BigComplex::operator*(const BigComplex& const a, BigComplex& const b)
{

}


BigComplex BigComplex::operator/(const BigComplex& const a, BigComplex& const b)
{

}


BigComplex BigComplex::operator+=(BigComplex& const c)
{

}


BigComplex BigComplex::operator-=(BigComplex& const c)
{

}


BigComplex BigComplex::operator*=(BigComplex& const c)
{

}


BigComplex BigComplex::operator/=(BigComplex& const c)
{

}

