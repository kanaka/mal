#include <complex>
#include <functional>
#include <iostream>
#include <typeinfo>
#include <cstdlib>
#include <cstdarg>
#include <gmpxx.h>
#include "exceptions.h"
#include "types.h"
#include "env.h"
#include "apply.h"
#include "printer.h"


template <class T> std::function<T(T, T)> apply_plus([](T x, T y)->T
{
    return x + y;
});


template <class T> std::function<T(T, T)> apply_minus([](T x, T y)->T
{
    return x - y;
});


template <class T> std::function<T(T, T)> apply_multiply([](T x, T y)->T
{
    return x * y;
});


template <class T> std::function<T(T, T)> apply_divide([](T x, T y)->T
{
    return x / y;
});


template <class T> std::function<T(T, T)> apply_modulo([](T x, T y)->T
{
    return x % y;
});

/* apply_arith_form - utility class that encapsulated part of the repetitive code
used in the arithmetic operation primitives.

WARNING: This function uses downcasting of a pointer from it's parent class to the
actual subclass. This is VERY questionable, which is partly why this code is isolated
into a separate template function.
*/
template <class MX, class X, class MY, class Y, class Z, class RET> TokenVector apply_arith_form(MalPtr x, MalPtr y, std::function<Z(Z, Z)> op)
{
    TokenVector result;

    // WARNING: this code down-casts the parameters x and y to their
    // respective types, then immeditately discards the resulting pointers.
    // Take care in modifiying this code, if at all!
    Z xp((dynamic_cast<MX*>(&(*x)))->numeric_value());
    Z yp((dynamic_cast<MY*>(&(*y)))->numeric_value());
    result.append(std::make_shared<RET>(op(xp, yp)));
    return result;
}


Procedure mal_plus([](TokenVector tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_tokens;

        x_tokens.append(tokens.next());


        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_tokens;
            y_tokens.append(tokens.next());

            MalPtr x = x_tokens.next();
            MalPtr y = y_tokens.next();

            TokenVector result;

            switch (x->type())
            {
                    case MAL_INTEGER:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalInteger, mpz_class, mpz_class, MalInteger>(x, y, apply_plus<mpz_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_plus<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_plus<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;

                    case MAL_FRACTIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalInteger, mpz_class, mpf_class, MalFractional>(x, y, apply_plus<mpf_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_plus<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalRational, mpf_class, mpf_class, MalFractional>(x, y, apply_plus<mpf_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    case MAL_RATIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalInteger, mpz_class, mpq_class, MalRational>(x, y, apply_plus<mpq_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalFractional, mpf_class, mpq_class, MalFractional>(x, y, apply_plus<mpq_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_plus<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                case MAL_COMPLEX:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalInteger, mpz_class, std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalFractional, mpf_class,  std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalRational, mpq_class, std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalComplex,std::complex<mpf_class>,  std::complex<mpf_class>, MalComplex>(x, y, apply_plus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x->value());
            }
        }
        else
        {
            throw new MissingFunctionArgumentException();
        }
    }
    else
    {
        throw new MissingFunctionArgumentException();
    }
});



Procedure mal_minus([](TokenVector tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_tokens;

        x_tokens.append(tokens.next());

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_tokens;
            y_tokens.append(tokens.next());


            MalPtr x = x_tokens.next();
            MalPtr y = y_tokens.next();

            TokenVector result;

            switch (x->type())
            {
                    case MAL_INTEGER:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalInteger, mpz_class, mpz_class, MalInteger>(x, y, apply_minus<mpz_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_minus<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_minus<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;

                    case MAL_FRACTIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalInteger, mpz_class, mpf_class, MalFractional>(x, y, apply_minus<mpf_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_minus<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalRational, mpf_class, mpf_class, MalFractional>(x, y, apply_minus<mpf_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    case MAL_RATIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalInteger, mpz_class, mpq_class, MalRational>(x, y, apply_minus<mpq_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalFractional, mpf_class, mpq_class, MalFractional>(x, y, apply_minus<mpq_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_minus<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                case MAL_COMPLEX:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalInteger, mpz_class, std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalFractional, mpf_class,  std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalRational, mpq_class, std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalComplex,std::complex<mpf_class>,  std::complex<mpf_class>, MalComplex>(x, y, apply_minus<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x->value());
            }
        }
        else
        {
            throw new MissingFunctionArgumentException();
        }
    }
    else
    {
        throw new MissingFunctionArgumentException();
    }
});



Procedure mal_multiply([](TokenVector tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_tokens;

        x_tokens.append(tokens.next());

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_tokens;
            y_tokens.append(tokens.next());

            MalPtr x = x_tokens.next();
            MalPtr y = y_tokens.next();

            TokenVector result;

            switch (x->type())
            {
                    case MAL_INTEGER:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalInteger, mpz_class, mpz_class, MalInteger>(x, y, apply_multiply<mpz_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_multiply<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_multiply<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;

                    case MAL_FRACTIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalInteger, mpz_class, mpf_class, MalFractional>(x, y, apply_multiply<mpf_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_multiply<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalRational, mpf_class, mpf_class, MalFractional>(x, y, apply_multiply<mpf_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    case MAL_RATIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalInteger, mpz_class, mpq_class, MalRational>(x, y, apply_multiply<mpq_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalFractional, mpf_class, mpq_class, MalFractional>(x, y, apply_multiply<mpq_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_multiply<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                case MAL_COMPLEX:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalInteger, mpz_class, std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalFractional, mpf_class,  std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalRational, mpq_class, std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalComplex,std::complex<mpf_class>,  std::complex<mpf_class>, MalComplex>(x, y, apply_multiply<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x->value());
            }
        }
        else
        {
            throw new MissingFunctionArgumentException();
        }
    }
    else
    {
        throw new MissingFunctionArgumentException();
    }
});



Procedure mal_divide([](TokenVector tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_tokens;

        x_tokens.append(tokens.next());

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_tokens;
            y_tokens.append(tokens.next());


            MalPtr x = x_tokens.next();
            MalPtr y = y_tokens.next();

            TokenVector result;

            switch (x->type())
            {
                    case MAL_INTEGER:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalInteger, mpz_class, mpz_class, MalInteger>(x, y, apply_divide<mpz_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_divide<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_divide<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;

                    case MAL_FRACTIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalInteger, mpz_class, mpf_class, MalFractional>(x, y, apply_divide<mpf_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalFractional, mpf_class, mpf_class, MalFractional>(x, y, apply_divide<mpf_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalRational, mpf_class, mpf_class, MalFractional>(x, y, apply_divide<mpf_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalFractional, mpf_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    case MAL_RATIONAL:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalInteger, mpz_class, mpq_class, MalRational>(x, y, apply_divide<mpq_class>);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalFractional, mpf_class, mpq_class, MalFractional>(x, y, apply_divide<mpq_class>);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalRational, mpq_class, mpq_class, MalRational>(x, y, apply_divide<mpq_class>);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalRational, mpq_class, MalComplex, std::complex<mpf_class>, std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                case MAL_COMPLEX:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalInteger, mpz_class, std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalFractional, mpf_class,  std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalRational, mpq_class, std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                return apply_arith_form<MalComplex, std::complex<mpf_class>, MalComplex,std::complex<mpf_class>,  std::complex<mpf_class>, MalComplex>(x, y, apply_divide<std::complex<mpf_class> >);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x->value());
            }
        }
        else
        {
            throw new MissingFunctionArgumentException();
        }
    }
    else
    {
        throw new MissingFunctionArgumentException();
    }
});



Procedure mal_modulo([](TokenVector tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_tokens;

        x_tokens.append(tokens.next());

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_tokens;
            y_tokens.append(tokens.next());


            MalPtr x = x_tokens.next();
            MalPtr y = y_tokens.next();

            TokenVector result;

            switch (x->type())
            {
                    case MAL_INTEGER:
                    {
                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                return apply_arith_form<MalInteger, mpz_class, MalInteger, mpz_class, mpz_class, MalInteger>(x, y, apply_modulo<mpz_class>);
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x->value());
            }
        }
        else
        {
            throw new MissingFunctionArgumentException();
        }
    }
    else
    {
        throw new MissingFunctionArgumentException();
    }
});


// comparisons

Procedure mal_equal([](TokenVector tokens)->TokenVector
{
    TokenVector car;
    car.append(tokens.car());
    TokenVector cdr;
    cdr.append(tokens.cdr());

    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    // first, check that the two elements to compare are of the same size
    // this handles the case where there are dangling elements
    if (car.size() != cdr.size())
    {
        return mal_false;
    }

    if (car.peek()->type() != cdr.peek()->type())
    {
        if (!(is_mal_container(car.peek()->type()) && is_mal_container(cdr.peek()->type())))
        {
            return mal_false;
        }
    }

    if (is_mal_numeric(car.peek()->type()))
    {
        switch (car.peek()->type())
        {
            case MAL_INTEGER:
                {
                    auto comp1 = dynamic_cast<MalInteger*>(&(*car.next()))->numeric_value();
                    auto comp2 = dynamic_cast<MalInteger*>(&(*cdr.next()))->numeric_value();
                    if (comp1 == comp2)
                    {
                        return mal_true;
                    }
                    else
                    {
                        return mal_false;
                    }
                }
                break;
            case MAL_RATIONAL:
                {
                    auto comp1 = dynamic_cast<MalRational*>(&(*car.next()))->numeric_value();
                    auto comp2 = dynamic_cast<MalRational*>(&(*cdr.next()))->numeric_value();
                    if (comp1 == comp2)
                    {
                        return mal_true;
                    }
                    else
                    {
                        return mal_false;
                    }
                }
                break;
            case MAL_FRACTIONAL:
                {
                    auto comp1 = dynamic_cast<MalFractional*>(&(*car.next()))->numeric_value();
                    auto comp2 = dynamic_cast<MalFractional*>(&(*cdr.next()))->numeric_value();
                    if (comp1 == comp2)
                    {
                        return mal_true;
                    }
                    else
                    {
                        return mal_false;
                    }
                }
                break;
            case MAL_COMPLEX:
                {
                    auto comp1 = dynamic_cast<MalComplex*>(&(*car.next()))->numeric_value();
                    auto comp2 = dynamic_cast<MalComplex*>(&(*cdr.next()))->numeric_value();
                    if (comp1 == comp2)
                    {
                        return mal_true;
                    }
                    else
                    {
                        return mal_false;
                    }
                }
                break;
            default:
                throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
        }
    }
    else if (is_mal_container(car.peek()->type()) || is_mal_reader_macro(car.peek()->type()))
    {
        auto car_list = car.peek()->raw_value();
        auto cdr_list = cdr.peek()->raw_value();

        if (car_list.size() != cdr_list.size())
        {
            return mal_false;
        }

        for (size_t i = 0; i < cdr_list.size(); ++i)
        {
            TokenVector comp;
            comp.append(car_list.next());
            comp.append(cdr_list.next());

            if (mal_equal(comp).peek()->value() == "false")
            {
                return mal_false;
            }
            comp.clear();
        }
        return mal_true;
    }

    else if (car.next()->value() != cdr.next()->value())
    {
        return mal_false;
    }
    else
    {
        return mal_true;
    }
});

Procedure mal_not_equal([](TokenVector tokens)->TokenVector
{
    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (mal_equal(tokens).next()->value() != "true")
    {
        return mal_false;
    }
    else
    {
        return mal_true;
    }

});


Procedure mal_greater_than([](TokenVector tokens)->TokenVector
{
    TokenVector car, cdr;
    car.append(tokens.car());
    cdr.append(tokens.cdr());

    if (!is_mal_numeric(car.peek()->type()))
    {
        throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }


    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (car.peek()->type() != cdr.peek()->type())
    {
        return mal_false;
    }


    switch (car.peek()->type())
    {
        case MAL_INTEGER:
            {
                auto comp1 = dynamic_cast<MalInteger*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalInteger*>(&(*cdr.next()))->numeric_value();
                if (comp1 > comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_RATIONAL:
            {
                auto comp1 = dynamic_cast<MalRational*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalRational*>(&(*cdr.next()))->numeric_value();
                if (comp1 > comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_FRACTIONAL:
            {
                auto comp1 = dynamic_cast<MalFractional*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalFractional*>(&(*cdr.next()))->numeric_value();
                if (comp1 > comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;

        default:
            throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }
});

Procedure mal_less_than([](TokenVector tokens)->TokenVector
{
    TokenVector car, cdr;
    car.append(tokens.car());
    cdr.append(tokens.cdr());

    if (!is_mal_numeric(car.peek()->type()))
    {
        throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }


    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (car.peek()->type() != cdr.peek()->type())
    {
        return mal_false;
    }


    switch (car.peek()->type())
    {
        case MAL_INTEGER:
            {
                auto comp1 = dynamic_cast<MalInteger*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalInteger*>(&(*cdr.next()))->numeric_value();
                if (comp1 < comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_RATIONAL:
            {
                auto comp1 = dynamic_cast<MalRational*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalRational*>(&(*cdr.next()))->numeric_value();
                if (comp1 < comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_FRACTIONAL:
            {
                auto comp1 = dynamic_cast<MalFractional*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalFractional*>(&(*cdr.next()))->numeric_value();
                if (comp1 < comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;

        default:
            throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }
});


Procedure mal_greater_equal([](TokenVector tokens)->TokenVector
{
    TokenVector car, cdr;
    car.append(tokens.car());
    cdr.append(tokens.cdr());

    if (!is_mal_numeric(car.peek()->type()))
    {
        throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }


    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (car.peek()->type() != cdr.peek()->type())
    {
        return mal_false;
    }


    switch (car.peek()->type())
    {
        case MAL_INTEGER:
            {
                auto comp1 = dynamic_cast<MalInteger*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalInteger*>(&(*cdr.next()))->numeric_value();
                if (comp1 >= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_RATIONAL:
            {
                auto comp1 = dynamic_cast<MalRational*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalRational*>(&(*cdr.next()))->numeric_value();
                if (comp1 >= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_FRACTIONAL:
            {
                auto comp1 = dynamic_cast<MalFractional*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalFractional*>(&(*cdr.next()))->numeric_value();
                if (comp1 >= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;

        default:
            throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }
});

Procedure mal_less_equal([](TokenVector tokens)->TokenVector
{
    TokenVector car, cdr;
    car.append(tokens.car());
    cdr.append(tokens.cdr());

    if (!is_mal_numeric(car.peek()->type()))
    {
        throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }


    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (car.peek()->type() != cdr.peek()->type())
    {
        return mal_false;
    }


    switch (car.peek()->type())
    {
        case MAL_INTEGER:
            {
                auto comp1 = dynamic_cast<MalInteger*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalInteger*>(&(*cdr.next()))->numeric_value();
                if (comp1 <= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_RATIONAL:
            {
                auto comp1 = dynamic_cast<MalRational*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalRational*>(&(*cdr.next()))->numeric_value();
                if (comp1 <= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        case MAL_FRACTIONAL:
            {
                auto comp1 = dynamic_cast<MalFractional*>(&(*car.next()))->numeric_value();
                auto comp2 = dynamic_cast<MalFractional*>(&(*cdr.next()))->numeric_value();
                if (comp1 <= comp2)
                {
                    return mal_true;
                }
                else
                {
                    return mal_false;
                }
            }
            break;
        default:
            throw new NonNumericComparisonException(car.next()->value(), cdr.next()->value());
    }
});


// list operators

Procedure mal_list([](TokenVector tokens)->TokenVector
{
    auto list =  std::make_shared<MalList>(tokens);
    TokenVector result;
    result.append(list);
    return result;
});

Procedure mal_count([](TokenVector tokens)->TokenVector
{
    TokenVector result;
    if (tokens.peek()->type() == MAL_LIST || tokens.peek()->type() == MAL_VECTOR)
    {
        auto size = tokens.next()->raw_value().size();
        auto count = std::make_shared<MalInteger>(size);
        result.append(count);
    }
    else
    {
        auto count = std::make_shared<MalInteger>(0);
        result.append(count);
    }

    return result;
});


Procedure mal_cons([](TokenVector tokens)->TokenVector
{
    TokenVector temp, result;

    temp.append(tokens.next());
    auto type = tokens.peek()->type();
    if (type == MAL_LIST)
    {
        auto elements = tokens.next()->raw_value();
        for (auto element = elements.next(); element != nullptr; element = elements.next())
        {
            temp.append(element);
        }
        result.append(std::make_shared<MalList>(temp));
    }
    else if (type == MAL_VECTOR)
    {
        auto elements = tokens.next()->raw_value();
        for (auto element = elements.next(); element != nullptr; element = elements.next())
        {
            temp.append(element);
        }
        result.append(std::make_shared<MalVector>(temp));
    }
    else
    {
        throw new InvalidConsPairException(tokens.peek()->value());
    }

    return result;
});

Procedure mal_car([](TokenVector tokens)->TokenVector
{
    auto type = tokens.peek()->type();
    if (is_mal_container(type))
    {
        TokenVector result;
        result.append(tokens.next()->raw_value().car());
        return result;
    }
    else
    {
        throw new InvalidConsPairException(tokens.peek()->value());
    }
});


Procedure mal_cdr([](TokenVector tokens)->TokenVector
{
    auto type = tokens.peek()->type();
    if (is_mal_container(type))
    {
        TokenVector result, collector;
        auto elements = tokens.next()->raw_value();
        elements.next();                // discard car value
        for (auto element = elements.next(); element != nullptr; element = elements.next())
        {
            collector.append(element);
        }
        result.append(std::make_shared<MalList>(collector));
        return result;
    }
    else
    {
        throw new InvalidConsPairException(tokens.peek()->value());
    }
});

Procedure mal_is_empty([](TokenVector tokens)->TokenVector
{
    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (is_mal_container(tokens.peek()->type()))
    {
        if (tokens.peek()->type() == MAL_HASHMAP)
        {
            // WARNING: This function uses downcasting of a pointer from it's parent class to the
            // actual subclass. This is VERY questionable, and if possible a better solution should be found!
            HashMapInternal hm((dynamic_cast<MalHashmap*>(&(*tokens.next())))->internal_map());
            if (hm.size() == 0)
            {
                return mal_true;
            }
        }
        else
        {
            if (tokens.next()->raw_value().size() == 0)
            {
                return mal_true;
            }
        }
    }
    return mal_false;
});


// type predicates

Procedure mal_is_list([](TokenVector tokens)->TokenVector
{
    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (tokens.peek()->type() == MAL_LIST || tokens.peek()->type() == MAL_NULL)
    {
        return mal_true;
    }
    else
    {
        return mal_false;
    }
});


Procedure mal_is_number([](TokenVector tokens)->TokenVector
{
    TokenVector mal_true, mal_false;
    mal_true.append(std::make_shared<MalBoolean>("true"));
    mal_false.append(std::make_shared<MalBoolean>("false"));

    if (is_mal_numeric(tokens.next()->type()))
    {
        return mal_true;
    }
    else
    {
        return mal_false;
    }
});


// printing

std::string filter_escapes(std::string source, bool complete = false)
{
    std::string s = "";

    if (source.empty())
    {
        return s;
    }

    for (size_t i = 0; i < source.size(); ++i)
    {
        char ch = source[i];

        if (ch == '\\')
        {
            if (complete)
            {
                i++;
                ch = source[i];
                if (ch == '\"' || ch == '\\' || ch == '\'')
                {
                    s += ch;
                }
                else if (ch == 'n')
                {
                    s += '\n';
                }
                else
                {
                    throw new UnbalancedStringException();
                }
            }
            else
            {
                i++;
                ch = source[i];
                if (ch == '\"' || ch == '\\' || ch == '\'')
                {
                    s += '\\';
                    s += ch;
                }
                else if (ch == 'n')
                {
                    s += '\\';
                    s += 'n';
                }
                else
                {
                    throw new UnbalancedStringException();
                }
            }
        }
        else if (ch != '\"')
        {
            s += ch;
        }
    }

    return s;
}







Procedure mal_str([](TokenVector tokens)->TokenVector
{
    std::string s = "";

    if (tokens.size() > 0)
    {
        s += pr_str(tokens, false);
    }
    TokenVector str;
    str.append(std::make_shared<MalString>(s));
    return str;
});



Procedure mal_pr_str([](TokenVector tokens)->TokenVector
{
    std::string s = "";

    if (tokens.size() > 0)
    {
        for (auto token = tokens.next(); token != nullptr; token = tokens.next())
        {
            TokenVector t;
            t.append(token);
            s += pr_str(t, true);
            if (tokens.peek() != nullptr)
            {
                s += " ";
            }
        }
    }

    TokenVector str;

    str.append(std::make_shared<MalString>(s));
    return str;
});


Procedure mal_prn([](TokenVector tokens)->TokenVector
{
    if (tokens.size() > 0)
    {
        std::cout << mal_pr_str(tokens).values();
    }

    std::cout << '\n';

    TokenVector mal_nil;
    mal_nil.append(std::make_shared<MalNil>());
    return mal_nil;
});


Procedure mal_println([](TokenVector tokens)->TokenVector
{
    if (tokens.values().length() != 0)
    {
        for (size_t i = 0; i < tokens.size(); ++i)
        {
            TokenVector temp;
            temp.append(tokens[i]);
            auto str = mal_str(temp);
            std::string s = str.values();
            std::cout << s;
            if (i < tokens.size()-1)
            {
                std::cout << " ";
            }
            temp.clear();
        }
    }
    std::cout << '\n';

    TokenVector mal_nil;
    mal_nil.append(std::make_shared<MalNil>());
    return mal_nil;
});



void init_global_environment()
{
    // basic arithmetic
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("+"), mal_plus, -2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("-"), mal_minus, -1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("*"), mal_multiply, -2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("/"), mal_divide, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("%"), mal_modulo, 2));

    // comparisons
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("="), mal_equal, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>(">"), mal_greater_than, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("<"), mal_less_than, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>(">="), mal_greater_equal, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("<="), mal_less_equal, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("!="), mal_not_equal, 2));

    // list manipulation
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("list"), mal_list, -1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("count"), mal_count, 1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("cons"), mal_cons, 2));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("car"), mal_car, 1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("cdr"), mal_cdr, 1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("empty?"), mal_is_empty, 1));

    // type predicates
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("list?"), mal_is_list, 1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("number?"), mal_is_number, 1));

    // printing values
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("prn"), mal_prn, -1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("str"), mal_str, -1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("pr-str"), mal_pr_str, -1));
    repl_env.set(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("println"), mal_println, -1));
}