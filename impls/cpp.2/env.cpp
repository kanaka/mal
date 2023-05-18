/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

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

Environment repl_env;


Env_Symbol::Env_Symbol(MalPtr s, MalPtr v): val(v), n_ary(0)
{
    if (s != nullptr && s->type() == MAL_SYMBOL)
    {
        sym = s->value();
    }
    else
    {
        throw new InvalidEnvironmentSymbolException(s->value());
    }
}



TokenVector Env_Primitive::apply(TokenVector& args)
{
    TokenVector result;
    size_t effective_arity = abs(arity());

    if ((args.size() == effective_arity) || (arity() < 0 && args.size() >= effective_arity))
    {
        return fn(args);
    }
    else
    {
        throw new ArityMismatchException();
    }


    return args;
}


TokenVector Env_Procedure::apply(TokenVector& args)
{
    TokenVector result;
    size_t effective_arity = abs(arity());

    if ((args.size() == effective_arity + 1) || (arity() < 0 && args.size() >= effective_arity))
    {
        return apply_fn(fn, args);
    }
    else
    {
        return apply_fn(fn, args);
        // throw new ArityMismatchException();
    }
    return args;
}


bool Environment::find(MalPtr p)
{
    if (p->type() == MAL_SYMBOL)
    {
        for (std::vector<EnvPtr>::iterator it = env.begin(); it != env.end(); ++it)
        {
            if (it->get()->symbol().value() == p->value())
            {
                return true;
            }
        }

        if (parent != nullptr)
        {
            return parent->find(p);
        }
    }

    return false;
}


EnvPtr Environment::get(MalPtr p)
{
    if (p->type() == MAL_SYMBOL)
    {
        for (std::vector<EnvPtr>::iterator it = env.begin(); it != env.end(); ++it)
        {
            if (it->get()->symbol().value() == p->value())
            {
                return *it;
            }
        }

        if (parent != nullptr)
        {
            return parent->get(p);
        }
    }

    return nullptr;
}


void Environment::append(EnvPtr element)
{
    env.push_back(element);
}


std::string Environment::element_names()
{
    std::string result;

    for (std::vector<EnvPtr>::iterator it = env.begin(); it != env.end(); ++it)
    {
        if (it != env.begin())
        {
            result += ", ";
        }
        result += it->get()->symbol().value();
    }

    return result;
}


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


std::function<TokenVector(TokenVector)> mal_plus([](TokenVector tokens)->TokenVector
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



std::function<TokenVector(TokenVector)> mal_minus([](TokenVector tokens)->TokenVector
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



std::function<TokenVector(TokenVector)> mal_multiply([](TokenVector tokens)->TokenVector
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



std::function<TokenVector(TokenVector)> mal_divide([](TokenVector tokens)->TokenVector
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



std::function<TokenVector(TokenVector)> mal_modulo([](TokenVector tokens)->TokenVector
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



void init_global_environment()
{
    repl_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("+"), mal_plus, -2));
    repl_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("-"), mal_minus, -1));
    repl_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("*"), mal_multiply, -2));
    repl_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("/"), mal_divide, 2));
    repl_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("%"), mal_modulo, 2));
}