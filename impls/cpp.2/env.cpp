/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

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

Environment global_env;


Env_Symbol::Env_Symbol(MalPtr s, MalPtr v): val(v)
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



TokenVector Env_Primitive::apply(TokenVector& args, Environment env)
{
    TokenVector result;
    size_t effective_arity = abs(arity);

    std::cout << "Arity: " << effective_arity << ", arg types: " << args.types() << std::endl;

    if ((args.size() <= effective_arity + 1) || (arity < 0 && args.size() >= effective_arity))
    {
        return fn(eval_ast(args, env), env);
    }
    else
    {
        throw new ArityMismatchException();
    }
    return args;
}


TokenVector Env_Procedure::apply(TokenVector& args, Environment env)
{
    TokenVector result;
    size_t effective_arity = abs(arity);

    if ((args.size() == effective_arity + 1) || (arity < 0 && args.size() >= effective_arity))
    {
        return apply_fn(fn, args, env);
    }
    else
    {
        throw new ArityMismatchException();
    }
    return args;
}


EnvPtr Environment::find(MalPtr p)
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


std::function<TokenVector(TokenVector, Environment)> mal_plus([](TokenVector tokens, Environment env)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_list;

        if (x_peek->type() == MAL_LIST)
        {
            std::cout << tokens.peek()->value() << std::endl;
            x_list = eval_list(tokens.next()->raw_value(), env);
        }
        else
        {
            x_list.append(tokens.next());
        }

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_list;
            if (y_peek->type() == MAL_LIST)
            {
                y_list = eval_list(tokens.next()->raw_value(), env);
            }
            else
            {
                y_list.append(tokens.next());
            }


            switch (x_peek->type())
            {
                    case MAL_INTEGER:
                    {
                        MalPtr x = x_list.next();
                        MalPtr y = y_list.next();


                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                mpz_class xp = (dynamic_cast<MalInteger*>(&(*x)))->numeric_value();
                                mpz_class yp = (dynamic_cast<MalInteger*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalInteger>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                mpf_class xp = (dynamic_cast<MalInteger*>(&(*x)))->numeric_value();
                                mpf_class yp = (dynamic_cast<MalFractional*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalFractional>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                mpq_class xp = (dynamic_cast<MalInteger*>(&(*x)))->numeric_value();
                                mpq_class yp = (dynamic_cast<MalRational*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalRational>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalInteger*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp = (dynamic_cast<MalComplex*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;

                    case MAL_FRACTIONAL:
                    {
                        MalPtr x = x_list.next();
                        MalPtr y = y_list.next();

                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                mpf_class xp = (dynamic_cast<MalFractional*>(&(*x)))->numeric_value();
                                mpz_class yp = (dynamic_cast<MalInteger*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalFractional>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                mpf_class xp = (dynamic_cast<MalFractional*>(&(*x)))->numeric_value();
                                mpf_class yp = (dynamic_cast<MalFractional*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalFractional>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                mpq_class xp((dynamic_cast<MalFractional*>(&(*x)))->numeric_value());
                                mpq_class yp = (dynamic_cast<MalRational*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalFractional>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalFractional*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp = (dynamic_cast<MalComplex*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    case MAL_RATIONAL:
                    {
                        MalPtr x = x_list.next();
                        MalPtr y = y_list.next();

                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                mpq_class xp = (dynamic_cast<MalRational*>(&(*x)))->numeric_value();
                                mpz_class yp = (dynamic_cast<MalInteger*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalRational>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                mpq_class xp = (dynamic_cast<MalRational*>(&(*x)))->numeric_value();
                                mpf_class yp = (dynamic_cast<MalFractional*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalFractional>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_RATIONAL:
                            {
                                mpq_class xp = (dynamic_cast<MalRational*>(&(*x)))->numeric_value();
                                mpq_class yp = (dynamic_cast<MalRational*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalRational>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalRational*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp = (dynamic_cast<MalComplex*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                case MAL_COMPLEX:
                    {
                        MalPtr x = x_list.next();
                        MalPtr y = y_list.next();

                        switch (y->type())
                        {
                            case MAL_INTEGER:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalComplex*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp((dynamic_cast<MalInteger*>(&(*y)))->numeric_value());
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_FRACTIONAL:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalComplex*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp((dynamic_cast<MalFractional*>(&(*y)))->numeric_value());
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_RATIONAL:
                            {   std::complex<mpf_class> xp((dynamic_cast<MalComplex*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp((dynamic_cast<MalRational*>(&(*y)))->numeric_value());
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            case MAL_COMPLEX:
                            {
                                std::complex<mpf_class> xp((dynamic_cast<MalComplex*>(&(*x)))->numeric_value());
                                std::complex<mpf_class> yp((dynamic_cast<MalComplex*>(&(*y)))->numeric_value());
                                TokenVector result;
                                result.append(std::make_shared<MalComplex>(xp + yp));
                                return result;
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x_peek->value());
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


std::function<TokenVector(TokenVector, Environment)> mal_minus([](TokenVector tokens, Environment env)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
        TokenVector x_list;

        if (x_peek->type() == MAL_LIST)
        {
            x_list = eval_list(tokens.next()->raw_value(), env);
        }
        else
        {
            x_list.append(tokens.next());
        }

        MalPtr y_peek = tokens.peek();
        if (y_peek != nullptr)
        {
            TokenVector y_list;
            if (y_peek->type() == MAL_LIST)
            {
                y_list = eval_list(tokens.next()->raw_value(), env);
            }
            else
            {
                y_list.append(tokens.next());
            }

            switch (x_peek->type())
            {
                    case MAL_INTEGER:
                    {
                        MalPtr x = x_list.next();
                        MalPtr y = y_list.next();

                        switch (y_peek->type())
                        {
                            case MAL_INTEGER:
                            {
                                MalPtr y = tokens.next();
                                mpz_class xp = (dynamic_cast<MalInteger*>(&(*x)))->numeric_value();
                                mpz_class yp = (dynamic_cast<MalInteger*>(&(*y)))->numeric_value();
                                TokenVector result;
                                result.append(std::make_shared<MalInteger>(xp - yp));
                                return result;
                            }
                                break;
                            default:
                                throw new InvalidFunctionArgumentException(y_peek->value());
                        }
                    }
                        break;
                    default:
                        throw new InvalidFunctionArgumentException(x_peek->value());
                }
            }
        else
        {
            throw MissingFunctionArgumentException();
        }
    }
    else
    {
        throw MissingFunctionArgumentException();
    }
});



void init_global_environment()
{
    global_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("+"), mal_plus, 2));
    global_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("-"), mal_minus, 2));
}