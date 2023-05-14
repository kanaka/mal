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



TokenVector Env_Primitive::apply(TokenVector& args)
{
    TokenVector result;
    size_t effective_arity = abs(arity);

    if ((args.size() == effective_arity + 1) || (arity < 0 && args.size() >= effective_arity))
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
    size_t effective_arity = abs(arity);

    if ((args.size() == effective_arity + 1) || (arity < 0 && args.size() >= effective_arity))
    {
        return apply_fn(fn, args);
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


std::function<TokenVector(TokenVector&)> mal_plus([](TokenVector& tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
       switch (x_peek->type())
       {
            case MAL_INTEGER:
            {
                MalPtr x = tokens.next();
                MalPtr y_peek = tokens.peek();

                if (y_peek != nullptr)
                {
                    switch (y_peek->type())
                    {
                        case MAL_INTEGER:
                        {
                            MalPtr y = tokens.next();
                            mpz_class xp = (dynamic_cast<MalInteger*>(&(*x)))->numeric_value();
                            mpz_class yp = (dynamic_cast<MalInteger*>(&(*y)))->numeric_value();
                            TokenVector result;
                            result.append(std::make_shared<MalInteger>(xp + yp));
                            return result;
                        }
                            break;
                        default:
                            throw InvalidFunctionArgumentException();
                    }
                }
                else
                {
                    throw InvalidFunctionArgumentException();
                }
            }
                break;
            default:
                throw InvalidFunctionArgumentException();
        }
    }
    else
    {
        throw InvalidFunctionArgumentException();
    }
});


std::function<TokenVector(TokenVector&)> mal_minus([](TokenVector& tokens)->TokenVector
{
    MalPtr x_peek = tokens.peek();

    if (x_peek != nullptr)
    {
       switch (x_peek->type())
       {
            case MAL_INTEGER:
            {
                MalPtr x = tokens.next();
                MalPtr y_peek = tokens.peek();

                if (y_peek != nullptr)
                {
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
                            throw InvalidFunctionArgumentException();
                    }
                }
                else
                {
                    throw InvalidFunctionArgumentException();
                }
            }
                break;
            default:
                throw InvalidFunctionArgumentException();
        }
    }
    else
    {
        throw InvalidFunctionArgumentException();
    }
});


void init_global_environment()
{
    global_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("+"), mal_plus, 2));
    global_env.append(std::make_shared<Env_Primitive>(std::make_shared<MalSymbol>("-"), mal_minus, 2));
}