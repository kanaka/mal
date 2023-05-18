
/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <memory>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <gmpxx.h>
#include "exceptions.h"
#include "types.h"
#include "apply.h"
#include "env.h"
#include "eval.h"


TokenVector EVAL(TokenVector input, Environment& env)
{

    if (input.empty())
    {
        return input;
    }

    auto type = input.peek()->type();

    if (is_mal_container(type) || type == MAL_SYMBOL)
    {
        TokenVector result = eval_ast(input, env);


        if (result.empty())
        {
            return result;
        }
        else
        {
            if (type == MAL_LIST)
            {
                TokenVector procedure, cdr;

                if (result.peek() == nullptr || result.peek()->raw_value().empty())
                {
                    throw new ProcedureNotFoundException("");
                }
                procedure.append(result.next()->raw_value());
                cdr.append(result.cdr());

                EnvPtr fn = env.get(procedure[0]);
                if (fn == nullptr)
                {
                    throw new ProcedureNotFoundException(procedure[0]->value());
                }

                return apply_fn(fn, cdr);
            }
            else
            {
                return result;
            }
        }
    }
    else
    {
        return input;
    }
}


TokenVector eval_ast(TokenVector input, Environment env)
{
    TokenVector result;
    MalPtr peek = input.peek();

    if (peek == nullptr)
    {
        return result;
    }

    MalTypeName type = peek->type();

    switch (type)
    {
        case MAL_SYMBOL:
            {
                MalPtr symbol = input.next();

                if (symbol == nullptr)
                {
                    throw new SymbolNotInitializedException("");
                }

                EnvPtr p = env.get(symbol);

                if (p == nullptr)
                {
                    throw new SymbolNotInitializedException(symbol->value());
                }

                if (p->type() == ENV_PRIMITIVE)
                {
                    MalPtr prim = std::make_shared<MalPrimitive>(p->symbol().value(), p->arity());
                    result.append(prim);
                }
                else if (p->type() == ENV_PROCEDURE)
                {
                    MalPtr proc = std::make_shared<MalProcedure>(p->value()->value(), p->arity());
                    result.append(proc);
                }
                else if (p->type() == ENV_SYMBOL)
                {
                    result.append(p->value());
                }
                else
                {
                    throw new SymbolNotInitializedException("");
                }
                return result;
            }
            break;
        case MAL_LIST:
            {
                TokenVector sourcelist, evlist;
                sourcelist = input.next()->raw_value();
                for (auto elem = sourcelist.next(); elem != nullptr; elem = sourcelist.next())
                {
                    TokenVector temp;
                    temp.append(elem);
                    evlist.append(EVAL(temp, env));
                    temp.clear();
                }

                return evlist;
            }
            break;
        case MAL_VECTOR:
            {
                TokenVector veclist = input.next()->raw_value();
                return eval_vec(veclist, env);
            }
            break;
        case MAL_HASHMAP:
            {
                // WARNING: This function uses downcasting of a pointer from it's parent class to the
                // actual subclass. This is VERY questionable, and if possible a better solution should be found!
                HashMapInternal hm((dynamic_cast<MalHashmap*>(&(*input.next())))->internal_map());
                return eval_hashmap(hm, env);
            }
            break;
        case MAL_QUASIQUOTE:
            return eval_quasiquoted(input.next()->raw_value(), env);
            break;
        default:
            return input;
    }
}

/* 
TokenVector eval_list(TokenVector& input, Environment env)
{
    MalTypeName type = input.peek()->type();

    if (type == MAL_SYMBOL)
    {
        MalPtr proc_ptr = input.next();
        EnvPtr procedure = env.find(proc_ptr);

        if (procedure == nullptr)
        {
            throw new ProcedureNotFoundException(proc_ptr->value());
        }

        if (procedure->type() == ENV_PRIMITIVE || procedure->type() == ENV_PROCEDURE)
        {
            return apply_fn(procedure, input, env);
        }
        else
        {
            throw new ApplyingNonFunctionException(procedure->value()->value());
        }
    }
    else if(type == MAL_LIST)
    {
        return apply_fn(eval_ast(input.next()->raw_value(), env), input, env);
    }
    else
    {
        return input;
    }
} */

TokenVector eval_vec(TokenVector input, Environment env)
{
    TokenVector temp, elements;
    for (MalPtr elem = input.next(); elem != nullptr; elem = input.next())
    {
        temp.append(elem);
        elements.append(EVAL(temp, env));
        temp.clear();
    }

    TokenVector result;
    MalPtr vec = std::make_shared<MalVector>(elements);
    result.append(vec);
    return result;
}

TokenVector eval_hashmap(HashMapInternal input, Environment env)
{
    HashMapInternal resultant;

    for (auto element : input)
    {
        TokenVector temp;
        temp.append(element.second);
        resultant.emplace(element.first, EVAL(temp, env).next());
        temp.clear();
    }

    TokenVector result;
    MalPtr new_hm = std::make_shared<MalHashmap>(resultant);
    result.append(new_hm);
    return result;
}


TokenVector eval_quasiquoted(TokenVector input, Environment env, bool islist)
{
    TokenVector elements, result;

    for (MalPtr elem = input.next(); elem != nullptr; elem = input.next())
    {
        if (elem->type() == MAL_LIST)
        {
            elements.append(eval_quasiquoted(elem->raw_value(), env, true));
        }

        else if(elem->type() == MAL_UNQUOTE)
        {
            elements.append(eval_ast(elem->raw_value(), env));
        }
        else
        {
            elements.append(elem);
        }
    }
    if (islist)
    {
        result.append(std::make_shared<MalList>(elements));
    }
    else
    {
        result.append(std::make_shared<MalQuasiquote>(elements));
    }
    return result;
}
