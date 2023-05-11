#include <functional>
#include <typeinfo>
#include <cstdlib>
#include <cstdarg>
#include "exceptions.h"
#include "types.h"
#include "env.h"
#include "apply.h"

Environment global_env;


TokenVector Env_Primitive::apply(TokenVector args)
{
    TokenVector result;
    size_t effective_arity = abs(arity);

    if ((args.size() == effective_arity) || (arity < 0 && args.size() >= effective_arity))
    {
        return fn(args);
    }
    else
    {
        throw new ArityMismatchException();
    }
    return args;
}


TokenVector Env_Procedure::apply(TokenVector args)
{
    TokenVector result;
    size_t effective_arity = abs(arity);

    if ((args.size() == effective_arity) || (arity < 0 && args.size() >= effective_arity))
    {
        return apply_fn(fn, args);
    }
    else
    {
        throw new ArityMismatchException();
    }
    return args;
}


EnvPtr Environment::find(MalSymbol p)
{
    for (std::vector<EnvPtr>::iterator it = env.begin(); it != env.end(); ++it)
    {
        if(it->get()->symbol().value() == p.value())
        {
            return *it;
        }
    }

    return nullptr;
}

