
#include <memory>
#include <string>
#include <vector>
#include <map>
#include "types.h"
#include "apply.h"
#include "eval.h"
#include "env.h"


TokenVector apply_fn(EnvPtr fn, TokenVector& args)
{
    if (fn->type() != ENV_PRIMITIVE)
    {
        throw new InvalidPrimitiveException();
    }
    else
    {
        return args;
    }
}


TokenVector apply_fn(TokenVector fn, TokenVector& args)
{
    fn.append(args);
    return fn;
}