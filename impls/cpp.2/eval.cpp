#include <memory>
#include <string>
#include <vector>
#include <map>
#include <gmpxx.h>
#include "exceptions.h"
#include "types.h"
#include "apply.h"
#include "env.h"
#include "eval.h"



TokenVector eval_ast(TokenVector& input, Environment env)
{
    MalTypeName type = input.peek()->type();

    switch (type)
    {
        case MAL_LIST:
            return eval_list(input, env);
            break;
        default:
            if (input.size() <= 1)
            {
                return input;
            }
            else
            {
                throw new TooManyInputsException();
            }
    }
}


TokenVector eval_list(TokenVector& input, Environment env)
{
    MalTypeName type = input.peek()->type();

    if (type == MAL_SYMBOL)
    {
        EnvPtr procedure = env.find(input.next());
        return apply_fn(procedure, input);
    }
    else if(type == MAL_LIST)
    {
        return eval_list(input, env);
    }
    else
    {
        return input;
    }
}