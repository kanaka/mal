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
            return eval_list(input.next()->raw_value(), env);
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
        if (procedure->type() == ENV_PRIMITIVE || procedure->type() == ENV_PROCEDURE)
        {
            return apply_fn(procedure, input);
        }
        else
        {
            throw new ApplyingNonFunctionException(procedure->value()->value());
        }
    }
    else if(type == MAL_LIST)
    {
        return apply_fn(eval_list(input.next()->raw_value(), env), input);
    }
    else
    {
        return input;
    }
}