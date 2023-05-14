
/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <memory>
#include <iostream>
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
            {
                TokenVector evlist = input.next()->raw_value();
                return eval_list(evlist, env);
            }
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
        MalPtr proc_ptr = input.next();
        EnvPtr procedure = env.find(proc_ptr);

        if (procedure == nullptr)
        {
            throw new ProcedureNotFoundException(proc_ptr->value());
        }

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