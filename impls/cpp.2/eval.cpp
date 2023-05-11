#include <memory>
#include <string>
#include <vector>
#include <map>
#include "exceptions.h"
#include "types.h"
#include "eval.h"
#include "env.h"

TokenVector eval_ast(TokenVector input, Environment env)
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


TokenVector eval_list(TokenVector input, Environment env)
{
    MalTypeName type = input.peek()->type();

    switch (type)
    {
        case MAL_LIST:
            return eval_list(input, env);
            break;
        default:
            return input;
    }
}