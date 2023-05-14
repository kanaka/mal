/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "printer.h"
#include "exceptions.h"
#include "eval.h"
#include "env.h"

TokenVector READ(std::string input)
{
    return read_str(input);
}


TokenVector EVAL(TokenVector input)
{
    TokenVector result;
    while((input.peek()) != nullptr)
    {
        MalPtr curr = input.next();
        if (curr->type() == MAL_LIST)
        {
            auto v = curr->raw_value();
            result.append(eval_ast(v, global_env));
        }
        else
        {
            result.append(curr);
        }
    }

    return result;
}


std::string PRINT(TokenVector input)
{
    pr_str(input);
    return input.values();
}


std::string rep(std::string input)
{
    return PRINT(EVAL(READ(input)));
}


int main()
{
    LineEdit line;

    while (true)
    {
        std::string input;
        try
        {
            input = line.getline("user> ");
        }
        catch(EndOfInputException* e)
        {
            break;
        }

        try
        {
            rep(input);
        }
        catch(UnbalancedParenthesesException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)." << '\n';
        }
        catch(UnbalancedVectorException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)." << '\n';
        }
        catch(UnbalancedStringException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)." << '\n';
        }
        catch(UnbalancedHashmapException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)."  << '\n';
        }
        catch(IncompleteComplexNumberException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)."  << '\n';
        }
        catch(IncompleteEscapeException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)."  << '\n';
        }
        catch(InvalidNumberException* e)
        {
            std::cout << "(invalid number): " << e->value() << "." << '\n';
        }
        catch(InvalidHashmapException* e)
        {
            std::cout << "(invalid hash map)." << '\n';
        }
        catch(InvalidMetaException* e)
        {
            std::cout << "(invalid meta expression)." << '\n';
        }
        catch(TooManyInputsException* e)
        {
            std::cout << "(too many elements in the REPL)." << '\n';
        }
        catch(ArityMismatchException* e)
        {
            std::cout << "(arity mismatch in function application)." << '\n';
        }
    }
    std::cout << "Exiting.\n";

    return 0;
}