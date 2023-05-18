/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <exception>
#include <functional>
#include <iostream>
#include <memory>
#include <new>
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


std::string PRINT(TokenVector input)
{
    pr_str(input);
    return input.values();
}


std::string rep(std::string input)
{
    return PRINT(EVAL(READ(input), repl_env));
}


int main()
{
    init_global_environment();

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
            std::cout << "(EOF|end of input|incomplete complex number)."  << '\n';
        }
        catch(IncompleteEscapeException* e)
        {
            std::cout << "(EOF|end of input|unbalanced)."  << '\n';
        }
        catch(InvalidComplexNumberException* e)
        {
            std::cout << "(invalid complex number): " << e->value() << "." << '\n';
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
        catch(ProcedureNotFoundException* e)
        {
            std::cout << "(procedure not found): " << e->value() << "." << '\n';
        }
        catch(InvalidFunctionArgumentException* e)
        {
            std::cout << "(invalid function argument): " << e->value() << "." << '\n';
        }
        catch(MissingFunctionArgumentException* e)
        {
            std::cout << "(missing function argument)." << '\n';
        }
        catch(SymbolNotInitializedException* e)
        {
            std::cout << "(unbound symbol): " << e->value() << "." << '\n';
        }

        catch(std::exception *e)
        {
            std::cout << e->what() << "." << '\n';
        }
    }
    std::cout << "Exiting.\n";

    return 0;
}