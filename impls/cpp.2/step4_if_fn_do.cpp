/* The following code applies the GNU Readline library and the GNU GMP library,
   which are licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <exception>
#include <functional>
#include <iostream>
#include <fstream>
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

void init_prelude();

TokenVector READ(std::string input)
{
    return read_str(input);
}


std::string PRINT(TokenVector input)
{
    return pr_str(input, true);
}


std::string rep(std::string input)
{
    return PRINT(EVAL(READ(input), repl_env));
}


int main()
{
    init_global_environment();
    init_prelude();

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
            std::cout << rep(input) << '\n';
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
            std::cout << "'" << e->value() << "' not found" << "\n";
        }
        catch(InvalidDefineException* e)
        {
            std::cout << "(invalid function syntax): " << e->value() << "." << '\n';
        }
        catch(InvalidLetException* e)
        {
            std::cout << "(invalid let syntax): " << e->value() << "." << '\n';
        }
        catch(InvalidEnvironmentSymbolException* e)
        {
            std::cout << "(invalid environment symbol): " << e->value() << "." << '\n';
        }
        catch(UnequalBindExprListsException* e)
        {
            std::cout << "(unequal number of parameters and arguments): " << e->value() << "." << '\n';
        }
        catch(InvalidBindExprListsException* e)
        {
            std::cout << "(parameters and/or arguments not lists or vectors): " << e->value() << "." << '\n';
        }
        catch(NonNumericComparisonException* e)
        {
            std::cout << "(non-numeric comparison): " << e->value() << "." << '\n';
        }
        catch(NullTokenException* e)
        {
            std::cout << "(null token)." << '\n';
        }
        catch(std::exception *e)
        {
            std::cout << e->what() << "." << '\n';
        }
    }
    std::cout << "Exiting.\n";

    return 0;
}


void init_prelude()
{
    char buff[65536];
    std::ifstream prelude("prelude.mal", std::ios::in);

    while (!prelude.eof())
    {
        prelude.getline(buff, 65535);
        std::string procedure = buff;
        EVAL(READ(procedure), repl_env);
    }
}