#include <string>
#include <iostream>
#include <vector>
#include <cstdlib>
#include "lineedit.h"
#include "token.h"
#include "reader.h"
#include "printer.h"
#include "exceptions.h"

TokenVector READ(std::string input)
{
    return read_str(input);
}


TokenVector EVAL(TokenVector input)
{
    return input;
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
            std::cout << "(EOF|end of input|unbalanced vector)." << '\n';
        }
        catch(UnbalancedStringException* e)
        {
            std::cout << "(EOF|end of input|unbalanced string)." << '\n';
        }
        catch(IncompleteComplexNumberException* e)
        {
            std::cout << "(incomplete complex)." << '\n';
        }
    }
    std::cout << "Exiting.\n";

    return 0;
}