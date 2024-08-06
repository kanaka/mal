/* The following code applies the GNU Readline library, which is
   licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


#include <string>
#include <iostream>
#include "lineedit.h"


std::string READ(std::string input);
std::string EVAL(std::string input);
std::string PRINT(std::string input);
std::string rep(std::string input);

std::string READ(std::string input)
{
    return input;
}

std::string EVAL(std::string input)
{
    return input;
}

std::string PRINT(std::string input)
{
    return input;
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
        std::cout << rep(input) << '\n';
    }
    std::cout << "Exiting.\n";

    return 0;
}