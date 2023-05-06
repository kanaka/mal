#include <string>
#include <iostream>
#include <vector>
#include "lineedit.h"
#include "token.h"
#include "reader.h"


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

        TokenVector tokens(read_str(input, line, 0));
        std::cout << tokens.values() << std::endl;
    }
    std::cout << "Exiting.\n";

    return 0;
}