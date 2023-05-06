#include <string>
#include <iostream>
#include <vector>
#include "lineedit.h"
#include "token.h"
#include "reader.h"
#include "printer.h"


TokenVector READ(std::string input, LineEdit& line)
{
    return read_str(input, line, 0);
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

std::string rep(std::string input, LineEdit& line)
{
    return PRINT(EVAL(READ(input, line)));
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
        rep(input, line);
    }
    std::cout << "Exiting.\n";

    return 0;
}