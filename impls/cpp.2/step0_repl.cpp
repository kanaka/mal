#include <iostream>
#include <string>
#include <vector>

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
    std::string prompt = "user> ";
    std::string input;

    while (true)
    {
        std::cout << prompt;
        if(!std::getline(std::cin, input))
            break;
        std::cout << rep(input) << std::endl;
    }

    return 0;
}