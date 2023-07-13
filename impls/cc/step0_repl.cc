#include <iostream>
#include <string>

std::string read(std::string input)
{
    return input;
}

std::string eval(std::string input)
{
    return input;
}

std::string print(std::string input)
{
    return input;
}

std::string rep(std::string input)
{
    auto read_result = read(input);
    auto eval_result = eval(read_result);
    auto print_result = print(eval_result);
    return print_result;
}

int main(int argc, char *argv[])
{
    std::string input;

    while (!std::cin.eof())
    {
        std::cout << "user> ";
        std::getline(std::cin, input);
        try
        {
            auto rep_result = rep(input);
            std::cout << rep_result << std::endl;
        }
        catch (std::exception &e)
        {
            std::cerr << e.what() << std::endl;
        }
    }

    return 0;
}
