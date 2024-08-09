#include "printer.hh"
#include "reader.hh"
#include <iostream>
#include <string>

std::shared_ptr<MalType> read(const std::string &input)
{
    return read_str(input);
}

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input)
{
    return input;
}

std::string print(std::shared_ptr<MalType> input)
{
    return pr_str(std::move(input));
}

std::string rep(const std::string &input)
{
    auto read_result = read(input);
    auto eval_result = eval(std::move(read_result));
    auto print_result = print(std::move(eval_result));
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
