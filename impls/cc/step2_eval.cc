#include "printer.hh"
#include "reader.hh"
#include <iostream>
#include <map>
#include <numeric>
#include <string>

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, const std::map<std::string, std::function<int(std::vector<int>)>> &env);

std::map<std::string, std::function<int(std::vector<int>)>> repl_env = {
    {"+", [](std::vector<int> args)
     { return std::accumulate(args.begin(), args.end(), 0, std::plus<int>()); }},
    {"-", [](std::vector<int> args)
     { return args[0] - args[1]; }},
    {"*", [](std::vector<int> args)
     { return std::accumulate(args.begin(), args.end(), 1, std::multiplies<int>()); }},
    {"/", [](std::vector<int> args)
     { return args[0] / args[1]; }},
};

std::shared_ptr<MalType> read(const std::string &input)
{
    return read_str(input);
}

std::shared_ptr<MalType> eval_ast(std::shared_ptr<MalType> ast, const std::map<std::string, std::function<int(std::vector<int>)>> &env)
{
    switch (ast->type())
    {
    case MalType::Type::Symbol:
    {
        auto symbol = static_cast<MalSymbol &>(*ast);
        if (symbol.is_string() || symbol.is_keyword())
            return ast;

        auto iter = env.find(symbol);
        if (iter == env.end())
        {
            std::cerr << "Invallid symbol";
            return nullptr;
        }
        return std::make_shared<MalFunc>(iter->second);
    }
    case MalType::Type::List:
    {
        auto &list = static_cast<MalList &>(*ast);
        auto new_list = std::make_shared<MalList>(list.lparen(), list.rparen());
        for (auto &a : list)
        {
            auto l = eval(std::move(a), env);
            if (!l)
                return nullptr;
            new_list->push_back(std::move(l));
        }
        return new_list;
    }
    default:
        return ast;
    }
}

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, const std::map<std::string, std::function<int(std::vector<int>)>> &env)
{
    if (!input)
        return nullptr;

    if (input->type() != MalType::Type::List)
        return eval_ast(std::move(input), env);

    auto &list = static_cast<MalList &>(*input);
    if (list.empty())
        return input;

    if (!list.is_list())
        return eval_ast(std::move(input), env);

    auto plist = eval_ast(std::move(input), env);
    if (!plist)
        return nullptr;

    auto &new_list = static_cast<MalList &>(*plist);
    auto &func = static_cast<const MalFunc &>(*new_list[0]);

    std::vector<int> args;
    for (unsigned i = 1; i < new_list.size(); ++i)
        args.push_back(static_cast<const MalInt &>(*new_list[i]));

    return std::make_shared<MalInt>(func(args));
}

std::string print(std::shared_ptr<MalType> input)
{
    return pr_str(std::move(input));
}

std::string rep(const std::string &input)
{
    auto read_result = read(input);
    auto eval_result = eval(std::move(read_result), repl_env);
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
        auto rep_result = rep(input);
        std::cout << rep_result << std::endl;
    }

    return 0;
}
