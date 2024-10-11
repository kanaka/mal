#include "env.hh"
#include "printer.hh"
#include "reader.hh"
#include <iostream>
#include <map>
#include <numeric>
#include <string>

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, std::shared_ptr<Env> env);

std::shared_ptr<MalType> read(const std::string &input)
{
    return read_str(input);
}

std::shared_ptr<MalType> eval_ast(std::shared_ptr<MalType> ast, std::shared_ptr<Env> env)
{
    switch (ast->type())
    {
    case MalType::Type::Symbol:
    {
        auto symbol = static_cast<MalSymbol &>(*ast);
        if (symbol.is_string() || symbol.is_keyword())
            return ast;

        return env->get(symbol);
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
    case MalType::Type::Map:
    {
        auto &map = static_cast<MalMap &>(*ast);
        auto new_map = std::make_shared<MalMap>();
        for (auto &[key, value] : map)
        {
            auto v = eval(value, env);
            if (!v)
                return nullptr;
            (*new_map)[key] = v;
        }
        return new_map;
    }
    default:
        return ast;
    }
}

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, std::shared_ptr<Env> env)
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

    std::string symbol = static_cast<const MalSymbol &>(*list[0]);
    if (symbol == "def!")
    {
        std::string key = static_cast<const MalSymbol &>(*list[1]);
        auto val = eval(list[2], env);
        if (!val)
            return nullptr;
        env->set(key, val);
        return val;
    }

    if (symbol == "let*")
    {
        auto new_env = std::make_shared<Env>(env);
        auto &bindings = static_cast<const MalList &>(*list[1]);
        for (unsigned i = 0; i < bindings.size(); i += 2)
        {
            std::string key = static_cast<const MalSymbol &>(*bindings[i]);
            auto val = eval(bindings[i + 1], new_env);
            if (!val)
                return nullptr;
            new_env->set(key, val);
        }
        return eval(list[2], new_env);
    }

    auto plist = eval_ast(std::move(input), env);
    if (!plist)
        return nullptr;

    auto &new_list = static_cast<MalList &>(*plist);
    auto &func = static_cast<const MalFunc &>(*new_list[0]);

    std::vector<std::shared_ptr<MalType>> args;
    for (unsigned i = 1; i < new_list.size(); ++i)
        args.push_back(new_list[i]);

    return func(args);
}

std::string print(std::shared_ptr<MalType> input)
{
    return pr_str(std::move(input));
}

std::string rep(const std::string &input, std::shared_ptr<Env> env)
{
    auto read_result = read(input);
    auto eval_result = eval(std::move(read_result), env);
    auto print_result = print(std::move(eval_result));
    return print_result;
}

int main(int argc, char *argv[])
{
    auto add = std::make_shared<MalFunc>([](std::vector<std::shared_ptr<MalType>> args)
                                         { return std::make_shared<MalInt>(std::accumulate(args.begin(), args.end(), 0, [](int acc, std::shared_ptr<MalType> i)
                                                                                           { return acc + static_cast<MalInt &>(*i); })); });
    auto sub = std::make_shared<MalFunc>([](std::vector<std::shared_ptr<MalType>> args)
                                         { return std::make_shared<MalInt>(static_cast<MalInt &>(*args[0]) - static_cast<MalInt &>(*args[1])); });
    auto mul = std::make_shared<MalFunc>([](std::vector<std::shared_ptr<MalType>> args)
                                         { return std::make_shared<MalInt>(std::accumulate(args.begin(), args.end(), 1, [](int acc, std::shared_ptr<MalType> i)
                                                                                           { return acc * static_cast<MalInt &>(*i); })); });
    auto div = std::make_shared<MalFunc>([](std::vector<std::shared_ptr<MalType>> args)
                                         { return std::make_shared<MalInt>(static_cast<MalInt &>(*args[0]) / static_cast<MalInt &>(*args[1])); });

    auto repl_env = std::make_shared<Env>();
    repl_env->set("+", add);
    repl_env->set("-", sub);
    repl_env->set("*", mul);
    repl_env->set("/", div);

    while (!std::cin.eof())
    {
        std::string input;
        std::cout << "user> ";
        std::getline(std::cin, input);
        try
        {
            auto rep_result = rep(input, repl_env);
            std::cout << rep_result << std::endl;
        }
        catch (std::exception &e)
        {
            std::cerr << e.what() << std::endl;
        }
    }

    return 0;
}
