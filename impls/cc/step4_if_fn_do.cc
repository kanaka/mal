#include "core.hh"
#include "env.hh"
#include "printer.hh"
#include "reader.hh"
#include <iostream>
#include <map>
#include <string>

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, Env *env);

std::shared_ptr<MalType> read(const std::string &input)
{
    return read_str(input);
}

std::shared_ptr<MalType> eval_ast(std::shared_ptr<MalType> ast, Env *env)
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
            auto l = eval(a, env);
            if (!l)
                return nullptr;
            new_list->push_back(l);
        }
        return new_list;
    }
    default:
        return ast;
    }
}

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, Env *env)
{
    if (!input)
        return nullptr;

    if (input->type() != MalType::Type::List)
        return eval_ast(input, env);

    auto &list = static_cast<MalList &>(*input);
    if (list.empty())
        return input;

    if (!list.is_list())
        return eval_ast(input, env);

    if (list[0] && list[0]->type() == MalType::Type::Symbol)
    {
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
            Env new_env(env);
            auto &bindings = static_cast<const MalList &>(*list[1]);
            for (unsigned i = 0; i < bindings.size(); i += 2)
            {
                std::string key = static_cast<const MalSymbol &>(*bindings[i]);
                auto val = eval(bindings[i + 1], &new_env);
                if (!val)
                    return nullptr;
                new_env.set(key, val);
            }
            return eval(list[2], &new_env);
        }

        if (symbol == "do")
        {
            for (unsigned i = 1; i < list.size() - 1; ++i)
                eval(list[i], env);
            return eval(list[list.size() - 1], env);
        }

        if (symbol == "if")
        {
            auto cond = eval(list[1], env);
            if (cond && cond->type() == MalType::Type::Symbol)
            {
                auto condition = static_cast<MalSymbol &>(*cond);
                if (!condition)
                    return list.size() > 3 ? eval(list[3], env) : env->get("nil");
            }
            return eval(list[2], env);
        }

        if (symbol == "fn*")
        {
            auto closure = [list, env](std::vector<std::shared_ptr<MalType>> params)
            {
                // TODO: fix memory leak
                auto new_env = new Env(static_cast<const MalList &>(*list[1]), params, env);
                return eval(list[2], new_env);
            };
            return std::make_shared<MalFunc>(closure);
        }
    }

    auto plist = eval_ast(input, env);
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
    return pr_str(input);
}

std::string rep(const std::string &input, Env *env)
{
    auto read_result = read(input);
    auto eval_result = eval(read_result, env);
    auto print_result = print(eval_result);
    return print_result;
}

int main(int argc, char *argv[])
{
    Env repl_env(nullptr);

    for (auto &[key, value] : ns())
        repl_env.set(key, value);

    rep("(def! not (fn* (a) (if a false true)))", &repl_env);

    while (!std::cin.eof())
    {
        std::string input;
        std::cout << "user> ";
        std::getline(std::cin, input);
        auto rep_result = rep(input, &repl_env);
        std::cout << rep_result << std::endl;
    }

    return 0;
}
