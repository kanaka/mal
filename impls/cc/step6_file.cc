#include "core.hh"
#include "env.hh"
#include "printer.hh"
#include "reader.hh"
#include <iostream>
#include <map>
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

std::shared_ptr<MalType> eval(std::shared_ptr<MalType> input, std::shared_ptr<Env> env)
{
    while (true)
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

        if (list[0]->type() == MalType::Type::Symbol)
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
                input = list[2];
                env = new_env;
                continue;
            }

            if (symbol == "do")
            {
                for (unsigned i = 1; i < list.size() - 1; ++i)
                    eval(list[i], env);
                input = list[list.size() - 1];
                continue;
            }

            if (symbol == "if")
            {
                auto cond = eval(list[1], env);
                if (cond && cond->type() == MalType::Type::Symbol)
                {
                    auto condition = static_cast<MalSymbol &>(*cond);
                    if (!condition)
                    {
                        if (list.size() > 3)
                        {
                            input = list[3];
                            continue;
                        }
                        else
                            return env->get("nil");
                    }
                }
                input = list[2];
                continue;
            }

            if (symbol == "fn*")
            {
                auto closure = [list, env](std::vector<std::shared_ptr<MalType>> params)
                {
                    auto new_env = std::make_shared<Env>(static_cast<const MalList &>(*list[1]), params, env);
                    return eval(list[2], new_env);
                };
                return std::make_shared<MalFunc>(closure, list[2], list[1], env);
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

        if (func.is_fn())
        {
            input = func.ast();
            env = std::make_shared<Env>(static_cast<const MalList &>(*func.params()), args, func.env());
            continue;
        }

        return func(args);
    }
}

std::string print(std::shared_ptr<MalType> input)
{
    return pr_str(input);
}

std::string rep(const std::string &input, std::shared_ptr<Env> env)
{
    auto read_result = read(input);
    auto eval_result = eval(read_result, env);
    auto print_result = print(eval_result);
    return print_result;
}

int main(int argc, char *argv[])
{
    auto repl_env = std::make_shared<Env>(nullptr);

    for (auto &[key, value] : ns())
        repl_env->set(key, value);

    auto closure = [repl_env](std::vector<std::shared_ptr<MalType>> ast)
    {
        return eval(ast[0], repl_env);
    };
    repl_env->set("eval", std::make_shared<MalFunc>(closure));

    rep("(def! not (fn* (a) (if a false true)))", repl_env);
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env);

    auto args = std::make_shared<MalList>('(', ')');
    for (int i = 2; i < argc; ++i)
        args->push_back(read('"' + std::string(argv[i]) + '"'));
    repl_env->set("*ARGV*", args);

    if (argc > 1)
    {
        rep("(load-file \"" + std::string(argv[1]) + "\")", repl_env);
        return 0;
    }

    while (!std::cin.eof())
    {
        std::string input;
        std::cout << "user> ";
        std::getline(std::cin, input);
        auto rep_result = rep(input, repl_env);
        std::cout << rep_result << std::endl;
    }

    return 0;
}
