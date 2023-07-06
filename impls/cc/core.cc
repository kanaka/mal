#include "core.hh"
#include "printer.hh"
#include "reader.hh"
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>

MalSymbol true_("true");
MalSymbol false_("false");
MalSymbol nil_("nil");

std::shared_ptr<MalType> add(std::vector<std::shared_ptr<MalType>> args)
{
    int result = std::accumulate(args.begin(), args.end(), 0, [](int acc, std::shared_ptr<MalType> i)
                                 { return acc + static_cast<MalInt &>(*i); });
    return std::make_shared<MalInt>(result);
}

std::shared_ptr<MalType> sub(std::vector<std::shared_ptr<MalType>> args)
{
    int result = static_cast<MalInt &>(*args[0]) - static_cast<MalInt &>(*args[1]);
    return std::make_shared<MalInt>(result);
}

std::shared_ptr<MalType> mul(std::vector<std::shared_ptr<MalType>> args)
{
    int result = std::accumulate(args.begin(), args.end(), 1, [](int acc, std::shared_ptr<MalType> i)
                                 { return acc * static_cast<MalInt &>(*i); });
    return std::make_shared<MalInt>(result);
}

std::shared_ptr<MalType> divide(std::vector<std::shared_ptr<MalType>> args)
{
    int result = static_cast<MalInt &>(*args[0]) / static_cast<MalInt &>(*args[1]);
    return std::make_shared<MalInt>(result);
}

std::shared_ptr<MalType> prstr(std::vector<std::shared_ptr<MalType>> args)
{
    std::ostringstream oss;

    oss << '\"';
    if (!args.empty())
    {
        for (auto arg : args)
            oss << pr_str(arg, true) << ' ';
        oss.seekp(-1, oss.end);
    }
    oss << '\"';

    return std::make_shared<MalSymbol>(oss.str());
}

std::shared_ptr<MalType> str(std::vector<std::shared_ptr<MalType>> args)
{
    std::ostringstream oss;

    oss << '\"';
    for (auto arg : args)
        oss << pr_str(arg, false);
    oss << '\"';

    return std::make_shared<MalSymbol>(oss.str());
}

std::shared_ptr<MalType> prn(std::vector<std::shared_ptr<MalType>> args)
{
    std::ostringstream oss;

    for (auto arg : args)
        oss << pr_str(arg, true) << ' ';

    std::cout << (args.empty() ? "" : oss.str().substr(0, oss.str().length() - 1)) << std::endl;

    return std::make_shared<MalSymbol>(nil_);
}

std::shared_ptr<MalType> println(std::vector<std::shared_ptr<MalType>> args)
{
    std::ostringstream oss;

    for (auto arg : args)
        oss << pr_str(arg, false) << ' ';

    std::cout << (args.empty() ? "" : oss.str().substr(0, oss.str().length() - 1)) << std::endl;

    return std::make_shared<MalSymbol>(nil_);
}

std::shared_ptr<MalType> list(std::vector<std::shared_ptr<MalType>> args)
{
    auto list = std::make_shared<MalList>('(', ')');
    for (auto arg : args)
        list->push_back(arg);

    return list;
}

std::shared_ptr<MalType> is_list(std::vector<std::shared_ptr<MalType>> args)
{
    bool result = false;

    if (args[0]->type() == MalType::Type::List)
    {
        auto list = static_cast<MalList &>(*args[0]);
        result = list.is_list();
    }

    return result ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> empty(std::vector<std::shared_ptr<MalType>> args)
{
    MalList &list = static_cast<MalList &>(*args[0]);
    return list.empty() ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> count(std::vector<std::shared_ptr<MalType>> args)
{
    if (args[0]->type() != MalType::Type::List)
        return std::make_shared<MalInt>(0);

    MalList &list = static_cast<MalList &>(*args[0]);
    return std::make_shared<MalInt>(list.size());
}

std::shared_ptr<MalType> eq(std::vector<std::shared_ptr<MalType>> args)
{
    if (args[0]->type() != args[1]->type())
        return std::make_shared<MalSymbol>(false_);

    return *args[0] == *args[1] ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> gt(std::vector<std::shared_ptr<MalType>> args)
{
    bool result = static_cast<MalInt &>(*args[0]) > static_cast<MalInt &>(*args[1]);
    return result ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> ge(std::vector<std::shared_ptr<MalType>> args)
{
    bool result = static_cast<MalInt &>(*args[0]) >= static_cast<MalInt &>(*args[1]);
    return result ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> lt(std::vector<std::shared_ptr<MalType>> args)
{
    bool result = static_cast<MalInt &>(*args[0]) < static_cast<MalInt &>(*args[1]);
    return result ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> le(std::vector<std::shared_ptr<MalType>> args)
{
    bool result = static_cast<MalInt &>(*args[0]) <= static_cast<MalInt &>(*args[1]);
    return result ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> read_string(std::vector<std::shared_ptr<MalType>> args)
{
    auto &input = static_cast<MalSymbol &>(*args[0]);
    return read_str(input);
}

std::shared_ptr<MalType> slurp(std::vector<std::shared_ptr<MalType>> args)
{
    std::string input = static_cast<MalSymbol &>(*args[0]);
    std::ifstream ifs(input);
    std::string content((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
    return std::make_shared<MalSymbol>('"' + content + '"');
}

std::shared_ptr<MalType> atom(std::vector<std::shared_ptr<MalType>> args)
{
    return std::make_shared<MalAtom>(args[0]);
}

std::shared_ptr<MalType> is_atom(std::vector<std::shared_ptr<MalType>> args)
{
    return args[0]->type() == MalType::Type::Atom ? std::make_shared<MalSymbol>(true_) : std::make_shared<MalSymbol>(false_);
}

std::shared_ptr<MalType> deref(std::vector<std::shared_ptr<MalType>> args)
{
    auto &atom = static_cast<MalAtom &>(*args[0]);
    return atom.deref();
}

std::shared_ptr<MalType> reset(std::vector<std::shared_ptr<MalType>> args)
{
    auto &atom = static_cast<MalAtom &>(*args[0]);
    return atom.reset(args[1]);
}

std::shared_ptr<MalType> swap(std::vector<std::shared_ptr<MalType>> args)
{
    auto &atom = static_cast<MalAtom &>(*args[0]);
    auto &func = static_cast<MalFunc &>(*args[1]);

    std::vector<std::shared_ptr<MalType>> args_{atom.deref()};
    for (unsigned i = 2; i < args.size(); ++i)
        args_.push_back(args[i]);

    return atom.reset(func(args_));
}

std::shared_ptr<MalType> cons(std::vector<std::shared_ptr<MalType>> args)
{
    auto list = std::make_shared<MalList>('(', ')');
    list->push_back(args[0]);

    for (auto arg : static_cast<MalList &>(*args[1]))
        list->push_back(arg);

    return list;
}

std::shared_ptr<MalType> concat(std::vector<std::shared_ptr<MalType>> args)
{
    auto list = std::make_shared<MalList>('(', ')');

    for (auto arg : args)
        for (auto item : static_cast<MalList &>(*arg))
            list->push_back(item);

    return list;
}

std::shared_ptr<MalType> vec(std::vector<std::shared_ptr<MalType>> args)
{
    auto &list = static_cast<MalList &>(*args[0]);
    if (list.is_vector())
        return args[0];

    return list.to_vector();
}

std::shared_ptr<MalType> nth(std::vector<std::shared_ptr<MalType>> args)
{
    if (args[0]->type() != MalType::Type::List)
        return std::make_shared<MalSymbol>(nil_);

    auto &list = static_cast<MalList &>(*args[0]);
    auto &index = static_cast<MalInt &>(*args[1]);

    return list[index];
}

std::shared_ptr<MalType> first(std::vector<std::shared_ptr<MalType>> args)
{
    if (args[0]->type() != MalType::Type::List)
        return std::make_shared<MalSymbol>(nil_);

    auto &list = static_cast<MalList &>(*args[0]);
    if (list.empty())
        return std::make_shared<MalSymbol>(nil_);

    return list[0];
}

std::shared_ptr<MalType> rest(std::vector<std::shared_ptr<MalType>> args)
{
    auto new_list = std::make_shared<MalList>('(', ')');

    if (args[0]->type() != MalType::Type::List)
        return new_list;

    auto &list = static_cast<MalList &>(*args[0]);
    if (list.empty())
        return new_list;

    for (unsigned i = 1; i < list.size(); ++i)
        new_list->push_back(list[i]);

    return new_list;
}

std::map<std::string, std::shared_ptr<MalType>> ns()
{
    return {
        {"true", std::make_shared<MalSymbol>(true_)},
        {"false", std::make_shared<MalSymbol>(false_)},
        {"nil", std::make_shared<MalSymbol>(nil_)},
        {"+", std::make_shared<MalFunc>(add)},
        {"-", std::make_shared<MalFunc>(sub)},
        {"*", std::make_shared<MalFunc>(mul)},
        {"/", std::make_shared<MalFunc>(divide)},
        {"pr-str", std::make_shared<MalFunc>(prstr)},
        {"str", std::make_shared<MalFunc>(str)},
        {"prn", std::make_shared<MalFunc>(prn)},
        {"println", std::make_shared<MalFunc>(println)},
        {"list", std::make_shared<MalFunc>(list)},
        {"list?", std::make_shared<MalFunc>(is_list)},
        {"empty?", std::make_shared<MalFunc>(empty)},
        {"count", std::make_shared<MalFunc>(count)},
        {"=", std::make_shared<MalFunc>(eq)},
        {"<", std::make_shared<MalFunc>(lt)},
        {"<=", std::make_shared<MalFunc>(le)},
        {">", std::make_shared<MalFunc>(gt)},
        {">=", std::make_shared<MalFunc>(ge)},
        {"read-string", std::make_shared<MalFunc>(read_string)},
        {"slurp", std::make_shared<MalFunc>(slurp)},
        {"atom", std::make_shared<MalFunc>(atom)},
        {"atom?", std::make_shared<MalFunc>(is_atom)},
        {"deref", std::make_shared<MalFunc>(deref)},
        {"reset!", std::make_shared<MalFunc>(reset)},
        {"swap!", std::make_shared<MalFunc>(swap)},
        {"cons", std::make_shared<MalFunc>(cons)},
        {"concat", std::make_shared<MalFunc>(concat)},
        {"vec", std::make_shared<MalFunc>(vec)},
        {"nth", std::make_shared<MalFunc>(nth)},
        {"first", std::make_shared<MalFunc>(first)},
        {"rest", std::make_shared<MalFunc>(rest)},
    };
}
