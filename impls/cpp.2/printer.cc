#include "printer.hh"
#include <sstream>

std::string escape(const std::string &token)
{
    std::ostringstream oss;

    oss << '"';
    for (unsigned int i = 0; i < token.size(); ++i)
    {
        switch (token[i])
        {
        case '"':
            oss << "\\\"";
            break;
        case '\n':
            oss << "\\n";
            break;
        case '\\':
            oss << "\\\\";
            break;
        default:
            oss << token[i];
            break;
        }
    }
    oss << '"';

    return oss.str();
}

std::string pr_str(std::shared_ptr<MalType> input, bool print_readably)
{
    if (!input)
    {
        return "";
    }

    switch (input->type())
    {
    case MalType::Type::Symbol:
    {
        auto symbol = static_cast<MalSymbol &>(*input);
        return (print_readably && symbol.is_string()) ? escape(symbol) : static_cast<std::string>(symbol);
    }
    case MalType::Type::Int:
        return std::to_string(static_cast<MalInt &>(*input));
    case MalType::Type::List:
    {
        std::ostringstream oss;
        auto &list = static_cast<MalList &>(*input);
        oss << list.lparen();
        if (!list.empty())
        {
            for (auto &l : list)
                oss << pr_str(l, print_readably) << ' ';
            oss.seekp(-1, oss.end);
        }
        oss << list.rparen();
        return oss.str();
    }
    case MalType::Type::Map:
    {
        std::ostringstream oss;
        auto &map = static_cast<MalMap &>(*input);
        oss << '{';
        if (!map.empty())
        {
            for (auto &[key, value] : map)
                oss << ((print_readably && key.is_string()) ? escape(key) : static_cast<std::string>(key)) << ' ' << pr_str(value, print_readably) << ' ';
            oss.seekp(-1, oss.end);
        }
        oss << '}';
        return oss.str();
    }
    case MalType::Type::Func:
        return "#<function>";
    case MalType::Type::Atom:
    {
        auto &atom = static_cast<MalAtom &>(*input);
        return "(atom " + pr_str(atom.deref(), print_readably) + ")";
    }
    default:
        return "";
    }
}
