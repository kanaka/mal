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
            {
                oss << pr_str(l, print_readably) << ' ';
            }
            oss.seekp(-1, oss.end);
        }
        oss << list.rparen();
        return oss.str();
    }
    case MalType::Type::Func:
        return "#<function>";
    default:
        return "";
    }
}
