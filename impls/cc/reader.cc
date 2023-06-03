#include "reader.hh"
#include <charconv>
#include <iostream>
#include <sstream>

std::unique_ptr<MalType> read_from(Reader &reader);

Reader tokenize(const std::string &input)
{
    std::regex regex(R"([\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*))");
    return Reader(input, regex);
}

std::string unescape(const std::string &token)
{
    std::ostringstream oss;

    unsigned int i = 0;
    for (; i < token.size() - 1; ++i)
    {
        if (token[i] == '\\')
        {
            switch (token[++i])
            {
            case '"':
                oss << '"';
                break;
            case 'n':
                oss << '\n';
                break;
            case '\\':
                oss << '\\';
                break;
            default:
                oss << '\\' << token[i];
                break;
            }
        }
        else
        {
            oss << token[i];
        }
    }

    oss << token[i];
    return oss.str();
}

std::unique_ptr<MalType> read_atom(Reader &reader)
{
    if (reader.empty())
    {
        return nullptr;
    }

    auto token = reader.next();
    if (token[0] == '"')
    {
        auto unescaped_token = unescape(token);
        if (*unescaped_token.rbegin() != '"' || unescaped_token.size() < 2)
        {
            std::cerr << "unbalanced";
            return nullptr;
        }

        return std::make_unique<MalSymbol>(unescaped_token);
    }

    int result;
    auto [ptr, ec] = std::from_chars(token.c_str(), token.c_str() + token.size(), result);
    if (ec == std::errc() && ptr == token.c_str() + token.size())
    {
        return std::make_unique<MalInt>(result);
    }

    return std::make_unique<MalSymbol>(token);
}

std::unique_ptr<MalList> read_list(Reader &reader, char lparen, char rparen)
{
    if (reader.empty())
    {
        std::cerr << "unbalanced";
        return nullptr;
    }

    auto list = std::make_unique<MalList>(lparen, rparen);
    auto token = read_from(reader);

    while (token && (token->type() != MalType::Type::Symbol ||
                     static_cast<MalSymbol &>(*token) != std::string(1, rparen)))
    {
        list->push_back(std::move(token));
        token = read_from(reader);
    }

    if (!token)
    {
        std::cerr << "unbalanced";
        return nullptr;
    }

    return list;
}

std::unique_ptr<MalList> read_macro(Reader &reader, const std::string &name)
{
    auto macro = read_from(reader);
    auto result = std::make_unique<MalList>('(', ')');
    result->push_back(std::make_unique<MalSymbol>(name));
    result->push_back(std::move(macro));
    return result;
}

std::unique_ptr<MalList> read_meta(Reader &reader)
{
    auto map = read_from(reader);
    auto vector = read_from(reader);
    auto result = std::make_unique<MalList>('(', ')');
    result->push_back(std::make_unique<MalSymbol>("with-meta"));
    result->push_back(std::move(vector));
    result->push_back(std::move(map));
    return result;
}

std::unique_ptr<MalType> read_from(Reader &reader)
{
    if (reader.empty())
    {
        return nullptr;
    }

    auto token = reader.peak();
    switch (token[0])
    {
    case '(':
        reader.next();
        return read_list(reader, '(', ')');
    case '[':
        reader.next();
        return read_list(reader, '[', ']');
    case '{':
        reader.next();
        return read_list(reader, '{', '}');
    case '\'':
        reader.next();
        return read_macro(reader, "quote");
    case '`':
        reader.next();
        return read_macro(reader, "quasiquote");
    case '@':
        reader.next();
        return read_macro(reader, "deref");
    case '~':
        reader.next();
        return read_macro(reader, token.size() == 1 ? "unquote" : "splice-unquote");
    case '^':
        reader.next();
        return read_meta(reader);
    default:
        return read_atom(reader);
    }
}

std::unique_ptr<MalType> read_str(const std::string &input)
{
    auto reader = tokenize(input);
    return read_from(reader);
}
