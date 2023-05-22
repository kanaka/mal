#include <iostream>
#include <string>
#include "types.h"
#include "printer.h"
#include "exceptions.h"


std::string pr_formatted_string(MalPtr p);
std::string pr_list(TokenVector list);

std::string pr_str(TokenVector tokens, bool print_readably)
{
    std::string s = "";

    if (print_readably)
    {
        for (auto token = tokens.next(); token != nullptr; token = tokens.next())
        {
            switch (token->type())
            {
                case MAL_PRINT_NIL:
                    continue;
                case MAL_LIST:
                    s += "(" + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_VECTOR:
                    s += "[" + pr_list(token->raw_value()) + "]";
                    break;
                case MAL_QUOTE:
                    s += "(quote " + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_QUASIQUOTE:
                    s += "(quasiquote " + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_UNQUOTE:
                    s += "(unquote " + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_SPLICE_UNQUOTE:
                    s += "(splice-unquote " + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_DEREF:
                    s += "(deref " + pr_list(token->raw_value()) + ")";
                    break;
                case MAL_META:
                    s += "(with-meta " + pr_list(token->raw_value()) + ")";
                    break;

                case MAL_HASHMAP:
                {
                    // WARNING: This function uses downcasting of a pointer from it's parent class to the
                    // actual subclass. This is VERY questionable, and if possible a better solution should be found!
                    HashMapInternal hm((dynamic_cast<MalHashmap*>(&(*token)))->internal_map());
                    s  += "{";
                    for (auto hash: hm)
                    {
                        if (hash.first[0] == ':')
                        {
                            s += hash.first + " ";
                        }
                        else
                        {
                            s += "\"" + hash.first + "\" ";
                        }
                        TokenVector temp;
                        temp.append(hash.second);
                        s += pr_str(temp, true);
                        temp.clear();
                    }
                    s  += "}";
                }
                    break;
                case MAL_STRING:
                    s += pr_formatted_string(token);
                    break;
                default:
                    s += token->value();
            }
        }
    }
    else
    {
        for (auto token = tokens.next(); token != nullptr; token = tokens.next())
        {
            if (token->type() != MAL_PRINT_NIL)
            {
                s += token->value();
            }
        }
    }

    return s;
}


std::string pr_formatted_string(MalPtr p)
{
    std::string s = "";
    auto source = p->value(); // string to parse

    for (size_t i = 0; i < source.length(); i++)
    {
        char ch = source[i];

        if (ch == '\"' && (i == 0 || i == source.length()-1))
        {
            s += '\\';
            s += ch;
        }
        else if (ch == '\\')
        {
            s += ch;
            s += ch;
            i++;
            if (i >= source.length())
            {
                throw new IncompleteEscapeException();
            }

            ch = source[i];
            switch (ch)
            {
                case '\\':
                case '\'':
                case '\"':
                {
                    s += '\\';
                    s += ch;
                }
                    break;
                case 'n':
                {
                    s += 'n';
                }
                    break;
                case 't':
                 {
                    s += 't';
                 }
                    break;
                default:
                {
                    throw new IncompleteEscapeException();
                }

            }
        }
        else
        {
            s += ch;
        }
    }

    return s;
}


std::string pr_list(TokenVector list)
{
    std::string s = "";

    for (auto element = list.next(); element != nullptr; element = list.next())
    {
        TokenVector temp;
        temp.append(element);
        s += pr_str(temp, true);
        if (list.peek() != nullptr)
        {
            s += " ";
        }
        temp.clear();
    }

    return s;
}