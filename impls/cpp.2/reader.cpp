#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "types.h"
#include "exceptions.h"


void read_whitespace(std::string input_stream);
void read_comment(std::string input_stream);
void read_hashmap(std::string input_stream);
void read_string(std::string input_stream, char leading, TokenVector& tokens);
void read_number(std::string input_stream, char leading, TokenVector& tokens);
void read_symbol(std::string input_stream, char leading, TokenVector& tokens);

unsigned int paren_count = 0;
unsigned int square_bracket_count = 0;
unsigned int hm_count = 0;
unsigned int s_index = 0;


TokenVector read_str(std::string s)
{
    paren_count = 0;
    square_bracket_count = 0;
    hm_count = 0;
    s_index = 0;
    return tokenize(s);
}


TokenVector tokenize(std::string input_stream)
{
    TokenVector tokens;
    char ch;

    while (s_index < input_stream.length())
    {
        std::string s = "";

        ch = input_stream[s_index++];

        if (isspace(ch))
        {
            read_whitespace(input_stream);
        }
        else if (ch == ';')
        {
            read_comment(input_stream);
        }
        else if (ch == '(')
        {
            paren_count++;

            ch = input_stream[s_index++];
            while (isspace(ch))
            {
                ch = input_stream[s_index++];
            }
            if (ch == ')')
            {
                tokens.append(std::make_shared<MalNull>());
            }
            else
            {
                s_index--;
                tokens.append(std::make_shared<MalList>(tokenize(input_stream)));
            }
        }
        else if (ch == ')')
        {
            if (paren_count > 0)
            {
                paren_count--;
            }
            else
            {
                throw new UnbalancedParenthesesException();
            }
            break;
        }
        else if (ch == '[')
        {
            square_bracket_count++;
            tokens.append(std::make_shared<MalVector>(tokenize(input_stream)));
        }
        else if (ch == ']')
        {
            if (square_bracket_count > 0)
            {
                square_bracket_count--;
            }
            else
            {
                throw new UnbalancedVectorException();
            }
            break;
        }
        else if (ch == '{')
        {
            hm_count++;
            tokens.append(std::make_shared<MalHashmap>(tokenize(input_stream)));
        }
        else if (ch == '}')
        {
            if (hm_count > 0)
            {
                hm_count--;
            }
            else
            {
                throw new UnbalancedHashmapException();
            }
        }
        else if (ch == '.')
        {
            tokens.append(std::make_shared<MalPeriod>());
        }
        else if (ch == ',')
        {
            tokens.append(std::make_shared<MalComma>());
        }
        else if (ch == '@')
        {
            tokens.append(std::make_shared<MalAt>());
        }
        else if (ch == '\'')
        {
            tokens.append(std::make_shared<MalQuote>());
        }
        else if (ch == '`')
        {
            s += ch;
            tokens.append(std::make_shared<MalQuasiquote>());
        }
        else if (ch == '\"')
        {
            read_string(input_stream, ch, tokens);
        }
        else if (isdigit(ch))
        {
            read_number(input_stream, ch, tokens);
        }
        else
        {
            read_symbol(input_stream, ch, tokens);
        }
    }

    if (paren_count > 0)
    {
        throw new UnbalancedParenthesesException();
    }
    if (square_bracket_count > 0)
    {
        throw new UnbalancedVectorException();
    }
    if (hm_count > 0)
    {
        throw new UnbalancedHashmapException();
    }

    return tokens;
}

void read_whitespace(std::string input_stream)
{
    for (char ch = ' '; isspace(ch) && s_index < input_stream.length(); s_index++)
    {
        ch = input_stream[s_index];
    }
    if (s_index < input_stream.length())
    {
        s_index--;
    }
}

void read_comment(std::string input_stream)
{
    for (char ch = input_stream[s_index]; ch != '\n' && s_index < input_stream.length(); s_index++)
    {
        ch = input_stream[s_index];
    }
}


void read_string(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s = "";
    char ch = leading;

    s += ch;
    ch = input_stream[s_index++];
    while ((ch != '\"') && s_index < input_stream.length())
    {
        if ((ch == '\\' ) && s_index < input_stream.length())
        {
            ch = input_stream[s_index++];
            if (s_index > input_stream.length())
            {
                throw new IncompleteEscapeException();
            }
            switch (ch)
            {
                case '\"':
                case '\\':
                    s += ch;
                    break;
                case 'n':
                    s += '\n';
                    break;
                case 't':
                    s += '\t';
                    break;
                case 'r':
                    s += '\r';
                    break;
                default:
                    throw new IncompleteEscapeException();
            }
            ch = input_stream[s_index++];
            continue;
        }
        s += ch;
        ch = input_stream[s_index++];
    }
    if (s_index > input_stream.length())
    {
        throw new UnbalancedStringException();
    }
    s += '\"';
    tokens.append(std::make_shared<MalString>(s));
}



void read_number(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s;
    char ch = leading;

    if (ch == '0')
    {
        s += ch;
        ch = input_stream[s_index++];
        switch (ch)
        {
            case 'x':
                s += ch;
                ch = input_stream[s_index++];
                while ((isdigit(ch) ||
                        (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
                        && s_index < input_stream.length())
                {
                    s += ch;
                    ch = input_stream[s_index++];
                }
                if (s_index < input_stream.length())
                {
                    s_index--;
                }
                else
                {
                    s += ch;
                }
                tokens.append(std::make_shared<MalHex>(s));
                break;

            case 'b':
                s += ch;
                ch = input_stream[s_index++];
                while ((ch == '0' || ch == '1') && s_index < input_stream.length())
                {
                    s += ch;
                    ch = input_stream[s_index++];
                }
                if (s_index < input_stream.length())
                {
                    s_index--;
                }
                else
                {
                    s += ch;
                }
                tokens.append(std::make_shared<MalBinary>(s));
                break;
            case '0':
                ch = input_stream[s_index++];
                while ((ch == '0') && s_index < input_stream.length())
                {
                    s += ch;
                    ch = input_stream[s_index++];
                }
                if (s_index < input_stream.length())
                {
                    s_index--;
                }
                else
                {
                    s += ch;
                }
                tokens.append(std::make_shared<MalInteger>(s));
                break;

            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
                s += ch;
                ch = input_stream[s_index++];
                while ((ch >= '0' && ch <= '7') && s_index < input_stream.length())
                {
                    s += ch;
                    ch = input_stream[s_index++];
                }
                if (s_index < input_stream.length())
                {
                    s_index--;
                }
                else
                {
                    s += ch;
                }
                tokens.append(std::make_shared<MalOctal>(s));
                break;
        }
    }
    else
    {
        bool already_found = false;
        while (isdigit(ch) && s_index < input_stream.length())
        {
            s += ch;
            ch = input_stream[s_index++];
            if (!isdigit(ch))
            {
                switch (ch)
                {
                    case '.':
                        s += ch;
                        ch = input_stream[s_index++];
                        while (isdigit(ch) && s_index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[s_index++];
                        }
                        if (ch == '+' || ch == '-')
                        {
                            while (isdigit(ch) && s_index < input_stream.length())
                            {
                                s += ch;
                                ch = input_stream[s_index++];
                            }
                            if (ch == '.')
                            {
                                while (isdigit(ch) && s_index < input_stream.length())
                                {
                                    s += ch;
                                    ch = input_stream[s_index++];
                                }
                            }
                            if (ch == 'i')
                            {
                                s += ch;
                                tokens.append(std::make_shared<MalComplex>(s));
                                already_found = true;
                            }
                            else
                            {
                                throw new IncompleteComplexNumberException();
                            }
                        }
                        else if (s_index < input_stream.length())
                        {
                            s_index--;
                        }
                        else
                        {
                            s += ch;
                        }
                        tokens.append(std::make_shared<MalDecimal>(s));
                        already_found = true;
                        break;
                    case '/':
                        s += ch;
                        ch = input_stream[s_index++];
                        while (isdigit(ch) && s_index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[s_index++];
                        }
                        if (s_index < input_stream.length())
                        {
                            s_index--;
                        }
                        else
                        {
                            s += ch;
                        }
                        tokens.append(std::make_shared<MalRational>(s));
                        already_found = true;
                        break;
                    case '+':
                    case '-':
                        s += ch;
                        ch = input_stream[s_index++];
                        while ((isdigit(ch) || ch == 'i') && s_index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[s_index++];
                        }
                        if (ch == '.')
                        {
                            while (isdigit(ch) && s_index < input_stream.length())
                            {
                                s += ch;
                                ch = input_stream[s_index++];
                            }
                        }
                        if (ch == 'i')
                        {
                            s += ch;
                            tokens.append(std::make_shared<MalComplex>(s));
                            already_found = true;
                        }
                        else
                        {
                            throw new IncompleteComplexNumberException();
                        }
                        if (s_index < input_stream.length())
                        {
                            s_index--;
                        }
                        else
                        {
                            s += ch;
                        }
                        break;
                    default:
                        if (!(isspace(ch) || ch == ')' || ch == ']'))
                        {
                            throw new InvalidNumberException();
                        }
                }
            }
        }
        if (!already_found)
        {
            if (s_index < input_stream.length())
            {
                s_index--;
            }
            else
            {
                s += ch;
            }
            tokens.append(std::make_shared<MalInteger>(s));
        }
    }
}

void read_symbol(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s = "";

    char ch = leading;

    while ((!isspace(ch) && ch != ')' && ch != ']' && ch != '}') && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }
    if (s_index < input_stream.length() || ch == ')' || ch == ']' || ch == '}')
    {
        s_index--;
    }
    else
    {
        s += ch;
    }

    if (s == "nil")
    {
        tokens.append(std::make_shared<MalNull>());
    }
    else if (s == "true" || s == "false")
    {
        tokens.append(std::make_shared<MalBoolean>(s));
    }
    else if (s[0] == ':')
    {
        tokens.append(std::make_shared<MalKeyword>(s));
    }
    else
    {
        tokens.append(std::make_shared<MalSymbol>(s));
    }
}