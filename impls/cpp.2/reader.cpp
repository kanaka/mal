#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "types.h"
#include "exceptions.h"

unsigned int paren_count = 0;
unsigned int square_bracket_count = 0;
unsigned int s_index = 0;


TokenVector read_str(std::string s)
{
    paren_count = 0;
    square_bracket_count = 0;
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
            continue;
        }
        else if (ch == ';')
        {
            while (ch != '\n' && s_index < input_stream.length())
            {
                s_index++;
            }
        }
        else if (ch == '(')
        {
            paren_count++;
            tokens.append(std::make_shared<MalList>(tokenize(input_stream)));
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
            s += ch;
            ch = input_stream[s_index++];
            while ((ch != '\"') && s_index < input_stream.length())
            {
                if ((ch == '\\' ) && s_index < input_stream.length())
                {
                    if (ch == '\"')
                    {
                        s += ch;
                    }
                }
                s += ch;
                ch = input_stream[s_index++];
            }
            if (s_index >= input_stream.length())
            {
                throw new UnbalancedStringException();
            }
            s += '\"';
            tokens.append(std::make_shared<MalString>(s));
        }

        else if (isdigit(ch))
        {
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
                while (isdigit(ch) && s_index < input_stream.length())
                {
                    s += ch;
                    ch = input_stream[s_index++];
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
                            tokens.append(std::make_shared<MalDecimal>(s));
                            break;
                        case '/':
                            s += ch;
                            ch = input_stream[s_index++];
                            while (isdigit(ch) && s_index < input_stream.length())
                            {
                                s += ch;
                                ch = input_stream[s_index++];
                            }
                            tokens.append(std::make_shared<MalRational>(s));
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
                            if (ch == 'i')
                            {
                                s += ch;
                                tokens.append(std::make_shared<MalComplex>(s));
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
                            break;
                    }
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
            }
        }
        else
        {
            while ((!isspace(ch) && ch != ')') && s_index < input_stream.length())
            {
                s += ch;
                ch = input_stream[s_index++];
            }
            if (s_index < input_stream.length() || ch == ')')
            {
                s_index--;
            }
            else
            {
                s += ch;
            }
            tokens.append(std::make_shared<MalSymbol>(s));
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


    return tokens;
}
