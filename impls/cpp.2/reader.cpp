#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "types.h"

unsigned int paren_count=0;

TokenVector read_str(std::string s, LineEdit& line, unsigned int index=0)
{
    TokenVector tokens(tokenize(s, line, index));

    while (paren_count > 0)
    {
        std::string input = "";
        try
        {
            input = line.getline("..... ");
        }
        catch (EndOfInputException* e)
        {
            std::cout << "\nEscape during multi-line edit, exiting.\n";
            exit(0);
        }
        TokenVector additional_tokens(tokenize(input, line, index));
        tokens.append(additional_tokens);
    }

    return tokens;
}


TokenVector tokenize(std::string input_stream, LineEdit& line, unsigned int index)
{
    TokenVector tokens;
    char ch;

    while (index < input_stream.length())
    {
        std::string s = "";

        ch = input_stream[index++];

        if (isspace(ch))
        {
            continue;
        }
        else if (ch == ';')
        {
            while (ch != '\n' && index < input_stream.length())
            {
                // discard comment
            }
        }
        else if (ch == '(')
        {
            tokens.append(std::make_unique<MalList>(read_str(input_stream, line, index)));
        }
        else if (ch == ')')
        {
            break;
        }
        else if (ch == '.')
        {
            tokens.append(std::make_unique<MalPeriod>());
        }
        else if (ch == ',')
        {
            tokens.append(std::make_unique<MalComma>());
        }
        else if (ch == '@')
        {
            tokens.append(std::make_unique<MalAt>());
        }
        else if (ch == '\'')
        {
            tokens.append(std::make_unique<MalQuote>());
        }
        else if (ch == '`')
        {
            s += ch;
            tokens.append(std::make_unique<MalQuasiquote>());
        }
        else if (ch == '\"')
        {
            while ((ch != '\"') && index < input_stream.length())
            {
                if ((ch == '\\' ) && index < input_stream.length())
                {
                    ch = input_stream[index++];
                    if (ch == '\"')
                    {
                        s += ch;
                    }
                }
                s += ch;
                ch = input_stream[index++];
            }
            tokens.append(std::make_unique<MalString>(s));
        }

        else if (isdigit(ch))
        {
            if (ch == '0')
            {
                ch = input_stream[index];
                s+= ch;
                switch (ch)
                {
                    case 'x':
                        ch = input_stream[index++];
                        while ((isdigit(ch) ||
                                (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
                               && index < input_stream.length())
                            {
                                s += ch;
                                ch = input_stream[index++];
                            }
                        index--;
                        tokens.append(std::make_unique<MalHex>(s));
                        break;

                    case 'b':
                        ch = input_stream[index++];
                        while ((ch == '0' || ch == '1') && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.append(std::make_unique<MalBinary>(s));
                        break;
                    case '0':
                        ch = input_stream[index++];
                        while ((ch == '0') && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.append(std::make_unique<MalInteger>(s));
                        break;

                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                        ch = input_stream[index++];
                        while ((ch >= '0' && ch <= '7') && index < input_stream.length())
                        {
                            ch = input_stream[index++];
                            s += ch;
                        }
                        if (index < input_stream.length())
                        {
                            index--;
                        }
                        tokens.append(std::make_unique<MalOctal>(s));
                        break;
                }
            }
            else
            {
                while (isdigit(ch))
                {
                    s += ch;
                    ch = input_stream[index++];
                }
                switch (ch)
                {
                    case '.':
                        s += ch;
                        ch = input_stream[index++];
                        while (isdigit(ch) && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        if (index < input_stream.length())
                        {
                            index--;
                        }
                        tokens.append(std::make_unique<MalDecimal>(s));
                        break;
                    case '/':
                        s += ch;
                        ch = input_stream[index++];
                        while (isdigit(ch) && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        if (index < input_stream.length())
                        {
                            index--;
                        }
                        tokens.append(std::make_unique<MalRational>(s));
                        break;
                    case '+':
                        s += ch;
                        ch = input_stream[index++];
                        while ((isdigit(ch) || ch == 'i') && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        if (index < input_stream.length())
                        {
                            index--;
                        }
                        else
                        {
                            s += ch;
                        }
                        if (ch == 'i')
                        {
                            tokens.append(std::make_unique<MalComplex>(s));
                        }
                        else
                        {
                            tokens.append(std::make_unique<MalSymbol>(s));
                        }
                        break;
                    default:
                        if (index < input_stream.length())
                        {
                            index--;
                        }
                        tokens.append(std::make_unique<MalInteger>(s));
                        break;
                }
            }
        }
        else
        {
            while ((!isspace(ch) && ch != ')') && index < input_stream.length())
            {
                s += ch;
                ch = input_stream[index++];
            }
            if (index < input_stream.length() || ch == ')')
            {
                index--;
            }
            else
            {
                s += ch;
            }
            tokens.append(std::make_unique<MalSymbol>(s));
        }
    }

    return tokens;
}