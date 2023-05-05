#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "token.h"

std::vector<std::unique_ptr<Token> > read_str(std::string s, LineEdit& line)
{
    int paren_count = 0;
    std::vector<std::unique_ptr<Token> > tokens;
    tokens = tokenize(s, paren_count);
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
            abort();
        }
        std::vector<std::unique_ptr<Token> > additional_tokens;
        additional_tokens = tokenize(s, paren_count);
        for (std::vector<std::unique_ptr<Token> >::iterator it = additional_tokens.begin();
             it != additional_tokens.end();
             ++it)
        {
            tokens.push_back(std::move(*it));
        }
    }

    return tokens;
}


std::vector<std::unique_ptr<Token> > tokenize(std::string input_stream, int& paren_count)
{
    unsigned int index = 0;
    std::vector<std::unique_ptr<Token> > tokens;
    char ch;

    while (index < input_stream.length())
    {
        std::string s = "";

        ch = input_stream[index++];
        s += ch;

        if (isspace(ch))
        {
            continue;
        }
        else if (ch == '(')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, LPAREN));
            paren_count++;
        }
        else if (ch == ')')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, RPAREN));
            if (paren_count > 0)
            {
                paren_count--;
            }
        }
        else if (ch == ',')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, COMMA));
        }
        else if (ch == '@')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, AT));
        }
        else if (ch == '\'')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, QUOTE));
        }
        else if (ch == '`')
        {
            s += ch;
            tokens.push_back(std::make_unique<Token>(s, QUASIQUOTE));
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
            tokens.push_back(std::make_unique<Token>(s, STRING));
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
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, HEX));
                        break;

                    case 'b':
                        ch = input_stream[index++];
                        while ((ch == '0' || ch == '1') && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, BINARY));
                        break;
                    case '0':
                        ch = input_stream[index++];
                        while (ch == '0')
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, INTEGER));
                        break;

                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                        ch = input_stream[index++];
                        while (ch <= '0' && ch <= '7')
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, OCTAL));
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
                        while (isdigit(ch))
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, INTEGER));
                        break;
                    case '/':
                        while ((isdigit(ch)) && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, RATIONAL));
                        break;
                    case '+':
                        while ((isdigit(ch) || ch =='i') && index < input_stream.length())
                        {
                            s += ch;
                            ch = input_stream[index++];
                            if (ch == 'i')
                            {
                                break;
                            }
                        }
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, COMPLEX));
                        break;
                    default:
                        index--;
                        tokens.push_back(std::make_unique<Token>(s, INTEGER));
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
            index--;
            tokens.push_back(std::make_unique<Token>(s, SYMBOL));
        }
    }

    return tokens;
}