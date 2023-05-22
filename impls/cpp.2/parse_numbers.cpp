#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "reader.h"
#include "types.h"
#include "token_types.h"
#include "exceptions.h"
#include "parse_numbers.h"


void read_number(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s;
    char ch = leading;

    if (ch == '0')
    {
        if (input_stream[s_index]  == '.')
        {
            s_index++;
            read_fractional(input_stream, "0.", tokens);
        }
        else
        {
            read_based_integer(input_stream, tokens);
        }
    }
    else
    {
        read_decimal(input_stream, ch, tokens);
    }
}


void read_based_integer(std::string input_stream, TokenVector& tokens)
{
    std::string s = "0";
    char ch;

    ch = input_stream[s_index++];
    switch (ch)
    {
        case 'x':
            read_hex(input_stream, tokens);
            break;

        case 'b':
            read_binary(input_stream, tokens);
            break;

        case '0':
            read_trailing_zeroes(input_stream, tokens);
            break;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
            read_octal(input_stream, ch, tokens);
            break;
        case ' ':
        case ')':
        case ']':
        case '}':
            tokens.append(std::make_shared<MalInteger>(s));
            s_index--;
            break;
        default:
            if (s_index >= input_stream.length())
            {
                tokens.append(std::make_shared<MalInteger>(s));
            }
            else
            {
                throw new InvalidNumberException(s);
            }
    }
}

void read_trailing_zeroes(std::string input_stream, TokenVector& tokens)
{
    std::string s = "00";
    char ch = '0';
    s += ch;

    while (ch == '0' && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }
    if (!(ch == '0' || isspace(ch) || is_right_balanced(ch)))
    {
        throw new InvalidNumberException(s + ch);
    }
    else if (s_index < input_stream.length())
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalInteger>("0"));
}



void read_binary(std::string input_stream, TokenVector & tokens)
{
    std::string s = "0b";
    char ch = input_stream[s_index++];

    while (is_binary(ch) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }

    if (!(is_binary(ch) || isspace(ch) || is_right_balanced(ch)))
    {
        throw new InvalidBinaryNumberException(s + ch);
    }
    else if (s_index < input_stream.length())
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalBinary>(s));
}


void read_octal(std::string input_stream, char leading, TokenVector & tokens)
{
    std::string s = "0";
    char ch = leading;

    while (is_octal(ch) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }

    if (!(is_octal(ch) || isspace(ch) || is_right_balanced(ch)))
    {
        throw new InvalidOctalNumberException(s + ch);
    }
    else if (s_index < input_stream.length())
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalOctal>(s));
}


void read_hex(std::string input_stream, TokenVector & tokens)
{
    std::string s = "0x";
    char ch = input_stream[s_index++];

    while (is_hex(ch) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }

    if (!(is_hex(ch) || isspace(ch) || is_right_balanced(ch)))
    {
        throw new InvalidHexNumberException(s + ch);
    }
    else if (s_index < input_stream.length())
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalHex>(s));
}


void read_decimal(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s = "";
    char ch = leading;

    if (ch == '-' || ch == '+')
    {
        s += ch;
        ch = input_stream[s_index++];
    }

    while (isdigit(ch) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
        if (!isdigit(ch))
        {
            switch(ch)
            {
                case '.':
                    read_fractional(input_stream, s, tokens);
                    return;
                case '/':
                    read_rational(input_stream, s, tokens);
                    return;
                case '-':
                case '+':
                    read_complex(input_stream, s, ch, tokens);
                    return;
                default:
                    break;
            }
        }
    }

    if (!(isdigit(ch) || isspace(ch) || is_right_balanced(ch)  || ch == ','  || ch == ';'))
    {
        throw new InvalidNumberException(s + ch);
    }
    else if (s_index < input_stream.length() || isspace(ch) || is_right_balanced(ch) || ch == ','  || ch == ';')
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalInteger>(s));
}


void read_fractional(std::string input_stream, std::string leading, TokenVector& tokens)
{
    std::string s = leading + '.';
    char ch = input_stream[s_index++];

    while (isdigit(ch) && (s_index < input_stream.length()))
    {
        s += ch;
        ch = input_stream[s_index++];

        if (ch == '+' || ch == '-')
        {
            read_complex(input_stream, s, ch, tokens);
            return;
        }
    }

    if (!(isdigit(ch) || isspace(ch) || is_right_balanced(ch)  || ch == ','  || ch == ';'))
    {
        throw new InvalidNumberException(s + ch);
    }
    else if (s_index < input_stream.length() || isspace(ch) || is_right_balanced(ch) || ch == ',')
    { 
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalFractional>(s));
}


void read_rational(std::string input_stream, std::string leading, TokenVector& tokens)
{
    std::string s = leading;
    char ch = '/';

    s += ch;
    ch = input_stream[s_index++];
    while (isdigit(ch) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }

    if (!(isdigit(ch) || isspace(ch) || is_right_balanced(ch)  || ch == ','  || ch == ';'))
    {
        throw new InvalidNumberException(s + ch);
    }
    else if (s_index < input_stream.length() || isspace(ch) || is_right_balanced(ch) || ch == ',')
    {
        s_index--;
    }
    else
    {
        s += ch;
    }
    tokens.append(std::make_shared<MalRational>(s));
}



void read_complex(std::string input_stream, std::string leading, char trailing, TokenVector& tokens)
{
    std::string s = leading;
    char ch = trailing;

    s += ch;
    ch = input_stream[s_index++];
    while (isdigit(ch) && s_index < input_stream.length())
    {
        bool decimal_point_found = false;
        s += ch;
        ch = input_stream[s_index++];
        if (ch == '.')
        {
            if (!decimal_point_found)
            {
                decimal_point_found = true;
                s += ch;
                ch = input_stream[s_index++];
            }
            else
            {
                throw new InvalidComplexNumberException(s);
            }
            s += ch;
            ch = input_stream[s_index++];
        }
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
}