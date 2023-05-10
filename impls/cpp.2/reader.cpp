#include <iostream>
#include <memory>
#include <vector>
#include <cctype>
#include <cstdlib>
#include "lineedit.h"
#include "reader.h"
#include "types.h"
#include "exceptions.h"


TokenVector read_form(std::string input_stream);
void read_whitespace(std::string input_stream, char leading);
void read_comment(std::string input_stream);
void read_hashmap(std::string input_stream);
void read_string(std::string input_stream, char leading, TokenVector& tokens);
void read_number(std::string input_stream, char leading, TokenVector& tokens);
void read_based_integer(std::string input_stream, TokenVector& tokens);
void read_binary(std::string input_stream, TokenVector & tokens);
void read_octal(std::string input_stream, char leading, TokenVector & tokens);
void read_hex(std::string input_stream, TokenVector & tokens);
void read_trailing_zeroes(std::string input_stream, TokenVector& tokens);
void read_decimal(std::string input_stream, char leading, TokenVector& tokens);
void read_fractional(std::string input_stream, std::string leading, TokenVector& tokens);
void read_rational(std::string input_stream, std::string leading, TokenVector& tokens);
void read_complex(std::string input_stream, std::string leading, char trailing, TokenVector& tokens);
void read_symbol(std::string input_stream, char leading, TokenVector& tokens);
template<class RM> void read_reader_macro(std::string input_stream, TokenVector& tokens);
void read_tilde(std::string input_stream, TokenVector& tokens);
void read_meta(std::string input_stream, TokenVector& tokens);
void read_list(std::string input_stream, TokenVector& tokens);
void close_list();
void read_vector(std::string input_stream, TokenVector& tokens);
void close_vector();
void read_hashmap(std::string input_stream, TokenVector& tokens);
void close_hashmap();



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
    TokenVector tokens = tokenize(s);

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
        else
        {
            switch (ch)
            {
                case ';':
                    read_comment(input_stream);
                    break;
                case '(':
                    read_list(input_stream, tokens);
                    break;
                case ')':
                    close_list();
                    break;
                case '[':
                    read_vector(input_stream, tokens);
                    break;
                case ']':
                    close_vector();
                    return tokens;
                    break;
                case '{':
                    read_hashmap(input_stream, tokens);
                    break;
                case '}':
                    close_hashmap();
                    return tokens;
                    break;
                case '.':
                    tokens.append(std::make_shared<MalPeriod>());
                    break;
                case '~':
                    read_tilde(input_stream, tokens);
                    break;
                case ',':
                    continue;
                    break;
                case '@':
                    read_reader_macro<MalAt>(input_stream, tokens);
                    break;
                case '\'':
                    read_reader_macro<MalQuote>(input_stream, tokens);
                    break;
                case '`':
                    read_reader_macro<MalQuasiquote>(input_stream, tokens);
                    break;
                case '^':
                    read_meta(input_stream, tokens);
                    break;
                case '\"':
                    read_string(input_stream, ch, tokens);
                    break;
                default:
                    if (isdigit(ch))
                    {
                        read_number(input_stream, ch, tokens);
                    }
                    else
                    {
                        read_symbol(input_stream, ch, tokens);
                    }
            }
        }
    }

    return tokens;
}


bool is_left_balanced(char ch)
{
    return (ch == '(' || ch == '[' || ch == '{' || ch == '\"');
}


bool is_right_balanced(char ch)
{
    return (ch == ')' || ch == ']' || ch == '}');
}


bool is_right_closed(char ch)
{
    return (is_right_balanced(ch) || ch == '\"');
}

bool is_syntax(char ch)
{
    return (is_left_balanced(ch) || is_right_closed(ch)
            || ch == '\'' || ch == '`' || ch == '^'
            || ch == '@'  || ch == ',' || ch == '~'
            || ch == '^'  ||  ch == '.' || ch == ';');
}

bool is_binary(char ch)
{
    return (ch == '0' || ch == '1');
}

bool between(char test, char ch1, char ch2)
{
    return ((test >= ch1) && (test <= ch2));
}

bool is_octal(char ch)
{
    return (is_binary(ch) || between(ch, '2', '7'));
}

bool is_hex(char ch)
{
    return (isdigit(ch) || between(ch, 'a', 'f') || between(ch, 'A', 'F'));
}


TokenVector read_form(std::string input_stream)
{
    return tokenize(input_stream);
}



void read_whitespace(std::string input_stream, char leading)
{
    char ch = leading;
    if (!isspace(ch)) return;

    while (isspace(ch) && s_index < input_stream.length())
    {
        ch = input_stream[s_index++];
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
            if (s_index == input_stream.length())
            {
                throw new IncompleteEscapeException();
            }
            switch (ch)
            {
                case '\"':
                case '\'':
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
    if (ch != '\"')
    {
        throw new UnbalancedStringException();
    }
    s += ch;

    tokens.append(std::make_shared<MalString>(s));
}



void read_number(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s;
    char ch = leading;

    if (ch == '0')
    {
        read_based_integer(input_stream, tokens);
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
        default:
            tokens.append(std::make_shared<MalInteger>(s));
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
    while ((isdigit(ch) || ch == 'i') && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
        if (ch == '.')
        {
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



void read_symbol(std::string input_stream, char leading, TokenVector& tokens)
{
    std::string s = "";

    char ch = leading;

    while ((!isspace(ch) && !is_syntax(ch)) && s_index < input_stream.length())
    {
        s += ch;
        ch = input_stream[s_index++];
    }
    if (s_index < input_stream.length() || is_right_balanced(ch))
    {
        s_index--;
    }
    else
    {
        s += ch;
    }

    if (s == "nil")
    {
        tokens.append(std::make_shared<MalNil>());
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


void read_list(std::string input_stream, TokenVector& tokens)
{
    paren_count++;
    TokenVector ml = tokenize(input_stream);
    if (ml.values() == "")
    {
        tokens.append(std::make_shared<MalNull>());
    }
    else
    {
        tokens.append(std::make_shared<MalList>(ml));
    }
}


void close_list()
{
    if (paren_count > 0)
    {
        paren_count--;
    }
    else
    {
        throw new UnbalancedParenthesesException();
    }
}


void read_vector(std::string input_stream, TokenVector& tokens)
{
    square_bracket_count++;

    tokens.append(std::make_shared<MalVector>(tokenize(input_stream)));
}


void close_vector()
{
    if (square_bracket_count > 0)
    {
        square_bracket_count--;
    }
    else
    {
        throw new UnbalancedVectorException();
    }
}

void read_hashmap(std::string input_stream, TokenVector& tokens)
{
    hm_count++;

    tokens.append(std::make_shared<MalHashmap>(tokenize(input_stream)));
}


void close_hashmap()
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


template<class RM> void read_reader_macro(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    TokenVector rm_argument;

    if (is_left_balanced(ch))
    {
        switch(ch)
        {
            case '(':
                read_list(input_stream, rm_argument);
                break;
            case '[':
                read_vector(input_stream, rm_argument);
                break;
            case '{':
                read_hashmap(input_stream, rm_argument);
                break;
            case '\"':
                read_string(input_stream, ch, rm_argument);
                break;
        }
    }
    else if (isdigit(ch))
    {
        read_number(input_stream, ch, rm_argument);
    }
    else
    {
        read_symbol(input_stream, ch, rm_argument);
    }

    tokens.append(std::make_shared<RM>(rm_argument));
}


void read_tilde(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    if (ch == '@')
    {
        read_reader_macro<MalTildeAt>(input_stream, tokens);
    }
    else
    {
        s_index--;
        read_reader_macro<MalTilde>(input_stream, tokens);
    }
}


void read_meta(std::string input_stream, TokenVector& tokens)
{
    char ch = input_stream[s_index++];
    TokenVector seq_argument, main_argument;

    if (is_left_balanced(ch))
    {
        switch(ch)
        {
            case '(':
                read_list(input_stream, seq_argument);
                break;
            case '[':
                read_vector(input_stream, seq_argument);
                break;
            case '{':
                read_hashmap(input_stream, seq_argument);
                break;
            case '\"':
                read_string(input_stream, ch, seq_argument);
                break;
        }

        read_whitespace(input_stream, ch);
        if (s_index >= input_stream.length())
        {
            throw new InvalidMetaException();
        }

        s_index++;
        ch = input_stream[s_index++];
        std::cout << ch << std::endl;
        if (is_left_balanced(ch))
        {
            switch(ch)
            {
                case '(':
                    read_list(input_stream, main_argument);
                    break;
                case '[':
                    read_vector(input_stream, main_argument);
                    break;
                case '{':
                    read_hashmap(input_stream, main_argument);
                    break;
                case '\"':
                    read_string(input_stream, ch, main_argument);
                    break;
            }
        }
        else if (isdigit(ch))
        {
            read_number(input_stream, ch, main_argument);
        }
        else if (!is_syntax(ch))
        {
            read_symbol(input_stream, ch, main_argument);
        }
        else
        {
            throw new InvalidMetaException();
        }
    }
    else
    {
        throw new InvalidMetaException();
    }


    tokens.append(std::make_shared<MalMeta>(main_argument, seq_argument));
}

