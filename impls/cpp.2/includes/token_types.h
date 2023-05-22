#ifndef TOKEN_TYPES_H
#define TOKEN_TYPES_H


inline bool is_left_balanced(char ch)
{
    return (ch == '(' || ch == '[' || ch == '{' || ch == '\"');
}


inline bool is_right_balanced(char ch)
{
    return (ch == ')' || ch == ']' || ch == '}');
}


inline bool is_right_closed(char ch)
{
    return (is_right_balanced(ch) || ch == '\"');
}

inline bool is_syntax(char ch)
{
    return (is_left_balanced(ch) || is_right_closed(ch)
            || ch == '\'' || ch == '`' || ch == '^'
            || ch == '@'  || ch == ',' || ch == '~'
            || ch == '^'  ||  ch == '.' || ch == ';');
}

inline bool is_binary(char ch)
{
    return (ch == '0' || ch == '1');
}

inline bool between(char test, char ch1, char ch2)
{
    return ((test >= ch1) && (test <= ch2));
}

inline bool is_octal(char ch)
{
    return (is_binary(ch) || between(ch, '2', '7'));
}

inline bool is_hex(char ch)
{
    return (isdigit(ch) || between(ch, 'a', 'f') || between(ch, 'A', 'F'));
}

#endif