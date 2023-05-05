#ifndef TOKEN_H
#define TOKEN_H

#include <memory>
#include <string>


enum TokenType
{
    LPAREN, RPAREN,
    COMMA, AT, QUOTE, QUASIQUOTE,
    SYMBOL, STRING, CHAR,
    INTEGER, DECIMAL, RATIONAL, COMPLEX,
    HEX, OCTAL, BINARY
};


extern std::vector<std::string> token_type_names;



class Token
{
public:
    Token(std::string t, TokenType tt): token(t), type(tt) {}
    friend std::ostream& operator<<(std::ostream &os, Token const & token);
    friend std::ostream& operator<<(std::ostream &os, std::unique_ptr<Token> const & token);
    friend std::ostream& operator<<(std::ostream &os, std::vector<std::unique_ptr<Token> > const & token);

private:
    std::string token;
    TokenType type;
};


std::ostream& operator<<(std::ostream &os, Token const & token);
std::ostream& operator<<(std::ostream &os, std::unique_ptr<Token> const & token);
std::ostream& operator<<(std::ostream &os, std::vector<std::unique_ptr<Token> > const & token);

#endif