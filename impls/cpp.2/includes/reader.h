#ifndef READER_H
#define READER_H

#include <memory>
#include "token.h"


class Reader
{
public:
    Reader();
    std::unique_ptr<Token> next();
    std::unique_ptr<Token> peek();

private:

};

std::vector<std::unique_ptr<Token> > tokenize(std::string input_stream, unsigned int& paren_count);
std::vector<std::unique_ptr<Token> > read_str(std::string s, LineEdit& line);
#endif