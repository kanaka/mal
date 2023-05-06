#ifndef READER_H
#define READER_H

#include <string>
#include <memory>
#include <vector>
#include "lineedit.h"
#include "types.h"


class Reader
{
public:
    Reader(TokenVector t): tokens(t) {};
    MalPtr next();
    MalPtr peek();
private:
    TokenVector tokens;
};

TokenVector tokenize(std::string input_stream, LineEdit& line, unsigned int index);
TokenVector read_str(std::string s, LineEdit& line, unsigned int index);

#endif