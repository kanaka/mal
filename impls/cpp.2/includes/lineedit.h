#ifndef LINEEDIT_H
#define LINEEDIT_H

#include <string>
#include <cstdio>

#include <readline/readline.h>
#include <readline/history.h>

class EndOfInputException
{
public:
    EndOfInputException() {};
};

class LineEdit
{
public:
    LineEdit();
    ~LineEdit();
    std::string getline(std::string prompt);

private:

};

#endif
