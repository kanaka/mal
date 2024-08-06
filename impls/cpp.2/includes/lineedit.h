#ifndef LINEEDIT_H
#define LINEEDIT_H

/* The following code applies the GNU Readline library, which is
   licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/


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
