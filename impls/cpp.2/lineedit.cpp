/* The following code applies the GNU Readline library, which is
   licensed under the GPL version 3.0. Please refer to the file
   'LICENSE' in the implementation subdirectory.
*/

#include <string>
#include <cstdio>
#include <cstdlib>
#include <histedit.h>
#include "lineedit.h"

LineEdit::LineEdit()
{
    // By default readline does filename completion. With -d, we disable this
    // by asking readline to just insert the TAB character itself.
    rl_bind_key('\t', rl_insert);
}

LineEdit::~LineEdit()
{

}

std::string LineEdit::getline(std::string prompt)
{
    char *buffer;
    std::string s;

    while (true)
    {
        buffer = readline(prompt.c_str());
        if (buffer == nullptr)
        {
            throw new EndOfInputException();
        }
        if (strlen(buffer) != 0)
        {
            s = buffer;
            add_history(buffer);
            break;
        }
        free(buffer);
    }
    free(buffer);
    return s;
}

