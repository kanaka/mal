#include "MAL.h"

#include "ReadLine.h"
#include "Types.h"

#include <iostream>
#include <memory>

malValuePtr READ(const String& input);
String PRINT(malValuePtr ast);

static ReadLine s_readLine("~/.mal-history");

int main(int argc, char* argv[])
{
    String prompt = "user> ";
    String input;
    while (s_readLine.get(prompt, input)) {
        String out;
        try {
            out = rep(input);
        }
        catch (malEmptyInputException&) {
            continue; // no output
        }
        catch (String& s) {
            out = s;
        };
        std::cout << out << "\n";
    }
    return 0;
}

String rep(const String& input)
{
    return PRINT(EVAL(READ(input)));
}

malValuePtr READ(const String& input)
{
    return readStr(input);
}

malValuePtr EVAL(malValuePtr ast)
{
    return ast;
}

String PRINT(malValuePtr ast)
{
    return ast->print(true);
}
