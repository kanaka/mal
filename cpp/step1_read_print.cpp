#include "MAL.h"

#include "ReadLine.h"
#include "Types.h"

#include <iostream>
#include <memory>

malValuePtr READ(const String& input);
String PRINT(malValuePtr ast);

static ReadLine s_readLine("~/.mal-history");

static String rep(const String& input);
static malValuePtr EVAL(malValuePtr ast);

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

static String rep(const String& input)
{
    return PRINT(EVAL(READ(input)));
}

malValuePtr READ(const String& input)
{
    return readStr(input);
}

static malValuePtr EVAL(malValuePtr ast)
{
    return ast;
}

String PRINT(malValuePtr ast)
{
    return ast->print(true);
}

// These have been added after step 1 to keep the linker happy.
malValuePtr EVAL(malValuePtr ast, malEnvPtr)
{
    return ast;
}

malValuePtr APPLY(malValuePtr ast, malValueIter, malValueIter, malEnvPtr)
{
    return ast;
}
