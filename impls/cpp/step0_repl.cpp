#include "String.h"
#include "ReadLine.h"

#include <iostream>
#include <memory>

String READ(const String& input);
String EVAL(const String& ast);
String PRINT(const String& ast);
String rep(const String& input);

static ReadLine s_readLine("~/.mal-history");

int main(int argc, char* argv[])
{
    String prompt = "user> ";
    String input;
    while (s_readLine.get(prompt, input)) {
        std::cout << rep(input) << "\n";
    }
    return 0;
}

String rep(const String& input)
{
    return PRINT(EVAL(READ(input)));
}

String READ(const String& input)
{
    return input;
}

String EVAL(const String& ast)
{
    return ast;
}

String PRINT(const String& ast)
{
    return ast;
}
