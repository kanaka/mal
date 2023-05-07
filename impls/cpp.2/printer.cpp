#include <iostream>
#include <string>
#include "types.h"
#include "printer.h"


void pr_str(TokenVector tokens)
{
    std::cout << tokens.values() << '\n';
}