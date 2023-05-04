#include <string>



std::wstring READ(std::wstring input);
std::wstring EVAL(std::wstring input);
std::wstring PRINT(std::wstring input);
std::wstring rep(std::wstring input);

std::wstring READ(std::wstring input)
{
    return input;
}

std::wstring EVAL(std::wstring input)
{
    return input;
}

std::wstring PRINT(std::wstring input)
{
    return input;
}

std::wstring rep(std::wstring input)
{
    return PRINT(EVAL(READ(input)));
}

int main()
{
    std::wstring prompt = L"user> ";
    std::wstring input;

    while (true)
    {
        std::wcout << prompt;
        if(!std::getline(std::wcin, input))
            break;
        std::wcout << rep(input) << L'\n';
    }
    std::wcout << L"\nExiting.\n";

    return 0;
}