fun string READ(string input)
{
    return input;
}

fun string EVAL(string input)
{
    return input;
}

fun string PRINT(string input)
{
    return input;
}

fun string rep(string input)
{
    return input => READ => EVAL => PRINT;
}

fun void main()
{
    ConsoleInput stdin;
    string input;

    while( true )
    {
        stdin.prompt("user>") => now;
        stdin.getLine() => input;
        chout <= rep(input) + "\n";
    }
}

main();
