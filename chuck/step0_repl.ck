// @import readline.ck

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
    int done;

    while( !done )
    {
        Readline.readline("user> ") => string input;

        if( input != null )
        {
            chout <= rep(input) + "\n";
        }
        else
        {
            true => done;
        }
    }
}

main();
