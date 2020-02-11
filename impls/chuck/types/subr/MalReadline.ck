public class MalReadline extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalString).value() => string prompt;
        Readline.readline(prompt) => string input;

        if( input == null )
        {
            return Constants.NIL;
        }
        else
        {
            return MalString.create(input);
        }
    }
}
