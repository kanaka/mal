public class MalReadStr extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalString).value() => string input;
        return Reader.read_str(input);
    }
}
