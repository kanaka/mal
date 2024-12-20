public class MalReadStr extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].stringValue => string input;
        return Reader.read_str(input);
    }
}
