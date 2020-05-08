public class MalDeref extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return (args[0]$MalAtom).value();
    }
}
