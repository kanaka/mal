public class MalThrow extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalError.create(args[0]);
    }
}
