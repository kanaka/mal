public class MalVectorify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalVector.create(args);
    }
}
