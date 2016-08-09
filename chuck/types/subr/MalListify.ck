public class MalListify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalList.create(args);
    }
}
