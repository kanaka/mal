public class MalHashMapify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalHashMap.create(args);
    }
}
