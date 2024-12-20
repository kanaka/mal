public class MalDiv extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalInt.create(args[0].intValue / args[1].intValue);
    }
}
