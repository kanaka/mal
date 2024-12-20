public class MalKeywordify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        return MalKeyword.create(args[0].stringValue);
    }
}
