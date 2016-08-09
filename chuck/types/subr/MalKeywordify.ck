public class MalKeywordify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalString).value() => string name;
        return MalKeyword.create(name);
    }
}
