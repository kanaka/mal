public class MalKeywordify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0].object $ String).value => string name;
        return MalKeyword.create(name);
    }
}
