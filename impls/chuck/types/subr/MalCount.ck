public class MalCount extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].type => string kind;
        if( kind == "list" || kind == "vector" )
        {
            return MalInt.create(args[0].objects.size());
        }
        else
        {
            return MalInt.create(0);
        }
    }
}
