public class MalNth extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].malObjectValues() @=> MalObject list[];
        args[1].intValue => int n;

        if( n < list.size() )
        {
            return list[n];
        }
        else
        {
            return MalError.create("out of bounds");
        }
    }
}
