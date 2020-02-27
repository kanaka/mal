public class MalIsHashMap extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.type == "hashmap" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
