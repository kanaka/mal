public class MalIsKeyword extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.type == "keyword" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
