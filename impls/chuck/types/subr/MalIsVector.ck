public class MalIsVector extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.type == "vector" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
