public class MalLess extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].intValue < args[1].intValue )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
