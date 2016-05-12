public class MalGreaterEqual extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0]$MalInt @=> MalInt a;
        args[1]$MalInt @=> MalInt b;

        if( a.value() >= b.value() )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
