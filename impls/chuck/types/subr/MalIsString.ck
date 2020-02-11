public class MalIsString extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "string" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
