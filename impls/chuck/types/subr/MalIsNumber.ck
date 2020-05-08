public class MalIsNumber extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "int" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
