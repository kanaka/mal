public class MalIsList extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "list" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
