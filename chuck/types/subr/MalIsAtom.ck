public class MalIsAtom extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "atom" )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
