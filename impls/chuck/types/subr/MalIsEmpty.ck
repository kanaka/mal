public class MalIsEmpty extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].objects.size() == 0 )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
