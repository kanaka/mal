public class MalIsEmpty extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        MalObject.toMalObjectArray(args[0].objects) @=> MalObject values[];
        if( values.size() == 0 )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
