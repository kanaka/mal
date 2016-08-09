public class MalMeta extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.meta == null )
        {
            return Constants.NIL;
        }
        else
        {
            return (arg.meta)$MalObject;
        }
    }
}
