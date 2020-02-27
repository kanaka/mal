public class MalFirst extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.type == "nil" )
        {
            return Constants.NIL;
        }

        Util.sequenceToMalObjectArray(arg) @=> MalObject list[];

        if( list.size() > 0 )
        {
            return list[0];
        }
        else
        {
            return Constants.NIL;
        }
    }
}
