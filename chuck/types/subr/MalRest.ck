public class MalRest extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;
        MalObject result[0];

        if( arg.type == "nil" )
        {
            return MalList.create(result);
        }

        Util.sequenceToMalObjectArray(args[0]) @=> MalObject list[];

        if( list.size() > 0 )
        {
            MalObject.slice(list, 1) @=> result;
        }

        return MalList.create(result);
    }
}
