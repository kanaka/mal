public class MalConj extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        Util.sequenceToMalObjectArray(args[0]) @=> MalObject list[];
        MalObject.slice(args, 1) @=> MalObject rest[];

        if( args[0].type == "list" )
        {
            return MalList.create(MalObject.append(MalObject.reverse(rest), list));
        }
        else // args[0].type == "vector"
        {
            return MalVector.create(MalObject.append(list, rest));
        }
    }
}
