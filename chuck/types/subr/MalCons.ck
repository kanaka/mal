public class MalCons extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;
        Util.sequenceToMalObjectArray(args[1]) @=> MalObject list[];
        return MalList.create(MalObject.append([arg], list));
    }
}
