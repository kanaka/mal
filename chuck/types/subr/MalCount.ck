public class MalCount extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].type => string kind;
        if( kind == "list" || kind == "vector" )
        {
            Util.sequenceToMalObjectArray(args[0]) @=> MalObject values[];
            return MalInt.create(values.size());
        }
        else
        {
            return MalInt.create(0);
        }
    }
}
