public class MalConcat extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        MalObject value[0];

        for( 0 => int i; i < args.size(); i++ )
        {
            Util.sequenceToMalObjectArray(args[i]) @=> MalObject list[];
            MalObject.append(value, list) @=> value;
        }

        return MalList.create(value);
    }
}
