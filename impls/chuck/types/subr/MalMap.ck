public class MalMap extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject f;
        Util.sequenceToMalObjectArray(args[1]) @=> MalObject list[];

        for( 0 => int i; i < list.size(); i++ )
        {
            (eval$MalSubr).apply(f, [list[i]]) @=> MalObject value;

            if( value.type == "error" )
            {
                return value;
            }

            value @=> list[i];
        }

        return MalList.create(list);
    }
}
