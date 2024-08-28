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

        args[0].malObjectValues() @=> MalObject list[];

        if( list.size() > 0 )
        {
            MalObject.slice(list, 1) @=> result;
        }

        return MalList.create(result);
    }
}
