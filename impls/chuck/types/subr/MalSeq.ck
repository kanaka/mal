public class MalSeq extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;

        if( arg.type == "nil" )
        {
            return Constants.NIL;
        }
        else if( arg.type == "list" || arg.type == "vector" )
        {
            Util.sequenceToMalObjectArray(args[0]) @=> MalObject list[];

            if( list.size() > 0 )
            {
                return MalList.create(list);
            }
            else
            {
                return Constants.NIL;
            }
        }
        else if( arg.type == "string" )
        {
            (args[0]$MalString).value() => string value;

            if( value.length() > 0 )
            {
                MalObject chars[value.length()];

                for( 0 => int i; i < value.length(); i++ )
                {
                    MalString.create(value.substring(i, 1)) @=> chars[i];
                }

                return MalList.create(chars);
            }
            else
            {
                return Constants.NIL;
            }
        }
    }
}
