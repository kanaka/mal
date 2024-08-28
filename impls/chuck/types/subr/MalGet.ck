public class MalGet extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "nil" )
        {
            return Constants.NIL;
        }

        args[0].malObjectValues() @=> MalObject map[];
        args[1].stringValue => string keyName;

        MalObject mapKey;
        MalObject mapValue;
        false => int isKeyPresent;
        0 => int i;

        while( !isKeyPresent && i < map.size() )
        {
            map[i] @=> mapKey;
            map[i+1] @=> mapValue;

            if( keyName == mapKey.stringValue )
            {
                true => isKeyPresent;
            }

            2 +=> i;
        }

        if( isKeyPresent )
        {
            return mapValue;
        }
        else
        {
            return Constants.NIL;
        }
    }
}
