public class MalIsContains extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalHashMap).value() @=> MalObject map[];
        Util.keyName(args[1]) => string keyName;

        MalObject mapKey;
        MalObject mapValue;
        false => int isKeyPresent;
        0 => int i;

        while( !isKeyPresent && i < map.size() )
        {
            map[i] @=> mapKey;
            Util.keyName(mapKey) => string mapKeyName;

            if( keyName == mapKeyName )
            {
                true => isKeyPresent;
            }

            2 +=> i;
        }

        if( isKeyPresent )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
