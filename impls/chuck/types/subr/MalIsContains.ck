public class MalIsContains extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].malObjectValues() @=> MalObject map[];
        args[1].stringValue => string keyName;

        MalObject mapKey;
        MalObject mapValue;
        false => int isKeyPresent;
        0 => int i;

        while( !isKeyPresent && i < map.size() )
        {
            map[i] @=> mapKey;

            if( keyName == mapKey.stringValue )
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
