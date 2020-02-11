public class MalDissoc extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalHashMap).value() @=> MalObject map[];
        MalObject.slice(args, 1) @=> MalObject ks[];

        MalObject result[0];
        int cachedKeys[0];

        for( 0 => int i; i < ks.size(); i++ )
        {
            Util.keyName(ks[i]) => string key;
            true => cachedKeys[key];
        }

        for( 0 => int i; i < map.size(); 2 +=> i )
        {
            map[i] @=> MalObject key;
            map[i+1] @=> MalObject value;
            // HACK: using name doesn't work in a nested scope
            Util.keyName(key) => string keyName;

            if( !cachedKeys[keyName] )
            {
                result << key;
                result << value;
            }
        }

        return MalHashMap.create(result);
    }
}
