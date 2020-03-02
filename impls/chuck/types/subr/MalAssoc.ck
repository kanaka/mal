public class MalAssoc extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalHashMap).value() @=> MalObject map[];
        MalObject.slice(args, 1) @=> MalObject kvs[];

        MalObject result[0];
        MalObject cachedKeys[0];
        MalObject cachedValues[0];
        string keys[0];

        for( 0 => int i; i < map.size(); 2 +=> i )
        {
            Util.keyName(map[i]) => string key;

            keys << key;

            map[i] @=> cachedKeys[key];
            map[i+1] @=> cachedValues[key];
        }

        for( 0 => int i; i < kvs.size(); 2 +=> i )
        {
            Util.keyName(kvs[i]) => string key;

            if( cachedValues[key] == null )
            {
                keys << key;
            }

            kvs[i] @=> cachedKeys[key];
            kvs[i+1] @=> cachedValues[key];
        }

        for( 0 => int i; i < keys.size(); i++ )
        {
            keys[i] => string key;
            result << cachedKeys[key];
            result << cachedValues[key];
        }

        return MalHashMap.create(result);
    }
}
