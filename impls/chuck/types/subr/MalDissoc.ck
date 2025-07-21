public class MalDissoc extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].malObjectValues() @=> MalObject map[];
        MalObject.slice(args, 1) @=> MalObject ks[];

        MalObject result[0];
        string cachedKeys[0];

        for( 0 => int i; i < ks.size(); i++ )
        {
            ks[i].type => cachedKeys[ks[i].stringValue];
        }

        for( 0 => int i; i < map.size(); 2 +=> i )
        {
            map[i] @=> MalObject key;
            map[i+1] @=> MalObject value;

            if (   cachedKeys[key.stringValue] == null
                || cachedKeys[key.stringValue] != key.type )
            {
                result << key;
                result << value;
            }
        }

        return MalHashMap.create(result);
    }
}
