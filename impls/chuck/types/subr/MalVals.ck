public class MalVals extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalHashMap).value() @=> MalObject map[];
        MalObject results[0];

        for( 1 => int i; i < map.size(); 2 +=> i )
        {
            results << map[i];
        }

        return MalList.create(results);
    }
}
