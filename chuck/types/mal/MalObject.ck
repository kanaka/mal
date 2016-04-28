public class MalObject
{
    string type;
    Object object;
    Object objects[];
    // no meta here because types can't be self-referential

    // helpers for sequence types
    fun static MalObject[] toMalObjectArray(Object objects[])
    {
        MalObject values[objects.size()];

        for( 0 => int i; i < objects.size(); i++ )
        {
            objects[i]$MalObject @=> values[i];
        }

        return values;
    }

    fun static Object[] toObjectArray(MalObject objects[])
    {
        Object values[objects.size()];

        for( 0 => int i; i < objects.size(); i++ )
        {
            objects[i]$Object @=> values[i];
        }

        return values;
    }
}
