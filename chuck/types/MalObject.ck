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

    fun static MalObject[] slice(MalObject objects[], int index)
    {
        MalObject values[objects.size() - index];

        for( index => int i; i < objects.size(); i++ )
        {
            objects[i] @=> values[i - index];
        }

        return values;
    }

    fun static MalObject[] slice(MalObject objects[], int from, int to)
    {
        MalObject values[0];

        for( from => int i; i < to; i++ )
        {
            values << objects[i];
        }

        return values;
    }

    fun static MalObject[] append(MalObject as[], MalObject bs[])
    {
        MalObject output[as.size()+bs.size()];

        for( 0 => int i; i < as.size(); i++ )
        {
            as[i] @=> output[i];
        }

        for( 0 => int i; i < bs.size(); i++ )
        {
            bs[i] @=> output[as.size()+i];
        }

        return output;
    }
}
