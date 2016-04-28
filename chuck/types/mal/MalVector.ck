public class MalVector extends MalObject
{
    "vector" => type;
    MalObject meta;

    fun MalObject[] value()
    {
        return MalObject.toMalObjectArray(objects);
    }

    fun void init(MalObject values[])
    {
        MalObject.toObjectArray(values) @=> objects;
    }

    fun static MalVector create(MalObject values[])
    {
        MalVector m;
        m.init(values);
        return m;
    }
}
