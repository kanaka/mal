public class MalList extends MalObject
{
    "list" => type;
    MalObject meta;

    fun MalObject[] value()
    {
        return MalObject.toMalObjectArray(objects);
    }

    fun void init(MalObject values[])
    {
        MalObject.toObjectArray(values) @=> objects;
    }

    fun static MalList create(MalObject values[])
    {
        MalList m;
        m.init(values);
        return m;
    }
}
