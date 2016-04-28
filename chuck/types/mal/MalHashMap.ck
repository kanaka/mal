public class MalHashMap extends MalObject
{
    "hashmap" => type;
    MalObject meta;

    fun MalObject[] value()
    {
        return MalObject.toMalObjectArray(objects);
    }

    fun void init(MalObject values[])
    {
        MalObject.toObjectArray(values) @=> objects;
    }

    fun static MalHashMap create(MalObject values[])
    {
        MalHashMap m;
        m.init(values);
        return m;
    }
}
