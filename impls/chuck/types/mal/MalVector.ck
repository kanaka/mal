public class MalVector extends MalObject
{
    "vector" => type;

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

    fun MalObject clone()
    {
        MalVector value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
