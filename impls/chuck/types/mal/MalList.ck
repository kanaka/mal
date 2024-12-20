public class MalList extends MalObject
{
    "list" => type;

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

    fun MalObject clone()
    {
        MalList value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
