public class MalInt extends MalObject
{
    "int" => type;

    fun void init(int value)
    {
        value => intValue;
    }

    fun static MalInt create(int value)
    {
        MalInt m;
        m.init(value);
        return m;
    }

    fun MalObject clone()
    {
        MalInt value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
