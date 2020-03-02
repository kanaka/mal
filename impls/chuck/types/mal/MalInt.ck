public class MalInt extends MalObject
{
    "int" => type;

    fun int value()
    {
        return (object$Int).value;
    }

    fun void init(int value)
    {
        Int.create(value) @=> object;
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
