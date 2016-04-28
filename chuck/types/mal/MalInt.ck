public class MalInt extends MalObject
{
    "int" => type;
    MalObject meta;

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
}
