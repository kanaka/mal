public class MalString extends MalObject
{
    "string" => type;
    MalObject meta;

    fun string value()
    {
        return (object$String).value;
    }

    fun void init(string value)
    {
        String.create(value) @=> object;
    }

    fun static MalString create(string value)
    {
        MalString m;
        m.init(value);
        return m;
    }
}
