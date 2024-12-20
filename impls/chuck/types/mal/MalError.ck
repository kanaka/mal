public class MalError extends MalObject
{
    "error" => type;

    fun void init(MalObject value)
    {
        value @=> object;
    }

    fun static MalError create(string value)
    {
        MalError m;
        m.init(MalString.create(value));
        return m;
    }

    fun static MalError create(MalObject value)
    {
        MalError m;
        m.init(value);
        return m;
    }
}
