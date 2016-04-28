public class MalError extends MalObject
{
    "error" => type;
    string data;

    fun int value()
    {
        return (object$Int).value;
    }

    fun void init(int value)
    {
        Int.create(value) @=> object;
    }

    fun void init(int value, string arg)
    {
        Int.create(value) @=> object;
        arg => data;
    }

    fun static MalError create(int value)
    {
        MalError m;
        m.init(value);
        return m;
    }

    fun static MalError create(int value, string data)
    {
        MalError m;
        m.init(value, data);
        return m;
    }
}
