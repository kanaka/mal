public class MalError extends MalObject
{
    "error" => type;

    fun void init(string value)
    {
        value => stringValue;
    }

    fun static MalError create(string value)
    {
        MalError m;
        m.init(value);
        return m;
    }
}
