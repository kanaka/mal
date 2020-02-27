public class MalError extends MalObject
{
    "error" => type;

    fun MalObject value()
    {
        return object$MalObject;
    }

    fun void init(MalObject value)
    {
        value @=> object;
    }

    fun static MalError create(MalObject value)
    {
        MalError m;
        m.init(value);
        return m;
    }
}
