public class MalKeyword extends MalObject
{
    "keyword" => type;
    MalObject meta;

    fun string value()
    {
        return (object$String).value;
    }

    fun void init(string value)
    {
        String.create(value) @=> object;
    }

    fun static MalKeyword create(string value)
    {
        MalKeyword m;
        m.init(value);
        return m;
    }
}
