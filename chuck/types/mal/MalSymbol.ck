public class MalSymbol extends MalObject
{
    "symbol" => type;
    MalObject meta;

    fun string value()
    {
        return (object$String).value;
    }

    fun void init(string value)
    {
        String.create(value) @=> object;
    }

    fun static MalSymbol create(string value)
    {
        MalSymbol m;
        m.init(value);
        return m;
    }
}
