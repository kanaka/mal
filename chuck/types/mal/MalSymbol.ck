public class MalSymbol extends MalObject
{
    "symbol" => type;

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

    fun MalObject clone()
    {
        MalSymbol value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
