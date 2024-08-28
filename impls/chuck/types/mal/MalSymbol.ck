public class MalSymbol extends MalObject
{
    "symbol" => type;

    fun void init(string value)
    {
        value => stringValue;
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
