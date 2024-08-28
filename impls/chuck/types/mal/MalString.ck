public class MalString extends MalObject
{
    "string" => type;

    fun void init(string value)
    {
       value => stringValue;
    }

    fun static MalString create(string value)
    {
        MalString m;
        m.init(value);
        return m;
    }

    fun MalObject clone()
    {
        MalString value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
