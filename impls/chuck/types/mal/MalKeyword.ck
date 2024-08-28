public class MalKeyword extends MalObject
{
    "keyword" => type;

    fun void init(string value)
    {
        value => stringValue;
    }

    fun static MalKeyword create(string value)
    {
        MalKeyword m;
        m.init(value);
        return m;
    }

    fun MalObject clone()
    {
        MalKeyword value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
