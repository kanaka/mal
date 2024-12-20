public class MalTrue extends MalObject
{
    "true" => type;

    fun void init()
    {
        1 => intValue;
    }

    fun static MalTrue create()
    {
        MalTrue m;
        m.init();
        return m;
    }

    fun MalObject clone()
    {
        MalTrue value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
