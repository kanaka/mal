public class MalFalse extends MalObject
{
    "false" => type;

    fun void init()
    {
        0 => intValue;
    }

    fun static MalFalse create()
    {
        MalFalse m;
        m.init();
        return m;
    }

    fun MalObject clone()
    {
        MalFalse value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
