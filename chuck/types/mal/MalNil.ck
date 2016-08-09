public class MalNil extends MalObject
{
    "nil" => type;

    fun void init()
    {
        Int.create(-1) @=> object;
    }

    fun static MalNil create()
    {
        MalNil m;
        m.init();
        return m;
    }

    fun MalObject clone()
    {
        MalNil value;

        this.type => value.type;
        this.object @=> value.object;
        this.objects @=> value.objects;
        this.meta @=> value.meta;

        return value;
    }
}
