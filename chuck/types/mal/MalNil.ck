public class MalNil extends MalObject
{
    "nil" => type;
    MalObject meta;

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
}
