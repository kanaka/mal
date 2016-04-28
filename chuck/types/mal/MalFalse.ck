public class MalFalse extends MalObject
{
    "false" => type;
    MalObject meta;

    fun void init()
    {
        Int.create(0) @=> object;
    }

    fun static MalFalse create()
    {
        MalFalse m;
        m.init();
        return m;
    }
}
