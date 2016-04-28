public class MalTrue extends MalObject
{
    "true" => type;
    MalObject meta;

    fun void init()
    {
        Int.create(1) @=> object;
    }

    fun static MalTrue create()
    {
        MalTrue m;
        m.init();
        return m;
    }
}
