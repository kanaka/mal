public class MalAtom extends MalObject
{
    "atom" => type;
    MalObject meta;

    fun MalObject value()
    {
        return object$MalObject;
    }

    fun void init(MalObject value)
    {
        value @=> object;
    }

    fun static MalObject create(MalObject value)
    {
        MalAtom m;
        m.init(value);
        return m;
    }
}
