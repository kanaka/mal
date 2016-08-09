public class MalSubr extends MalObject
{
    "subr" => type;
    string name;
    // HACK
    MalObject eval;

    fun MalObject call(MalObject args[])
    {
        return new MalObject;
    }

    fun MalObject apply(MalObject f, MalObject args[])
    {
        return new MalObject;
    }
}
