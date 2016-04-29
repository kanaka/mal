public class MalSubr extends MalObject
{
    "subr" => type;

    fun MalObject call(MalObject args[])
    {
        return new MalObject;
    }
}
