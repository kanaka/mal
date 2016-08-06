public class MalWithMeta extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;
        args[1] @=> MalObject meta;

        MalObject value;
        arg.clone() @=> value;

        meta$Object @=> value.meta;

        return value;
    }
}
