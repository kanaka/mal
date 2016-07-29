public class MalDoSwap extends MalSubr
{
    // HACK: necessary for apply step
    "swap!" => name;

    fun MalObject call(MalObject args[])
    {
        args[0]$MalAtom @=> MalAtom atom;
        args[1]$MalObject @=> MalObject value;

        value @=> atom.object;

        return value;
    }
}
