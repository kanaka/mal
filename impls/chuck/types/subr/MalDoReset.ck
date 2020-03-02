public class MalDoReset extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0]$MalAtom @=> MalAtom atom;
        args[1]$MalObject @=> MalObject value;

        value @=> atom.object;

        return value;
    }
}
