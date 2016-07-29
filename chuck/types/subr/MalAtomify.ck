public class MalAtomify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject m;
        return MalAtom.create(m);
    }
}
