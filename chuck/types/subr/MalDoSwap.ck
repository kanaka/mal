public class MalDoSwap extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0]$MalAtom @=> MalAtom atom;
        atom.value() @=> MalObject value;
        args[1] @=> MalObject f;
        MalObject.slice(args, 2) @=> MalObject _args[];
        MalObject.append([value], _args) @=> _args;

        (eval$MalSubr).apply(f, _args) @=> value;
        value @=> atom.object;
        return value;
    }
}
