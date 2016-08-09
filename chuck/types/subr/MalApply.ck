public class MalApply extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject f;
        MalObject.slice(args, 1, args.size()-1) @=> MalObject _args[];
        (args[args.size()-1]$MalList).value() @=> MalObject rest[];

        MalObject.append(_args, rest) @=> _args;
        return (eval$MalSubr).apply(f, _args);
    }
}
