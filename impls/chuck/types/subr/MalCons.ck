public class MalCons extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0] @=> MalObject arg;
        args[1].malObjectValues() @=> MalObject list[];
        return MalList.create(MalObject.append([arg], list));
    }
}
