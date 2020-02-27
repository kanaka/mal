public class MalAdd extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0]$MalInt @=> MalInt a;
        args[1]$MalInt @=> MalInt b;

        return MalInt.create(a.value() + b.value());
    }
}
