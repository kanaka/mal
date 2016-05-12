public class MalGreaterEqual extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0]$MalInt @=> MalInt a;
        args[1]$MalInt @=> MalInt b;

        if( a.value() >= b.value() )
        {
            return MalTrue.create();
        }
        else
        {
            return MalFalse.create();
        }
    }
}
