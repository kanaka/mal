public class MalNth extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        Util.sequenceToMalObjectArray(args[0]) @=> MalObject list[];
        (args[1]$MalInt).value() @=> int n;

        if( n < list.size() )
        {
            return list[n];
        }
        else
        {
            return MalError.create(MalString.create("out of bounds"));
        }
    }
}
