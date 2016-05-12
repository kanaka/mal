public class MalIsEmpty extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        (args[0]$MalList).value() @=> MalObject values[];
        if( values.size() == 0 )
        {
            return MalTrue.create();
        }
        else
        {
            return MalFalse.create();
        }
    }
}
