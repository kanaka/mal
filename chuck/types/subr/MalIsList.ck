public class MalIsList extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "list" )
        {
            return MalTrue.create();
        }
        else
        {
            return MalFalse.create();
        }
    }
}
