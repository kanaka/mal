public class MalIsFn extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        if( args[0].type == "subr" || ( args[0].type == "func" &&
                                        !(args[0]$Func).isMacro ) )
        {
            return Constants.TRUE;
        }
        else
        {
            return Constants.FALSE;
        }
    }
}
