public class MalPrintln extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        string values[args.size()];

        for( 0 => int i; i < values.size(); i++ )
        {
            Printer.pr_str(args[i], false) => values[i];
        }

        Util.println(String.join(values, " "));
        return Constants.NIL;
    }
}
