public class MalPrn extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        string values[args.size()];

        for( 0 => int i; i < values.size(); i++ )
        {
            Printer.pr_str(args[i], true) => values[i];
        }

        Util.println(String.join(values, " "));
        return Constants.NIL;
    }
}
