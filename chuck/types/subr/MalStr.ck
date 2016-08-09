public class MalStr extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        string values[args.size()];

        for( 0 => int i; i < values.size(); i++ )
        {
            Printer.pr_str(args[i], false) => values[i];
        }

        return MalString.create(String.join(values, ""));
    }
}
