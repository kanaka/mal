public class MalSymbolify extends MalSubr
{
    fun MalObject call(MalObject args[])
    {
        args[0].stringValue => string name;
        return MalSymbol.create(name);
    }
}
