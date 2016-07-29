public class MalSubr extends MalObject
{
    "subr" => type;
    string name;
    Env env;

    fun MalObject call(MalObject args[])
    {
        return new MalObject;
    }

    // HACK: necessary for providing eval with repl_env
    fun static MalSubr create(string name, Env env)
    {
        MalSubr subr;
        name => subr.name;
        env @=> subr.env;
        return subr;
    }
}
