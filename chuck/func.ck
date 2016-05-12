public class Func extends MalObject
{
    "func" => type;
    Env env;
    string args[];
    MalObject ast;

    fun void init(Env _env, string _args[], MalObject _ast)
    {
        _env @=> env;
        _args @=> args;
        _ast @=> ast;
    }

    fun static Func create(Env _env, string _args[], MalObject _ast)
    {
        Func func;
        func.init(_env, _args, _ast);
        return func;
    }
}
