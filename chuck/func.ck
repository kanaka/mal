public class Func extends MalObject
{
    "func" => type;
    Env env;
    string args[];
    MalObject ast;
    int isMacro;

    fun void init(Env env, string args[], MalObject ast)
    {
        env @=> this.env;
        args @=> this.args;
        ast @=> this.ast;
    }

    fun static Func create(Env env, string args[], MalObject ast)
    {
        Func func;
        func.init(env, args, ast);
        return func;
    }

    fun MalObject clone()
    {
        Func value;

        this.type => value.type;
        this.env @=> value.env;
        this.args @=> value.args;
        this.ast @=> value.ast;
        this.isMacro @=> value.isMacro;

        return value;
    }
}
