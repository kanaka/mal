public class Env extends MalObject
{
    MalObject outer; // this would ideally be Env, but isn't supported
    MalObject data[0];

    fun void init(MalObject env)
    {
        env @=> outer;
    }

    fun static Env create(MalObject env)
    {
        Env e;
        e.init(env);
        return e;
    }

    fun void set(string key, MalObject value)
    {
        value @=> data[key];
    }

    fun MalObject find(string key)
    {
        data[key] @=> MalObject value;

        if( value != null )
        {
            return value;
        }
        else if( outer != null )
        {
            return (outer$Env).find(key);
        }
        else
        {
            return null;
        }
    }

    fun MalObject get(string key)
    {
        find(key) @=> MalObject value;

        if( value != null )
        {
            return value;
        }
        else
        {
            return MalError.create(Status.SYMBOL_NOT_FOUND, key);
        }
    }
}
