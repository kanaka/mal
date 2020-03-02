public class Env extends MalObject
{
    MalObject outer; // this would ideally be Env, but isn't supported
    MalObject data[0];

    fun void init(MalObject env)
    {
        env @=> outer;
    }

    fun void init(MalObject env, string binds[], MalObject exprs[])
    {
        env @=> outer;

        for( 0 => int i; i < binds.size(); i++ )
        {
            binds[i] => string bind;

            if( bind == "&" )
            {
                MalObject.slice(exprs, i) @=> MalObject rest_binds[];
                MalList.create(rest_binds) @=> data[binds[i+1]];
                break;
            }
            else
            {
                exprs[i] @=> data[bind];
            }
        }
    }

    fun static Env create(MalObject env)
    {
        Env e;
        e.init(env);
        return e;
    }

    fun static Env create(MalObject env, string binds[], MalObject exprs[])
    {
        Env e;
        e.init(env, binds, exprs);
        return e;
    }

    fun MalObject clone()
    {
        Env value;

        this.outer @=> value.outer;
        this.data @=> value.data;

        return value;
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
            return MalError.create(MalString.create("'" + key + "' not found"));
        }
    }
}
