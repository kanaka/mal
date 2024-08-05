import types;

class Env {
    Env outer;
    MalType[string] data;

    this(Env outer_v, MalType[] binds = [], MalType[] exprs = [])
    {
        outer = outer_v;
        foreach (i, MalType b; binds)
        {
            auto arg_name = verify_cast!MalSymbol(b);
            if (arg_name.name == "&")
            {
                auto rest_arg_name = verify_cast!MalSymbol(binds[i + 1]);
                auto rest_exprs = new MalList(exprs[i..$]);
                set(rest_arg_name.name, rest_exprs);
                break;
            }
            else
            {
                set(arg_name.name, exprs[i]);
            }
        }
    }

    MalType set(string key, MalType val)
    {
        data[key] = val;
        return val;
    }

    MalType get(string key)
    {
        auto val = (key in data);
        if (val !is null) {
            return data[key];
        } else if (outer is null) {
            return null;
        } else {
            return outer.get(key);
        }
    }
}
