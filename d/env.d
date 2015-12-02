import types;

class Env {
    Env outer;
    MalType[MalSymbol] data;

    this(Env outer_v, MalType[] binds = [], MalType[] exprs = [])
    {
        outer = outer_v;
        foreach (int i, MalType b; binds)
        {
            auto arg_name = verify_cast!MalSymbol(b);
            if (arg_name.name == "&")
            {
                auto rest_arg_name = verify_cast!MalSymbol(binds[i + 1]);
                auto rest_exprs = new MalList(exprs[i..$]);
                set(rest_arg_name, rest_exprs);
                break;
            }
            else
            {
                set(arg_name, exprs[i]);
            }
        }
    }

    MalType set(MalSymbol key, MalType val)
    {
        data[key] = val;
        return val;
    }

    Env find(MalSymbol key)
    {
        auto val = (key in data);
        if (val !is null) {
            return this;
        } else if (outer is null) {
            return null;
        } else {
            return outer.find(key);
        }
    }

    MalType get(MalSymbol key)
    {
        auto found = find(key);
        if (found is null) {
            throw new Exception("'" ~ key.print(true) ~ "' not found");
        }
        return found.data[key];
    }
}
