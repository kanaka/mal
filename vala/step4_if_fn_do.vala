class Mal.Main: GLib.Object {
    static bool eof;

    static construct {
        eof = false;
    }

    public static Mal.Val? READ() {
        string? line = Readline.readline("user> ");
        if (line != null) {
            if (line.length > 0)
                Readline.History.add(line);

            try {
                return Reader.read_str(line);
            } catch (Mal.Error err) {
                GLib.stderr.printf("%s\n", err.message);
                return null;
            }
        } else {
            stdout.printf("\n");
            eof = true;
            return null;
        }
    }

    public static Mal.Val eval_ast(Mal.Val ast, Mal.Env env)
    throws Mal.Error {
        if (ast is Mal.Sym)
            return env.get(ast as Mal.Sym);
        if (ast is Mal.List) {
            var results = new GLib.List<Mal.Val>();
            foreach (var elt in (ast as Mal.List).vs)
                results.append(EVAL(elt, env));
            return new Mal.List(results);
        }
        if (ast is Mal.Vector) {
            var results = new GLib.List<Mal.Val>();
            foreach (var elt in (ast as Mal.Vector).vs)
                results.append(EVAL(elt, env));
            return new Mal.Vector.from_list(results);
        }
        if (ast is Mal.Hashmap) {
            var result = new Mal.Hashmap();
            var map = (ast as Mal.Hashmap).vs;
            foreach (var key in map.get_keys())
                result.insert(key, EVAL(map[key], env));
            return result;
        }
        return ast;
    }

    private static Mal.Val define_eval(Mal.Val key, Mal.Val value,
                                       Mal.Env eval_env, Mal.Env def_env)
    throws Mal.Error {
        var symkey = key as Mal.Sym;
        if (symkey == null)
            throw new Mal.Error.BAD_PARAMS(
                "let*: expected a symbol to define");
        var val = EVAL(value, eval_env);
        def_env.set(symkey, val);
        return val;
    }

    public static Mal.Val EVAL(Mal.Val ast, Mal.Env env)
    throws Mal.Error {
        if (ast is Mal.List) {
            unowned GLib.List<Mal.Val> list = (ast as Mal.List).vs;
            if (list.first() == null)
                return ast;

            var first = list.first().data;
            if (first is Mal.Sym) {
                var sym = first as Mal.Sym;
                switch (sym.v) {
                case "def!":
                    if (list.length() != 3)
                        throw new Mal.Error.BAD_PARAMS(
                            "def!: expected two values");
                    return define_eval(list.next.data, list.next.next.data,
                                       env, env);
                case "let*":
                    if (list.length() != 3)
                        throw new Mal.Error.BAD_PARAMS(
                            "let*: expected two values");
                    var defns = list.nth(1).data;
                    var newenv = new Mal.Env.within(env);

                    if (defns is Mal.List) {
                        for (unowned GLib.List<Mal.Val> iter =
                                 (defns as Mal.List).vs;
                             iter != null; iter = iter.next.next) {
                            if (iter.next == null)
                                throw new Mal.Error.BAD_PARAMS(
                                    "let*: expected an even-length list" +
                                    " of definitions");
                            define_eval(iter.data, iter.next.data,
                                        newenv, newenv);
                        }
                    } else if (defns is Mal.Vector) {
                        var vec = (defns as Mal.Vector).vs;
                        if (vec.length % 2 != 0)
                            throw new Mal.Error.BAD_PARAMS(
                                "let*: expected an even-length vector" +
                                " of definitions");
                        for (var i = 0; i < vec.length; i += 2)
                            define_eval(vec[i], vec[i+1], newenv, newenv);
                    } else {
                        throw new Mal.Error.BAD_PARAMS(
                            "let*: expected a list or vector of definitions");
                    }
                    return EVAL(list.nth(2).data, newenv);
                case "do":
                    Mal.Val result = null;
                    for (list = list.next; list != null; list = list.next)
                        result = EVAL(list.data, env);
                    if (result == null)
                        throw new Mal.Error.BAD_PARAMS(
                            "do: expected at least one argument");
                    return result;
                case "if":
                    if (list.length() != 3 && list.length() != 4)
                        throw new Mal.Error.BAD_PARAMS(
                            "if: expected two or three arguments");
                    list = list.next;
                    var cond = EVAL(list.data, env);
                    list = list.next;
                    if (!cond.truth_value()) {
                        // Skip to the else clause, which defaults to nil.
                        list = list.next;
                        if (list == null)
                            return new Mal.Nil();
                    }
                    return EVAL(list.data, env);
                case "fn*":
                    if (list.length() != 3)
                        throw new Mal.Error.BAD_PARAMS(
                            "fn*: expected two arguments");
                    var binds = list.next.data as Mal.Listlike;
                    var body = list.next.next.data;
                    if (binds == null)
                        throw new Mal.Error.BAD_PARAMS(
                            "fn*: expected a list of parameter names");
                    for (var iter = binds.iter(); iter.nonempty(); iter.step())
                        if (!(iter.deref() is Mal.Sym))
                            throw new Mal.Error.BAD_PARAMS(
                                "fn*: expected parameter name to be "+
                                "symbol");
                    return new Mal.Function(binds, body, env);
                }
            }

            var newlist = eval_ast(ast, env) as Mal.List;
            unowned GLib.List<Mal.Val> firstlink = newlist.vs.first();
            Mal.Val firstdata = firstlink.data;
            newlist.vs.remove_link(firstlink);

            if (firstdata is Mal.BuiltinFunction) {
                return (firstdata as Mal.BuiltinFunction).call(newlist);
            } else if (firstdata is Mal.Function) {
                var fn = firstdata as Mal.Function;
                var newenv = new Mal.Env.funcall(
                    fn.env, fn.parameters, newlist);
                return EVAL(fn.body, newenv);
            } else {
                throw new Mal.Error.CANNOT_APPLY(
                    "bad value at start of list");
            }
        } else {
            return eval_ast(ast, env);
        }
    }

    public static void PRINT(Mal.Val value) {
        stdout.printf("%s\n", pr_str(value));
    }

    public static void rep(Mal.Env env) throws Mal.Error {
        Mal.Val? val = READ();
        if (val != null) {
            val = EVAL(val, env);
            PRINT(val);
        }
    }

    public static int main(string[] args) {
        var env = new Mal.Env();

        Mal.Core.make_ns();
        foreach (var key in Mal.Core.ns.get_keys())
            env.set(new Mal.Sym(key), Mal.Core.ns[key]);

        try {
            EVAL(Mal.Reader.read_str("(def! not (fn* (a) (if a false true)))"),
                 env);
        } catch (Mal.Error err) {
            assert(false); // shouldn't happen
        }

        while (!eof) {
            try {
                rep(env);
            } catch (Mal.Error err) {
                GLib.stderr.printf("%s\n", err.message);
            }
        }
        return 0;
    }
}
