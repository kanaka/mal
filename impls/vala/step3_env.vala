abstract class Mal.BuiltinFunctionDyadicArithmetic : Mal.BuiltinFunction {
    public abstract int64 result(int64 a, int64 b);
    public override Mal.Val call(Mal.List args) throws Mal.Error {
        if (args.vs.length() != 2)
            throw new Mal.Error.BAD_PARAMS("%s: expected two numbers", name());
        unowned Mal.Num a = args.vs.nth_data(0) as Mal.Num;
        unowned Mal.Num b = args.vs.nth_data(1) as Mal.Num;
        if (a == null || b == null)
            throw new Mal.Error.BAD_PARAMS("%s: expected two numbers", name());
        return new Mal.Num(result(a.v, b.v));
    }
}

class Mal.BuiltinFunctionAdd : Mal.BuiltinFunctionDyadicArithmetic {
    public override Mal.ValWithMetadata copy() {
        return new Mal.BuiltinFunctionAdd();
    }
    public override string name() { return "+"; }
    public override int64 result(int64 a, int64 b) { return a+b; }
}

class Mal.BuiltinFunctionSub : Mal.BuiltinFunctionDyadicArithmetic {
    public override Mal.ValWithMetadata copy() {
        return new Mal.BuiltinFunctionSub();
    }
    public override string name() { return "-"; }
    public override int64 result(int64 a, int64 b) { return a-b; }
}

class Mal.BuiltinFunctionMul : Mal.BuiltinFunctionDyadicArithmetic {
    public override Mal.ValWithMetadata copy() {
        return new Mal.BuiltinFunctionMul();
    }
    public override string name() { return "*"; }
    public override int64 result(int64 a, int64 b) { return a*b; }
}

class Mal.BuiltinFunctionDiv : Mal.BuiltinFunctionDyadicArithmetic {
    public override Mal.ValWithMetadata copy() {
        return new Mal.BuiltinFunctionDiv();
    }
    public override string name() { return "/"; }
    public override int64 result(int64 a, int64 b) { return a/b; }
}

class Mal.Main : GLib.Object {
    static bool eof;
    static Mal.Sym dbgevalsym;

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

    private static Mal.Val define_eval(Mal.Val key, Mal.Val value,
                                       Mal.Env env)
    throws Mal.Error {
        var rootk = new GC.Root(key); (void)rootk;
        var roote = new GC.Root(env); (void)roote;
        var symkey = key as Mal.Sym;
        if (symkey == null)
            throw new Mal.Error.BAD_PARAMS(
                "let*: expected a symbol to define");
        var val = EVAL(value, env);
        env.set(symkey, val);
        return val;
    }

    public static Mal.Val EVAL(Mal.Val ast, Mal.Env env)
    throws Mal.Error {
        var ast_root = new GC.Root(ast); (void)ast_root;
        var env_root = new GC.Root(env); (void)env_root;
        GC.Core.maybe_collect();

        if (dbgevalsym == null)
            dbgevalsym = new Mal.Sym("DEBUG-EVAL");
        var dbgeval = env.get(dbgevalsym);
        if (dbgeval != null && dbgeval.truth_value())
            stdout.printf("EVAL: %s\n", pr_str(ast));

        if (ast is Mal.Sym) {
            var key = ast as Mal.Sym;
            var val = env.get(key);
            if (val == null)
                throw new Error.ENV_LOOKUP_FAILED("'%s' not found", key.v);
            return val;
        }
        if (ast is Mal.Vector) {
            var vec = ast as Mal.Vector;
            var result = new Mal.Vector.with_size(vec.length);
            var root = new GC.Root(result); (void)root;
            for (var i = 0; i < vec.length; i++)
                result[i] = EVAL(vec[i], env);
            return result;
        }
        if (ast is Mal.Hashmap) {
            var result = new Mal.Hashmap();
            var root = new GC.Root(result); (void)root;
            var map = (ast as Mal.Hashmap).vs;
            foreach (var key in map.get_keys())
                result.insert(key, EVAL(map[key], env));
            return result;
        }
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
                                       env);
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
                            define_eval(iter.data, iter.next.data, newenv);
                        }
                    } else if (defns is Mal.Vector) {
                        var vec = defns as Mal.Vector;
                        if (vec.length % 2 != 0)
                            throw new Mal.Error.BAD_PARAMS(
                                "let*: expected an even-length vector" +
                                " of definitions");
                        for (var i = 0; i < vec.length; i += 2)
                            define_eval(vec[i], vec[i+1], newenv);
                    } else {
                        throw new Mal.Error.BAD_PARAMS(
                            "let*: expected a list or vector of definitions");
                    }
                    return EVAL(list.nth(2).data, newenv);
                }
            }

            Mal.Val firstdata = EVAL(list.first().data, env);
            var newlist = new Mal.List.empty();
            var root = new GC.Root(newlist); (void)root;
            for (var iter = (ast as Mal.Listlike).iter().step(); iter.nonempty(); iter.step())
                newlist.vs.append(EVAL(iter.deref(), env));

            if (firstdata is Mal.BuiltinFunction) {
                return (firstdata as Mal.BuiltinFunction).call(newlist);
            } else {
                throw new Mal.Error.CANNOT_APPLY(
                    "bad value at start of list");
            }
        } else {
            return ast;
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
        var root = new GC.Root(env); (void)root;

        env.set(new Mal.Sym("+"), new BuiltinFunctionAdd());
        env.set(new Mal.Sym("-"), new BuiltinFunctionSub());
        env.set(new Mal.Sym("*"), new BuiltinFunctionMul());
        env.set(new Mal.Sym("/"), new BuiltinFunctionDiv());

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
