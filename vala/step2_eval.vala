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

class Mal.Env : GLib.Object {
    public GLib.HashTable<Mal.Sym, Mal.Val> data;
    construct {
        data = new GLib.HashTable<Mal.Sym, Mal.Val>(
            Mal.Hashable.hash, Mal.Hashable.equal);
    }
    // Use the 'new' keyword to silence warnings about 'set' and 'get'
    // already having meanings that we're overwriting
    public new void set(Mal.Sym key, Mal.Val f) {
        data[key] = f;
    }
    public new Mal.Val get(Mal.Sym key) throws Mal.Error {
        var toret = data[key];
        if (toret == null)
            throw new Error.ENV_LOOKUP_FAILED("no such variable '%s'", key.v);
        return toret;
    }
}

class Mal.Main : GLib.Object {
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
        var roota = new GC.Root(ast); (void)roota;
        if (ast is Mal.Sym)
            return env.get(ast as Mal.Sym);
        if (ast is Mal.List) {
            var result = new Mal.List.empty();
            var root = new GC.Root(result); (void)root;
            foreach (var elt in (ast as Mal.List).vs)
                result.vs.append(EVAL(elt, env));
            return result;
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
        return ast;
    }

    public static Mal.Val EVAL(Mal.Val ast, Mal.Env env)
    throws Mal.Error {
        var ast_root = new GC.Root(ast); (void)ast_root;
        GC.Core.maybe_collect();

        if (ast is Mal.List) {
            unowned GLib.List<Mal.Val> list = (ast as Mal.List).vs;
            if (list.first() == null)
                return ast;
            var newlist = eval_ast(ast, env) as Mal.List;
            unowned GLib.List<Mal.Val> firstlink = newlist.vs.first();
            var fn = firstlink.data as Mal.BuiltinFunction;
            newlist.vs.remove_link(firstlink);
            return fn.call(newlist);
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
