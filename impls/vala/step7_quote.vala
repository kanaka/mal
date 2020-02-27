class Mal.BuiltinFunctionEval : Mal.BuiltinFunction {
    public Mal.Env env;
    public BuiltinFunctionEval(Mal.Env env_) { env = env_; }
    public override Mal.ValWithMetadata copy() {
        return new Mal.BuiltinFunctionEval(env);
    }
    public override string name() { return "eval"; }
    public override Mal.Val call(Mal.List args) throws Mal.Error {
        if (args.vs.length() != 1)
            throw new Mal.Error.BAD_PARAMS("%s: expected one argument", name());
        return Mal.Main.EVAL(args.vs.data, env);
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
        var roote = new GC.Root(env); (void)roote;
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

    public static Mal.Val quasiquote(Mal.Val ast)
    throws Mal.Error {
        if (!is_pair(ast)) {
            var list = new GLib.List<Mal.Val>();
            list.append(new Mal.Sym("quote"));
            list.append(ast);
            return new Mal.List(list);
        }

        var iter = (ast as Mal.Listlike).iter();
        var first = iter.deref();
        if (first is Mal.Sym && (first as Mal.Sym).v == "unquote") {
            if (iter.step().empty())
                throw new Mal.Error.BAD_PARAMS(
                    "unquote: expected two values");
            return iter.deref();
        }

        if (is_pair(first)) {
            var fiter = (first as Mal.Listlike).iter();
            var ffirst = fiter.deref();
            if (ffirst is Mal.Sym &&
                (ffirst as Mal.Sym).v == "splice-unquote") {
                var list = new GLib.List<Mal.Val>();
                list.append(new Mal.Sym("concat"));
                if (fiter.step().empty())
                    throw new Mal.Error.BAD_PARAMS(
                        "unquote: expected two values");
                list.append(fiter.deref());
                var sublist = new GLib.List<Mal.Val>();
                while (!iter.step().empty())
                    sublist.append(iter.deref());
                list.append(quasiquote(new Mal.List(sublist)));
                return new Mal.List(list);
            }
        }

        var list = new GLib.List<Mal.Val>();
        list.append(new Mal.Sym("cons"));
        list.append(quasiquote(first));
        var sublist = new GLib.List<Mal.Val>();
        while (!iter.step().empty())
            sublist.append(iter.deref());
        list.append(quasiquote(new Mal.List(sublist)));
        return new Mal.List(list);
    }

    public static Mal.Val EVAL(Mal.Val ast_, Mal.Env env_)
    throws Mal.Error {
        // Copy the implicitly 'unowned' function arguments into
        // ordinary owned variables which increment the objects'
        // reference counts. This is so that when we overwrite these
        // variables within the loop (for TCO) the objects we assign
        // into them don't immediately get garbage-collected.
        Mal.Val ast = ast_;
        Mal.Env env = env_;
        var ast_root = new GC.Root(ast); (void)ast_root;
        var env_root = new GC.Root(env); (void)env_root;
        while (true) {
            ast_root.obj = ast;
            env_root.obj = env;
            GC.Core.maybe_collect();
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
                        env = new Mal.Env.within(env);

                        if (defns is Mal.List) {
                            for (unowned GLib.List<Mal.Val> iter =
                                     (defns as Mal.List).vs;
                                 iter != null; iter = iter.next.next) {
                                if (iter.next == null)
                                    throw new Mal.Error.BAD_PARAMS(
                                        "let*: expected an even-length list" +
                                        " of definitions");
                                define_eval(iter.data, iter.next.data, env);
                            }
                        } else if (defns is Mal.Vector) {
                            var vec = defns as Mal.Vector;
                            if (vec.length % 2 != 0)
                                throw new Mal.Error.BAD_PARAMS(
                                    "let*: expected an even-length vector" +
                                    " of definitions");
                            for (var i = 0; i < vec.length; i += 2)
                                define_eval(vec[i], vec[i+1], env);
                        } else {
                            throw new Mal.Error.BAD_PARAMS(
                                "let*: expected a list or vector of definitions");
                        }
                        ast = list.nth(2).data;
                        continue;      // tail-call optimisation
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
                        ast = list.data;
                        continue;      // tail-call optimisation
                    case "fn*":
                        if (list.length() != 3)
                            throw new Mal.Error.BAD_PARAMS(
                                "fn*: expected two arguments");
                        var binds = list.next.data as Mal.Listlike;
                        var body = list.next.next.data;
                        if (binds == null)
                            throw new Mal.Error.BAD_PARAMS(
                                "fn*: expected a list of parameter names");
                        for (var iter = binds.iter(); iter.nonempty();
                             iter.step())
                            if (!(iter.deref() is Mal.Sym))
                                throw new Mal.Error.BAD_PARAMS(
                                    "fn*: expected parameter name to be "+
                                    "symbol");
                        return new Mal.Function(binds, body, env);
                    case "quote":
                        if (list.length() != 2)
                            throw new Mal.Error.BAD_PARAMS(
                                "quote: expected one argument");
                        return list.next.data;
                    case "quasiquote":
                        if (list.length() != 2)
                            throw new Mal.Error.BAD_PARAMS(
                                "quasiquote: expected one argument");
                        ast = quasiquote(list.next.data);
                        continue;      // tail-call optimisation
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
                    env = new Mal.Env.funcall(fn.env, fn.parameters, newlist);
                    ast = fn.body;
                    continue;      // tail-call optimisation
                } else {
                    throw new Mal.Error.CANNOT_APPLY(
                        "bad value at start of list");
                }
            } else {
                return eval_ast(ast, env);
            }
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

    public static void setup(string line, Mal.Env env) {
        try {
            EVAL(Reader.read_str(line), env);
        } catch (Mal.Error err) {
            assert(false); // shouldn't happen
        }
    }

    public static int main(string[] args) {
        var env = new Mal.Env();
        var root = new GC.Root(env); (void)root;

        Mal.Core.make_ns();
        foreach (var key in Mal.Core.ns.get_keys())
            env.set(new Mal.Sym(key), Mal.Core.ns[key]);
        env.set(new Mal.Sym("eval"), new Mal.BuiltinFunctionEval(env));

        setup("(def! not (fn* (a) (if a false true)))", env);
        setup("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", env);

        var ARGV = new GLib.List<Mal.Val>();
        if (args.length > 1) {
            for (int i = args.length - 1; i >= 2; i--)
                ARGV.prepend(new Mal.String(args[i]));
        }
        env.set(new Mal.Sym("*ARGV*"), new Mal.List(ARGV));

        if (args.length > 1) {
            var contents = new GLib.List<Mal.Val>();
            contents.prepend(new Mal.String(args[1]));
            contents.prepend(new Mal.Sym("load-file"));
            try {
                EVAL(new Mal.List(contents), env);
            } catch (Mal.Error err) {
                GLib.stderr.printf("%s\n", err.message);
                return 1;
            }
        } else {
            while (!eof) {
                try {
                    rep(env);
                } catch (Mal.Error err) {
                    GLib.stderr.printf("%s\n", err.message);
                }
            }
        }
        return 0;
    }
}
