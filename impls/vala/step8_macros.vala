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
                                       Mal.Env env,
                                       bool is_macro = false)
    throws Mal.Error {
        var rootk = new GC.Root(key); (void)rootk;
        var roote = new GC.Root(env); (void)roote;
        var symkey = key as Mal.Sym;
        if (symkey == null)
            throw new Mal.Error.BAD_PARAMS(
                "let*: expected a symbol to define");
        var val = EVAL(value, env);
        if (val is Mal.Function)
            (val as Mal.Function).is_macro = is_macro;
        env.set(symkey, val);
        return val;
    }

    //  If ast is (sym x), return x, else return null.
    public static Mal.Val? unquoted (Mal.Val ast,
                                     string sym)
    throws Mal.Error {
        var list = ast as Mal.List;
        if (list == null || list.vs == null) return null;
        var a0 = list.vs.data as Mal.Sym;
        if (a0 == null || a0.v != sym) return null;
        if (list.vs.next == null || list.vs.next.next != null)
            throw new Mal.Error.BAD_PARAMS(sym + ": wrong arg count");
        return list.vs.next.data;
    }

    public static Mal.Val qq_loop(Mal.Val elt,
                                  Mal.Val acc)
    throws Mal.Error {
        var list = new Mal.List.empty();
        var unq = unquoted(elt, "splice-unquote");
        if (unq != null) {
            list.vs.append(new Mal.Sym("concat"));
            list.vs.append(unq);
        } else {
            list.vs.append(new Mal.Sym("cons"));
            list.vs.append(quasiquote (elt));
        }
        list.vs.append(acc);
        return list;
    }

    public static Mal.Val qq_foldr(Mal.Iterator xs)
    throws Mal.Error {
        if (xs.empty()) {
            return new Mal.List.empty();
        } else {
            var elt = xs.deref();
            xs.step();
            return qq_loop(elt, qq_foldr(xs));
        }
    }

    public static Mal.Val quasiquote(Mal.Val ast)
    throws Mal.Error {
        if (ast is Mal.List) {
            var unq = unquoted(ast, "unquote");
            if (unq != null) {
                return unq;
            } else {
                return qq_foldr((ast as Mal.List).iter());
            }
        } else if (ast is Mal.Vector) {
            var list = new Mal.List.empty();
            list.vs.append(new Mal.Sym("vec"));
            list.vs.append(qq_foldr((ast as Mal.Vector).iter()));
            return list;
        } else if (ast is Mal.Sym || ast is Mal.Hashmap) {
            var list = new Mal.List.empty();
            list.vs.append(new Mal.Sym("quote"));
            list.vs.append(ast);
            return list;
        } else {
            return ast;
        }
    }

    public static bool is_macro_call(Mal.Val v, Mal.Env env) {
        var list = v as Mal.List;
        if (list == null || list.vs == null || !(list.vs.data is Mal.Sym))
            return false;
        try {
            var fn = env.get(list.vs.data as Mal.Sym) as Mal.Function;
            return (fn != null && fn.is_macro);
        } catch (Mal.Error err) {
            return false;
        }
    }

    public static Mal.Val macroexpand(Mal.Val ast_, Mal.Env env)
    throws Mal.Error {
        Mal.Val ast = ast_;
        while (is_macro_call(ast, env)) {
            var call = ast as Mal.List;
            var macro = (env.get(call.vs.data as Mal.Sym) as Mal.Function);
            var macroargs = new Mal.List(call.vs.copy());
            macroargs.vs.remove_link(macroargs.vs.first());
            var fnenv = new Mal.Env.funcall(
                macro.env, macro.parameters, macroargs);
            ast = Mal.Main.EVAL(macro.body, fnenv);
        }
        return ast;
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
            ast = macroexpand(ast, env);
            ast_root.obj = ast;
            if (ast is Mal.List) {
                unowned GLib.List<Mal.Val> list = (ast as Mal.List).vs;
                if (list.first() == null)
                    return ast;

                var first = list.first().data;
                if (first is Mal.Sym) {
                    var sym = first as Mal.Sym;
                    switch (sym.v) {
                    case "def!":
                    case "defmacro!":
                        if (list.length() != 3)
                            throw new Mal.Error.BAD_PARAMS(
                                "def!: expected two values");
                        return define_eval(list.next.data, list.next.next.data,
                                           env, sym.v == "defmacro!");
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
                    case "quasiquoteexpand":
                        if (list.length() != 2)
                            throw new Mal.Error.BAD_PARAMS(
                                "quasiquoteexpand: expected one argument");
                        return quasiquote(list.next.data);
                    case "quasiquote":
                        if (list.length() != 2)
                            throw new Mal.Error.BAD_PARAMS(
                                "quasiquote: expected one argument");
                        ast = quasiquote(list.next.data);
                        continue;      // tail-call optimisation
                    case "macroexpand":
                        if (list.length() != 2)
                            throw new Mal.Error.BAD_PARAMS(
                                "macroexpand: expected one argument");
                        return macroexpand(list.next.data, env);
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
        setup("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", env);

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
