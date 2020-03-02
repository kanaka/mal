class Mal.Env : GC.Object {
    private GLib.HashTable<weak Mal.Sym, weak Mal.Val> data;
    weak Mal.Env? outer;

    construct {
        data = new GLib.HashTable<weak Mal.Sym, weak Mal.Val>(
            Mal.Hashable.hash, Mal.Hashable.equal);
    }

    public Env.within(Mal.Env outer_) {
        outer = outer_;
    }

    public Env() {
        outer = null;
    }

    public override void gc_traverse(GC.Object.VisitorFunc visit) {
        visit(outer);
        foreach (var key in data.get_keys()) {
            visit(key);
            visit(data[key]);
        }
    }

    public Env.funcall(Mal.Env outer_, Mal.Listlike binds, Mal.List exprs)
    throws Mal.Error {
        outer = outer_;
        var binditer = binds.iter();
        unowned GLib.List<Mal.Val> exprlist = exprs.vs;

        while (binditer.nonempty()) {
            var paramsym = binditer.deref() as Mal.Sym;
            if (paramsym.v == "&") {
                binditer.step();
                var rest = binditer.deref();
                binditer.step();
                if (rest == null || binditer.nonempty())
                    throw new Mal.Error.BAD_PARAMS(
                        "expected exactly one parameter name after &");
                set(rest as Mal.Sym, new Mal.List(exprlist.copy()));
                return;
            } else {
                if (exprlist == null)
                    throw new Mal.Error.BAD_PARAMS(
                        "too few arguments for function");
                set(paramsym, exprlist.data);
                binditer.step();
                exprlist = exprlist.next;
            }
        }
        if (exprlist != null)
            throw new Mal.Error.BAD_PARAMS("too many arguments for function");
    }

    // Use the 'new' keyword to silence warnings about 'set' and 'get'
    // already having meanings that we're overwriting
    public new void set(Mal.Sym key, Mal.Val f) {
        data[key] = f;
    }

    public Mal.Env? find(Mal.Sym key) {
        if (key in data)
            return this;
        if (outer == null)
            return null;
        return outer.find(key);
    }

    public new Mal.Val get(Mal.Sym key) throws Mal.Error {
        var found = find(key);
        if (found == null)
            throw new Error.ENV_LOOKUP_FAILED("'%s' not found", key.v);
        return found.data[key];
    }
}
