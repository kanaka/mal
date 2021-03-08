public errordomain Mal.Error {
    BAD_TOKEN,
    PARSE_ERROR,
    HASH_KEY_TYPE_ERROR,
    ENV_LOOKUP_FAILED,
    BAD_PARAMS,
    CANNOT_APPLY,
    EXCEPTION_THROWN,
    NOT_IMPLEMENTED_IN_THIS_STEP,
}

abstract class Mal.Val : GC.Object {
    public abstract bool truth_value();
}

abstract class Mal.Hashable : Mal.Val {
    public string hashkey;
    public static uint hash(Hashable h) { return str_hash(h.hashkey); }
    public static bool equal(Hashable hl, Hashable hr) {
        return hl.hashkey == hr.hashkey;
    }
}

class Mal.Bool : Mal.Hashable {
    public bool v;
    public Bool(bool value) {
        v = value;
        hashkey = value ? "bt" : "bf";
    }
    public override bool truth_value() { return v; }
    public override void gc_traverse(GC.Object.VisitorFunc visit) {}
}

// Mal.Listlike is a subclass of Mal.Val which includes both lists and
// vectors, and provides a common iterator API so that core functions
// and special forms can treat them the same.
//
// Most core functions that take a list argument also accept nil. To
// make that easy, Mal.Nil also derives from Mal.Listlike.
abstract class Mal.Listlike : Mal.ValWithMetadata {
    public abstract Mal.Iterator iter();
}

abstract class Mal.Iterator : GLib.Object {
    public abstract Mal.Val? deref();
    public abstract Mal.Iterator step();
    public bool empty() { return deref() == null; }
    public bool nonempty() { return deref() != null; }
}

// ValWithMetadata is a subclass of Mal.Val which includes every value
// type you can put metadata on. Value types implementing this class
// must provide a copy() method, because with-meta has to make a copy
// of the value with new metadata.
abstract class Mal.ValWithMetadata : Mal.Val {
    public Mal.Val? metadata;
    construct {
        metadata = null;
    }
    public abstract Mal.ValWithMetadata copy();
    public abstract void gc_traverse_m(GC.Object.VisitorFunc visit);
    public override void gc_traverse(GC.Object.VisitorFunc visit) {
        visit(metadata);
        gc_traverse_m(visit);
    }
}

class Mal.Nil : Mal.Listlike {
    public override bool truth_value() { return false; }
    public override Mal.Iterator iter() { return new Mal.NilIterator(); }
    public override Mal.ValWithMetadata copy() { return new Mal.Nil(); }
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {}
}

class Mal.NilIterator : Mal.Iterator {
    public override Mal.Val? deref() { return null; }
    public override Mal.Iterator step() { return this; }
}

class Mal.List : Mal.Listlike {
    public GLib.List<weak Val> vs;
    public List(GLib.List<Val> values) {
        foreach (var value in values) {
            vs.append(value);
        }
    }
    public List.empty() {
    }
    public override bool truth_value() { return true; }
    public override Mal.Iterator iter() {
        var toret = new Mal.ListIterator();
        toret.node = vs;
        return toret;
    }
    public override Mal.ValWithMetadata copy() {
        return new Mal.List(vs);
    }        
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {
        foreach (var v in vs)
            visit(v);
    }
}

class Mal.ListIterator : Mal.Iterator {
    public unowned GLib.List<Mal.Val>? node;
    public override Mal.Val? deref() {
        return node == null ? null : node.data;
    }
    public override Mal.Iterator step() {
        if (node != null)
            node = node.next;
        return this;
    }
}

class Mal.Vector : Mal.Listlike {
    struct Ref { weak Mal.Val v; }
    private Ref[] rs;
    public Vector.from_list(GLib.List<Val> values) {
        rs = new Ref[values.length()];
        int i = 0;
        foreach (var value in values) {
            rs[i++] = { value };
        }
    }
    public Vector.with_size(uint size) {
        rs = new Ref[size];
    }
    private Vector.copy_of(Vector v) {
        rs = v.rs;
    }
    public override bool truth_value() { return true; }
    public override Mal.Iterator iter() {
        var toret = new Mal.VectorIterator();
        toret.vec = this;
        toret.pos = 0;
        return toret;
    }
    public override Mal.ValWithMetadata copy() {
        return new Mal.Vector.copy_of(this);
    }
    public uint length { get { return rs.length; } }
    public new Mal.Val @get(uint pos) {
        assert(pos < rs.length);
        return rs[pos].v;
    }
    public new void @set(uint pos, Mal.Val v) {
        assert(pos < rs.length);
        rs[pos].v = v;
    }
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {
        foreach (var r in rs)
            visit(r.v);
    }
}

class Mal.VectorIterator : Mal.Iterator {
    public Mal.Vector vec;
    public int pos;
    public override Mal.Val? deref() {
        return pos >= vec.length ? null : vec[pos];
    }
    public override Mal.Iterator step() {
        if (pos < vec.length) pos++;
        return this;
    }
}

class Mal.Num : Mal.Hashable {
    public int64 v;
    public Num(int64 value) {
        v = value;
        hashkey = "N" + v.to_string();
    }
    public override bool truth_value() { return true; }
    public override void gc_traverse(GC.Object.VisitorFunc visit) {}
}

abstract class Mal.SymBase : Mal.Hashable {
    public string v;
    public override bool truth_value() { return true; }
    public override void gc_traverse(GC.Object.VisitorFunc visit) {}
}

class Mal.Sym : Mal.SymBase {
    public Sym(string value) {
        v = value;
        hashkey = "'" + v;
    }
}

class Mal.Keyword : Mal.SymBase {
    public Keyword(string value) {
        v = value;
        hashkey = ":" + v;
    }
}

class Mal.String : Mal.Hashable {
    public string v;
    public String(string value) {
        v = value;
        hashkey = "\"" + v;
    }
    public override bool truth_value() { return true; }
    public override void gc_traverse(GC.Object.VisitorFunc visit) {}
}

class Mal.Hashmap : Mal.ValWithMetadata {
    public GLib.HashTable<weak Mal.Hashable, weak Mal.Val> vs;
    construct {
        vs = new GLib.HashTable<weak Mal.Hashable, weak Mal.Val>(
            Mal.Hashable.hash, Mal.Hashable.equal);
    }
    public void insert(Mal.Val key, Mal.Val value) throws Mal.Error {
        var hkey = key as Mal.Hashable;
        if (hkey == null)
            throw new Error.HASH_KEY_TYPE_ERROR("bad type as hash key");
        vs[hkey] = value;
    }
    public void remove(Mal.Val key) throws Mal.Error {
        var hkey = key as Mal.Hashable;
        if (hkey == null)
            throw new Error.HASH_KEY_TYPE_ERROR("bad type as hash key");
        vs.remove(hkey);
    }
    public override bool truth_value() { return true; }
    public override Mal.ValWithMetadata copy() {
        var toret = new Mal.Hashmap();
        toret.vs = vs;
        return toret;
    }        
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {
        foreach (var key in vs.get_keys()) {
            visit(key);
            visit(vs[key]);
        }
    }
}

abstract class Mal.BuiltinFunction : Mal.ValWithMetadata {
    public abstract string name();
    public abstract Mal.Val call(Mal.List args) throws Mal.Error;
    public override bool truth_value() { return true; }
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {}
}

class Mal.Function : Mal.ValWithMetadata {
    public bool is_macro;
#if !NO_ENV
    public weak Mal.Listlike parameters;
    public weak Mal.Val body;
    public weak Mal.Env env;
    public Function(Mal.Listlike parameters_, Mal.Val body_, Mal.Env env_) {
        parameters = parameters_;
        body = body_;
        env = env_;
        is_macro = false;
    }
#endif
    public override Mal.ValWithMetadata copy() {
#if !NO_ENV
        var copied = new Mal.Function(parameters, body, env);
        copied.is_macro = is_macro;
        return copied;
#else
        throw new Mal.Error.NOT_IMPLEMENTED_IN_THIS_STEP(
            "can't copy a Mal.Function without Mal.Env existing");
#endif
    }
    public override bool truth_value() { return true; }
    public override void gc_traverse_m(GC.Object.VisitorFunc visit) {
#if !NO_ENV
        visit(parameters);
        visit(body);
        visit(env);
#endif
    }
}

class Mal.Atom : Mal.Val {
    public weak Mal.Val v;
    public Atom(Mal.Val v_) { v = v_; }
    public override bool truth_value() { return true; }
    public override void gc_traverse(GC.Object.VisitorFunc visit) {
        visit(v);
    }
}
