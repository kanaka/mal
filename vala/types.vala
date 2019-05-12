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

abstract class Mal.Val : GLib.Object {
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
}

class Mal.Nil : Mal.Listlike {
    public override bool truth_value() { return false; }
    public override Mal.Iterator iter() { return new Mal.NilIterator(); }
    public override Mal.ValWithMetadata copy() { return new Mal.Nil(); }
}

class Mal.NilIterator : Mal.Iterator {
    public override Mal.Val? deref() { return null; }
    public override Mal.Iterator step() { return this; }
}

class Mal.List : Mal.Listlike {
    public GLib.List<Val> vs;
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
    public Val[] vs;
    public Vector.from_list(GLib.List<Val> values) {
        vs = new Val[values.length()];
        int i = 0;
        foreach (var value in values) {
            vs[i++] = value;
        }
    }
    public Vector.with_size(uint size) {
        vs = new Val[size];
    }
    private Vector.copy_of(Vector v) {
        vs = v.vs;
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
    public uint length { get { return vs.length; } }
    public new Mal.Val @get(uint pos) {
        assert(pos < vs.length);
        return vs[pos];
    }
    public new void @set(uint pos, Mal.Val v) {
        assert(pos < vs.length);
        vs[pos] = v;
    }
}

class Mal.VectorIterator : Mal.Iterator {
    public Mal.Vector vec;
    public int pos;
    public override Mal.Val? deref() {
        return pos >= vec.vs.length ? null : vec.vs[pos];
    }
    public override Mal.Iterator step() {
        if (pos < vec.vs.length) pos++;
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
}

abstract class Mal.SymBase : Mal.Hashable {
    public string v;
    public override bool truth_value() { return true; }
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
}

class Mal.Hashmap : Mal.ValWithMetadata {
    public GLib.HashTable<Mal.Hashable, Mal.Val> vs;
    construct {
        vs = new GLib.HashTable<Mal.Hashable, Mal.Val>(
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
}

abstract class Mal.BuiltinFunction : Mal.ValWithMetadata {
    public abstract string name();
    public abstract Mal.Val call(Mal.List args) throws Mal.Error;
    public override bool truth_value() { return true; }
}

class Mal.Function : Mal.ValWithMetadata {
    public bool is_macro;
#if !NO_ENV
    public Mal.Listlike parameters;
    public Mal.Val body;
    public Mal.Env env;
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
}

class Mal.Atom : Mal.Val {
    public Mal.Val v;
    public Atom(Mal.Val v_) { v = v_; }
    public override bool truth_value() { return true; }
}

bool is_pair(Mal.Val v) {
    var listlike = v as Mal.Listlike;
    return listlike != null && listlike.iter().nonempty();
}
