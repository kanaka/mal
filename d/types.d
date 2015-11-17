import std.algorithm;
import std.array;
import std.conv;
import std.functional;
import std.range;
import env;

abstract class MalType
{
    string print(bool readable) const;
    bool is_truthy() const { return true; }
}

interface MalMeta
{
    MalType meta();
    MalType with_meta(MalType new_meta);
}

class MalNil : MalType
{
    override string print(bool readable) const { return "nil"; }
    override bool is_truthy() const { return false; }
    override bool opEquals(Object o) { return (cast(MalNil)(o) !is null); }
}

class MalFalse : MalType
{
    override string print(bool readable) const { return "false"; }
    override bool is_truthy() const { return false; }
    override bool opEquals(Object o) { return (cast(MalFalse)(o) !is null); }
}

class MalTrue : MalType
{
    override string print(bool readable) const { return "true"; }
    override bool opEquals(Object o) { return (cast(MalTrue)(o) !is null); }
}

MalNil mal_nil;
MalFalse mal_false;
MalTrue mal_true;

static this()
{
    mal_nil = new MalNil;
    mal_false = new MalFalse;
    mal_true = new MalTrue;
}

MalType bool_to_mal(in bool b)
{
    return b ? mal_true : mal_false;
}

class MalSymbol : MalType
{
    const string name;
    this(in string token) { name = token; }
    override string print(bool readable) const { return name; }

    override size_t toHash()
    {
        return typeid(name).getHash(&name);
    }

    override int opCmp(Object other)
    {
        MalSymbol o = cast(MalSymbol) other;
        return cmp(name, o.name);
    }

    override bool opEquals(Object other)
    {
        auto o = cast(MalSymbol) other;
        return (o !is null && name == o.name);
    }
}

class MalInteger : MalType
{
    const long val;
    this(string token) { val = to!long(token); }
    this(long v) { val = v; }
    override string print(bool readable) const { return to!string(val); }

    override bool opEquals(Object o)
    {
        auto oint = cast(MalInteger)(o);
        return (oint !is null && val == oint.val);
    }
}

class MalString : MalType
{
    const string val;
    this(in string token) { val = token; }
    override string print(bool readable) const
    {
        if (is_keyword()) return ":" ~ val[2..$];
        if (readable)
        {
            string escaped = val.replace("\\", "\\\\")
                                .replace("\"", "\\\"")
                                .replace("\n", "\\n");
            return "\"" ~ escaped ~ "\"";
        }
        else
        {
            return val;
        }
    }

    bool is_keyword() const
    {
        return val.length > 1 && val[0..2] == "\u029e";
    }

    override bool opEquals(Object o)
    {
        auto ostr = cast(MalString)(o);
        return (ostr !is null && val == ostr.val);
    }
}

abstract class MalSequential : MalType, MalMeta
{
    MalType[] elements;
    MalType meta_val;

    this(MalType[] lst) {
        elements = lst;
        meta_val = mal_nil;
    }

    override bool opEquals(Object o)
    {
        auto oseq = cast(MalSequential)(o);
        return (oseq !is null && elements == oseq.elements);
    }

    MalSequential conj(MalType element);
}

class MalList : MalSequential, MalMeta
{
    this(MalType[] lst) { super(lst); }
    this(MalList that, MalType new_meta)
    {
        super(that.elements);
        meta_val = new_meta;
    }

    override string print(bool readable) const
    {
        auto items_strs = elements.map!(e => e.print(readable));
        return "(" ~ array(items_strs).join(" ") ~ ")";
    }

    override MalSequential conj(MalType element)
    {
        return new MalList([element] ~ elements);
    }

    override MalType meta() { return meta_val; }
    override MalType with_meta(MalType new_meta)
    {
        return new MalList(this, new_meta);
    }
}

class MalVector : MalSequential, MalMeta
{
    this(MalType[] lst) { super(lst); }
    this(MalVector that, MalType new_meta)
    {
        super(that.elements);
        meta_val = new_meta;
    }

    override string print(bool readable) const
    {
        auto items_strs = elements.map!(e => e.print(readable));
        return "[" ~ array(items_strs).join(" ") ~ "]";
    }

    override MalSequential conj(MalType element)
    {
        return new MalVector(elements ~ [element]);
    }

    override MalType meta() { return meta_val; }
    override MalType with_meta(MalType new_meta)
    {
        return new MalVector(this, new_meta);
    }
}

class MalHashmap : MalType, MalMeta
{
    MalType[string] data;
    MalType meta_val;

    this(MalType[string] map)
    {
        data = map;
        meta_val = mal_nil;
    }
    this(MalType[] lst)
    {
        put_kv_list(lst);
        meta_val = mal_nil;
    }
    this(MalHashmap that, MalType new_meta)
    {
        data = that.data;
        meta_val = new_meta;
    }

    bool contains(in MalType key)
    {
        auto valp = (make_hash_key(key) in data);
        return valp !is null;
    }

    MalType get(in MalType key)
    {
        auto valp = (make_hash_key(key) in data);
        return valp is null ? mal_nil : *valp;
    }

    void remove(in MalType key)
    {
        data.remove(make_hash_key(key));
    }

    void put(in MalType key, MalType val)
    {
        data[make_hash_key(key)] = val;
    }

    void put_kv_list(MalType[] lst)
    {
        foreach (kv; chunks(lst, 2))
        {
            if (kv.length < 2) throw new Exception("requires even number of elements");
            put(kv[0], kv[1]);
        }
    }

    private string make_hash_key(in MalType key)
    {
        return verify_cast!MalString(key).val;
    }

    override string print(bool readable) const
    {
        string[] parts;
        foreach (k, v; data)
        {
            parts ~= (new MalString(k)).print(readable);
            parts ~= v.print(readable);
        }
        return "{" ~ parts.join(" ") ~ "}";
    }

    override bool opEquals(Object o)
    {
        auto ohm = cast(MalHashmap)(o);
        return (ohm !is null && data == ohm.data);
    }

    override MalType meta() { return meta_val; }
    override MalType with_meta(MalType new_meta)
    {
        return new MalHashmap(this, new_meta);
    }
}

alias BuiltinStaticFuncType = MalType function(MalType[] a ...);
alias BuiltinFuncType = MalType delegate(MalType[] a ...);

class MalBuiltinFunc : MalType, MalMeta
{
    const BuiltinFuncType fn;
    const string name;
    MalType meta_val;

    this(in BuiltinFuncType fn_v, in string name_v)
    {
        fn = fn_v;
        name = name_v;
        meta_val = mal_nil;
    }

    this(in BuiltinStaticFuncType static_fn_v, in string name_v)
    {
        fn = toDelegate(static_fn_v);
        name = name_v;
        meta_val = mal_nil;
    }

    this(MalBuiltinFunc that, MalType new_meta)
    {
        fn = that.fn;
        name = that.name;
        meta_val = new_meta;
    }

    override string print(bool readable) const
    {
        return "<BuiltinFunc:" ~ name ~ ">";
    }

    override MalType meta() { return meta_val; }

    override MalType with_meta(MalType new_meta)
    {
        return new MalBuiltinFunc(this, new_meta);
    }
}

class MalFunc : MalType, MalMeta
{
    MalType[] arg_names;
    MalType func_body;
    Env def_env;
    bool is_macro;
    MalType meta_val;

    this(MalType[] arg_names_v, MalType func_body_v, Env def_env_v)
    {
        arg_names = arg_names_v;
        func_body = func_body_v;
        def_env = def_env_v;
        is_macro = false;
        meta_val = mal_nil;
    }

    this(MalFunc that, MalType new_meta)
    {
        arg_names = that.arg_names;
        func_body = that.func_body;
        def_env = that.def_env;
        is_macro = that.is_macro;
        meta_val = new_meta;
    }

    override string print(bool readable) const
    {
        return "<Function:args=" ~ array(arg_names.map!(e => e.print(true))).join(",") ~ ">";
    }

    override MalType meta() { return meta_val; }

    override MalType with_meta(MalType new_meta)
    {
        return new MalFunc(this, new_meta);
    }
}

class MalAtom : MalType, MalMeta
{
    MalType val;
    MalType meta_val;

    this(MalType v)
    {
        val = v;
        meta_val = mal_nil;
    }

    this(MalAtom that, MalType new_meta)
    {
        val = that.val;
        meta_val = new_meta;
    }

    override string print(bool readable) const
    {
        return "(atom " ~ val.print(readable) ~ ")";
    }

    override bool opEquals(Object other)
    {
        auto o = cast(MalAtom) other;
        return (o !is null && val == o.val);
    }

    override MalType meta() { return meta_val; }

    override MalType with_meta(MalType new_meta)
    {
        return new MalAtom(this, new_meta);
    }
}

class MalException : Exception
{
    MalType data;

    this(MalType val)
    {
        super("MalException");
        data = val;
    }
}

T verify_cast(T)(in MalType v)
{
    T res = cast(T) v;
    if (res is null) throw new Exception("Expected " ~ typeid(T).name);
    return res;
}

MalType mal_type_q(T)(in MalType[] a)
{
    verify_args_count(a, 1);
    T res = cast(T) a[0];
    return bool_to_mal(res !is null);
}

inout(MalType[]) verify_args_count(inout MalType[] args, in int expected_length)
{
    if (args.length != expected_length)
    {
        throw new Exception("Expected " ~ to!string(expected_length) ~ " arguments");
    }
    return args;
}

void verify_min_args_count(in MalType[] args, in int min_expected_length)
{
    if (args.length < min_expected_length)
    {
        throw new Exception("Expected at least " ~ to!string(min_expected_length) ~ " arguments");
    }
}
