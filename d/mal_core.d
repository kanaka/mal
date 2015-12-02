import core.time;
import std.algorithm;
import std.array;
import std.datetime;
import std.file;
import std.stdio;
import env;
import main;
import reader;
import readline;
import types;
import printer;

static MalType mal_equal(MalType[] a ...)
{
    verify_args_count(a, 2);
    return bool_to_mal(a[0] == a[1]);
}

static MalType mal_throw(MalType[] a ...)
{
    verify_args_count(a, 1);
    throw new MalException(a[0]);
}

static MalType mal_symbol(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto s = verify_cast!MalString(a[0]);
    return new MalSymbol(s.val);
}

static MalType mal_keyword(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto s = verify_cast!MalString(a[0]);
    return new MalString("\u029e" ~ s.val);
}

static MalType mal_keyword_q(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto s = cast(MalString) a[0];
    if (s is null) return mal_false;
    return bool_to_mal(s.is_keyword());
}

static MalType mal_pr_str(MalType[] a ...)
{
    auto items_strs = a.map!(e => pr_str(e, true));
    return new MalString(array(items_strs).join(" "));
}

static MalType mal_str(MalType[] a ...)
{
    auto items_strs = a.map!(e => pr_str(e, false));
    return new MalString(array(items_strs).join(""));
}

static MalType mal_prn(MalType[] a ...)
{
    auto items_strs = a.map!(e => pr_str(e, true));
    writeln(array(items_strs).join(" "));
    return mal_nil;
}

static MalType mal_println(MalType[] a ...)
{
    auto items_strs = a.map!(e => pr_str(e, false));
    writeln(array(items_strs).join(" "));
    return mal_nil;
}

static MalType mal_read_string(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto s = verify_cast!MalString(a[0]);
    return read_str(s.val);
}

static MalType mal_readline(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto s = verify_cast!MalString(a[0]);
    auto line = _readline(s.val);
    return line is null ? mal_nil : new MalString(line);
}

static MalType mal_slurp(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto filename = verify_cast!MalString(a[0]).val;
    auto content = cast(string) std.file.read(filename);
    return new MalString(content);
}

alias TwoIntFunc = MalType function(long x, long y);

MalType binary_int_op(TwoIntFunc f, MalType[] a ...)
{
    verify_args_count(a, 2);
    MalInteger i0 = verify_cast!MalInteger(a[0]);
    MalInteger i1 = verify_cast!MalInteger(a[1]);
    return f(i0.val, i1.val);
}

static MalType mal_time_ms(MalType[] a ...)
{
    immutable epoch = SysTime(unixTimeToStdTime(0));
    immutable hnsecs_since_epoch = Clock.currTime(UTC()) - epoch;
    immutable ms = hnsecs_since_epoch.total!"msecs"();
    return new MalInteger(ms);
}

static bool is_nil(MalType v)
{
    return cast(MalNil)(v) !is null;
}

static MalType mal_assoc(MalType[] a ...)
{
    verify_min_args_count(a, 1);
    auto hm = verify_cast!MalHashmap(a[0]);
    auto new_hm = new MalHashmap(hm.data.dup);
    new_hm.put_kv_list(a[1..$]);
    return new_hm;
}

static MalType mal_dissoc(MalType[] a ...)
{
    verify_min_args_count(a, 1);
    auto hm = verify_cast!MalHashmap(a[0]);
    auto new_hm = new MalHashmap(hm.data.dup);
    foreach (k; a[1..$])
    {
        new_hm.remove(k);
    }
    return new_hm;
}

static MalType mal_get(MalType[] a ...)
{
    verify_args_count(a, 2);
    if (is_nil(a[0])) return mal_nil;
    auto hm = verify_cast!MalHashmap(a[0]);
    return hm.get(a[1]);
}

static MalType mal_contains_q(MalType[] a ...)
{
    verify_args_count(a, 2);
    if (is_nil(a[0])) return mal_false;
    auto hm = verify_cast!MalHashmap(a[0]);
    return bool_to_mal(hm.contains(a[1]));
}

static MalType mal_keys(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto hm = verify_cast!MalHashmap(a[0]);
    auto keys = hm.data.keys.map!(s => cast(MalType)(new MalString(s)));
    return new MalList(array(keys));
}

static MalType mal_vals(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto hm = verify_cast!MalHashmap(a[0]);
    return new MalList(hm.data.values);
}

static MalType mal_cons(MalType[] a ...)
{
    verify_args_count(a, 2);
    auto lst = verify_cast!MalSequential(a[1]);
    return new MalList([a[0]] ~ lst.elements);
}

static MalType mal_concat(MalType[] a ...)
{
    MalType[] res;
    foreach (e; a)
    {
        auto lst = verify_cast!MalSequential(e);
        res ~= lst.elements;
    }
    return new MalList(res);
}

static MalType mal_nth(MalType[] a ...)
{
    verify_args_count(a, 2);
    if (is_nil(a[0]))
    {
        throw new Exception("nth: index out of range");
    }
    auto seq = verify_cast!MalSequential(a[0]);
    auto index = verify_cast!MalInteger(a[1]).val;
    if (index >= seq.elements.length)
    {
        throw new Exception("nth: index out of range");
    }
    return seq.elements[index];
}

static MalType mal_first(MalType[] a ...)
{
    verify_args_count(a, 1);
    if (is_nil(a[0])) return mal_nil;
    auto seq = verify_cast!MalSequential(a[0]);
    if (seq.elements.length == 0) return mal_nil;
    return seq.elements[0];
}

static MalType mal_rest(MalType[] a ...)
{
    verify_args_count(a, 1);
    if (is_nil(a[0])) return new MalList([]);
    auto seq = verify_cast!MalSequential(a[0]);
    if (seq.elements.length == 0) return new MalList([]);
    return new MalList(seq.elements[1..$]);
}


static MalType mal_empty_q(MalType[] a ...)
{
    verify_args_count(a, 1);
    if (is_nil(a[0]))
    {
        return mal_true;
    }
    auto s = verify_cast!MalSequential(a[0]);
    return bool_to_mal(s.elements.length == 0);
}

static MalType mal_count(MalType[] a ...)
{
    verify_args_count(a, 1);
    if (is_nil(a[0]))
    {
        return new MalInteger(0);
    }
    auto s = verify_cast!MalSequential(a[0]);
    return new MalInteger(cast(int)(s.elements.length));
}

static MalType mal_apply(MalType[] a ...)
{
    verify_min_args_count(a, 2);
    auto last_seq_elems = verify_cast!MalSequential(a[$-1]).elements;
    auto funcargs = a.length == 2 ? last_seq_elems : (a[1..$-1] ~ last_seq_elems);

    auto builtinfn = cast(MalBuiltinFunc) a[0];
    if (builtinfn !is null)
    {
        return builtinfn.fn(funcargs);
    }

    auto malfunc = verify_cast!MalFunc(a[0]);
    auto callenv = new Env(malfunc.def_env, malfunc.arg_names, funcargs);

    return EVAL(malfunc.func_body, callenv);
}

static MalType mal_map(MalType[] a ...)
{
    verify_args_count(a, 2);
    auto seq = verify_cast!MalSequential(a[1]);
    auto mapped_items = seq.elements.map!(e => mal_apply(a[0], new MalList([e])));
    return new MalList(array(mapped_items));
}

static MalType mal_conj(MalType[] a ...)
{
    verify_min_args_count(a, 1);
    auto seq = verify_cast!MalSequential(a[0]);
    return reduce!((s,e) => s.conj(e))(seq, a[1..$]);
}

static MalType mal_meta(MalType[] a ...)
{
    verify_args_count(a, 1);
    auto metaobj = cast(MalMeta) a[0];
    if (metaobj is null) return mal_nil;
    return metaobj.meta();
}

static MalType mal_with_meta(MalType[] a ...)
{
    verify_args_count(a, 2);
    auto metaobj = cast(MalMeta) a[0];
    if (metaobj is null) return a[0];
    return metaobj.with_meta(a[1]);
}

static MalType mal_reset_bang(MalType[] a ...)
{
    verify_args_count(a, 2);
    verify_cast!MalAtom(a[0]).val = a[1];
    return a[1];
}

static MalType mal_swap_bang(MalType[] a ...)
{
    verify_min_args_count(a, 2);
    auto atom = verify_cast!MalAtom(a[0]);
    auto args = [atom.val] ~ a[2..$];
    auto newval = mal_apply([a[1], new MalList(args)]);
    return mal_reset_bang([atom, newval]);
}

BuiltinStaticFuncType[string] core_ns;

static this()
{
    core_ns = [
        "=":        &mal_equal,
        "throw":    &mal_throw,

        "nil?":     (a ...) => mal_type_q!MalNil(a),
        "true?":    (a ...) => mal_type_q!MalTrue(a),
        "false?":   (a ...) => mal_type_q!MalFalse(a),
        "symbol":   &mal_symbol,
        "symbol?":  (a ...) => mal_type_q!MalSymbol(a),
        "keyword":  &mal_keyword,
        "keyword?": &mal_keyword_q,

        "pr-str":   &mal_pr_str,
        "str":      &mal_str,
        "prn":      &mal_prn,
        "println":  &mal_println,
        "read-string": &mal_read_string,
        "readline": &mal_readline,
        "slurp":    &mal_slurp,

        "<":        (a ...) => binary_int_op((x,y) => bool_to_mal(x < y), a),
        "<=":       (a ...) => binary_int_op((x,y) => bool_to_mal(x <= y), a),
        ">":        (a ...) => binary_int_op((x,y) => bool_to_mal(x > y), a),
        ">=":       (a ...) => binary_int_op((x,y) => bool_to_mal(x >= y), a),
        "+":        (a ...) => binary_int_op((x,y) => new MalInteger(x + y), a),
        "-":        (a ...) => binary_int_op((x,y) => new MalInteger(x - y), a),
        "*":        (a ...) => binary_int_op((x,y) => new MalInteger(x * y), a),
        "/":        (a ...) => binary_int_op((x,y) => new MalInteger(x / y), a),
        "time-ms":  &mal_time_ms,

        "list":     (a ...) => new MalList(a),
        "list?":    (a ...) => mal_type_q!MalList(a),
        "vector":   (a ...) => new MalVector(a),
        "vector?":  (a ...) => mal_type_q!MalVector(a),
        "hash-map": (a ...) => new MalHashmap(a),
        "map?":     (a ...) => mal_type_q!MalHashmap(a),
        "assoc":    &mal_assoc,
        "dissoc":   &mal_dissoc,
        "get":      &mal_get,
        "contains?": &mal_contains_q,
        "keys":     &mal_keys,
        "vals":     &mal_vals,

        "sequential?": (a ...) => mal_type_q!MalSequential(a),
        "cons":     &mal_cons,
        "concat":   &mal_concat,
        "nth":      &mal_nth,
        "first":    &mal_first,
        "rest":     &mal_rest,
        "empty?":   &mal_empty_q,
        "count":    &mal_count,
        "apply":    &mal_apply,
        "map":      &mal_map,

        "conj":     &mal_conj,

        "meta":     &mal_meta,
        "with-meta": &mal_with_meta,
        "atom":     (a ...) => new MalAtom(verify_args_count(a, 1)[0]),
        "atom?":    (a ...) => mal_type_q!MalAtom(a),
        "deref":    (a ...) => verify_cast!MalAtom(verify_args_count(a, 1)[0]).val,
        "reset!":   &mal_reset_bang,
        "swap!":    &mal_swap_bang
    ];
}
