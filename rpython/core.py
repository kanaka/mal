#import copy, time
import time

import mal_types as types
from mal_types import (MalType, MalMeta, nil, true, false,
                       MalInt, MalSym, MalStr,
                       MalList, MalVector, MalHashMap,
                       MalAtom, MalFunc)
import mal_readline
import reader
import printer

# General functions
def wrap_tf(tf):
    if tf: return true
    else:  return false

def do_equal(args): return wrap_tf(types._equal_Q(args[0], args[1]))

# Errors/Exceptions
def throw(args):
    raise types.MalException(args[0])

# Scalar functions
def nil_Q(args): return wrap_tf(types._nil_Q(args[0]))
def true_Q(args): return wrap_tf(types._true_Q(args[0]))
def false_Q(args): return wrap_tf(types._false_Q(args[0]))
def symbol(args):
    a0 = args[0]
    if isinstance(a0, MalStr):
        return types._symbol(a0.value)
    elif isinstance(a0, MalSym):
        return a0
    else:
        types.throw_str("symbol called on non-string/non-symbol")
def symbol_Q(args): return wrap_tf(types._symbol_Q(args[0]))
def keyword(args): return types._keyword(args[0])
def keyword_Q(args): return wrap_tf(types._keyword_Q(args[0]))


# String functions
def pr_str(args):
    parts = []
    for exp in args.values: parts.append(printer._pr_str(exp, True))
    return MalStr(u" ".join(parts))

def do_str(args):
    parts = []
    for exp in args.values: parts.append(printer._pr_str(exp, False))
    return MalStr(u"".join(parts))

def prn(args):
    parts = []
    for exp in args.values: parts.append(printer._pr_str(exp, True))
    print(u" ".join(parts))
    return nil

def println(args):
    parts = []
    for exp in args.values: parts.append(printer._pr_str(exp, False))
    print(u" ".join(parts))
    return nil

def do_readline(args):
    prompt = args[0]
    assert isinstance(prompt, MalStr)
    try:
        return MalStr(unicode(mal_readline.readline(str(prompt.value))))
    except EOFError:
        return nil

def read_str(args):
    a0 = args[0]
    assert isinstance(a0, MalStr)
    return reader.read_str(str(a0.value))

def slurp(args):
    a0 = args[0]
    assert isinstance(a0, MalStr)
    return MalStr(unicode(open(str(a0.value)).read()))

# Number functions
def lt(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return wrap_tf(a.value < b.value)
def lte(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return wrap_tf(a.value <= b.value)
def gt(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return wrap_tf(a.value > b.value)
def gte(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return wrap_tf(a.value >= b.value)

def plus(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value+b.value)
def minus(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value-b.value)
def multiply(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value*b.value)
def divide(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(int(a.value/b.value))

def time_ms(args):
    return MalInt(int(time.time() * 1000))


# Hash map functions
def do_hash_map(ml):
    assert isinstance(ml, MalList)
    return types._hash_mapl(ml.values)

def hash_map_Q(args):
    return wrap_tf(types._hash_map_Q(args[0]))

def assoc(args):
    src_hm, key_vals = args[0], args.rest()
    new_dct = src_hm.dct.copy()
    for i in range(0,len(key_vals),2):
        k = key_vals[i]
        assert isinstance(k, MalStr)
        new_dct[k.value] = key_vals[i+1]
    return MalHashMap(new_dct)

def dissoc(args):
    src_hm, keys = args[0], args.rest()
    new_dct = src_hm.dct.copy()
    for k in keys.values:
        assert isinstance(k, MalStr)
        if k.value in new_dct:
            del new_dct[k.value]
    return MalHashMap(new_dct)

def get(args):
    obj, key = args[0], args[1]
    if obj is nil:
        return nil
    elif isinstance(obj, MalHashMap):
        assert isinstance(key, MalStr)
        if obj and key.value in obj.dct:
            return obj.dct[key.value]
        else:
            return nil
    elif isinstance(obj, MalList):
        assert isinstance(key, MalInt)
        return obj.values[key.value]
    else:
        raise Exception("get called on invalid type")

def contains_Q(args):
    hm, key = args[0], args[1]
    assert isinstance(key, MalStr)
    return wrap_tf(key.value in hm.dct)

def keys(args):
    hm = args[0]
    keys = []
    for k in hm.dct.keys(): keys.append(MalStr(k))
    return MalList(keys)

def vals(args):
    hm = args[0]
    return MalList(hm.dct.values())


# Sequence functions
def do_list(ml):
    assert isinstance(ml, MalList)
    return ml

def list_Q(args):
    return wrap_tf(types._list_Q(args[0]))

def do_vector(ml):
    assert isinstance(ml, MalList)
    return MalVector(ml.values)

def vector_Q(args):
    return wrap_tf(types._vector_Q(args[0]))

def empty_Q(args):
    assert isinstance(args, MalType)
    seq = args[0]
    if isinstance(seq, MalList):
        return wrap_tf(len(seq) == 0)
    elif seq is nil:
        return true
    else:
        types.throw_str("empty? called on non-sequence")

def count(args):
    assert isinstance(args, MalType)
    seq = args[0]
    if isinstance(seq, MalList):
        return MalInt(len(seq))
    elif seq is nil:
        return MalInt(0)
    else:
        types.throw_str("count called on non-sequence")

def sequential_Q(args):
    return wrap_tf(types._sequential_Q(args[0]))

def cons(args):
    x, seq = args[0], args[1]
    assert isinstance(seq, MalList)
    return MalList([x] + seq.values)

def concat(args):
    new_lst = []
    for l in args.values:
        assert isinstance(l, MalList)
        new_lst = new_lst + l.values
    return MalList(new_lst)

def nth(args):
    lst, idx = args[0], args[1]
    assert isinstance(lst, MalList)
    assert isinstance(idx, MalInt)
    if idx.value < len(lst): return lst[idx.value]
    else: types.throw_str("nth: index out of range")

def first(args):
    a0 = args[0]
    assert isinstance(a0, MalList)
    if len(a0) == 0: return nil
    else:            return a0[0]

def rest(args):
    a0 = args[0]
    assert isinstance(a0, MalList)
    if len(a0) == 0: return MalList([])
    else:            return a0.rest()

# retains metadata
def conj(args):
    lst, args = args[0], args.rest()
    if types._list_Q(lst):
        vals = args.values[:]
        vals.reverse()
        new_lst = MalList(vals + lst.values)
    elif types._vector_Q(lst):
        new_lst = MalVector(lst.values + list(args.values))
    else:
        raise Exception("conj on non-list/non-vector")
    new_lst.meta = lst.meta
    return new_lst

def apply(args):
    f, fargs = args[0], args.rest()
    last_arg = fargs.values[-1]
    assert isinstance(last_arg, MalList)
    all_args = fargs.values[0:-1] + last_arg.values
    return f.apply(MalList(all_args))

def mapf(args):
    f, lst = args[0], args[1]
    assert isinstance(lst, MalList)
    res = []
    for a in lst.values:
        res.append(f.apply(MalList([a])))
    return MalList(res)


# Metadata functions
def with_meta(args):
    obj, meta = args[0], args[1]
    if isinstance(obj, MalMeta):
        new_obj = types._clone(obj)
        new_obj.meta = meta
        return new_obj
    else:
        types.throw_str("with-meta not supported on type")

def meta(args):
    obj = args[0]
    if isinstance(obj, MalMeta):
        return obj.meta
    else:
        types.throw_str("meta not supported on type")


# Atoms functions
def do_atom(args):
    return MalAtom(args[0])
def atom_Q(args):
    return wrap_tf(types._atom_Q(args[0]))
def deref(args):
    atm = args[0]
    assert isinstance(atm, MalAtom)
    return atm.value
def reset_BANG(args):
    atm, val = args[0], args[1]
    assert isinstance(atm, MalAtom)
    atm.value = val
    return atm.value
def swap_BANG(args):
    atm, f, fargs = args[0], args[1], args.slice(2)
    assert isinstance(atm, MalAtom)
    assert isinstance(f, MalFunc)
    assert isinstance(fargs, MalList)
    all_args = [atm.value] + fargs.values
    atm.value = f.apply(MalList(all_args))
    return atm.value


ns = {
        '=': do_equal,
        'throw': throw,
        'nil?': nil_Q,
        'true?': true_Q,
        'false?': false_Q,
        'symbol': symbol,
        'symbol?': symbol_Q,
        'keyword': keyword,
        'keyword?': keyword_Q,

        'pr-str': pr_str,
        'str': do_str,
        'prn': prn,
        'println': println,
        'readline': do_readline,
        'read-string': read_str,
        'slurp': slurp,
        '<':  lt,
        '<=': lte,
        '>':  gt,
        '>=': gte,
        '+':  plus,
        '-':  minus,
        '*':  multiply,
        '/':  divide,
        'time-ms': time_ms,

        'list': do_list,
        'list?': list_Q,
        'vector': do_vector,
        'vector?': vector_Q,
        'hash-map': do_hash_map,
        'map?': hash_map_Q,
        'assoc': assoc,
        'dissoc': dissoc,
        'get': get,
        'contains?': contains_Q,
        'keys': keys,
        'vals': vals,

        'sequential?': sequential_Q,
        'cons': cons,
        'concat': concat,
        'nth': nth,
        'first': first,
        'rest': rest,
        'empty?': empty_Q,
        'count': count,
        'conj': conj,
        'apply': apply,
        'map': mapf,

        'with-meta': with_meta,
        'meta': meta,
        'atom': do_atom,
        'atom?': atom_Q,
        'deref': deref,
        'reset!': reset_BANG,
        'swap!': swap_BANG
    }

