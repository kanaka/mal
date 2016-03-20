#import copy, time
import time

import mal_types as types
from mal_types import (throw_str,
                       MalType, MalMeta, nil, true, false,
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
def string_Q(args): return wrap_tf(types._string_Q(args[0]))
def symbol(args):
    a0 = args[0]
    if isinstance(a0, MalStr):
        return types._symbol(a0.value)
    elif isinstance(a0, MalSym):
        return a0
    else:
        throw_str("symbol called on non-string/non-symbol")
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
    if not isinstance(prompt, MalStr):
        throw_str("readline prompt is not a string")
    try:
        return MalStr(unicode(mal_readline.readline(str(prompt.value))))
    except EOFError:
        return nil

def read_str(args):
    a0 = args[0]
    if not isinstance(a0, MalStr):
        throw_str("read-string of non-string")
    return reader.read_str(str(a0.value))

def slurp(args):
    a0 = args[0]
    if not isinstance(a0, MalStr):
        throw_str("slurp with non-string filename")
    return MalStr(unicode(open(str(a0.value)).read()))

# Number functions
def lt(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("< called on non-integer")
    return wrap_tf(a.value < b.value)
def lte(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("<= called on non-integer")
    return wrap_tf(a.value <= b.value)
def gt(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("> called on non-integer")
    return wrap_tf(a.value > b.value)
def gte(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str(">= called on non-integer")
    return wrap_tf(a.value >= b.value)

def plus(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("+ called on non-integer")
    return MalInt(a.value+b.value)
def minus(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("- called on non-integer")
    return MalInt(a.value-b.value)
def multiply(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("* called on non-integer")
    return MalInt(a.value*b.value)
def divide(args):
    a, b = args[0], args[1]
    if not isinstance(a, MalInt) or not isinstance(b, MalInt):
        throw_str("/ called on non-integer")
    if b.value == 0:
        throw_str("divide by zero")
    return MalInt(int(a.value/b.value))

def time_ms(args):
    return MalInt(int(time.time() * 1000))


# Hash map functions
def do_hash_map(ml):
    return types._hash_mapl(ml.values)

def hash_map_Q(args):
    return wrap_tf(types._hash_map_Q(args[0]))

def assoc(args):
    src_hm, key_vals = args[0], args.rest()
    new_dct = src_hm.dct.copy()
    for i in range(0,len(key_vals),2):
        k = key_vals[i]
        if not isinstance(k, MalStr):
            throw_str("assoc called with non-string/non-keyword key")
        new_dct[k.value] = key_vals[i+1]
    return MalHashMap(new_dct)

def dissoc(args):
    src_hm, keys = args[0], args.rest()
    new_dct = src_hm.dct.copy()
    for k in keys.values:
        if not isinstance(k, MalStr):
            throw_str("dissoc called with non-string/non-keyword key")
        if k.value in new_dct:
            del new_dct[k.value]
    return MalHashMap(new_dct)

def get(args):
    obj, key = args[0], args[1]
    if obj is nil:
        return nil
    elif isinstance(obj, MalHashMap):
        if not isinstance(key, MalStr):
            throw_str("get called on hash-map with non-string/non-keyword key")
        if obj and key.value in obj.dct:
            return obj.dct[key.value]
        else:
            return nil
    elif isinstance(obj, MalList):
        if not isinstance(key, MalInt):
            throw_str("get called on list/vector with non-string/non-keyword key")
        return obj.values[key.value]
    else:
        throw_str("get called on invalid type")

def contains_Q(args):
    hm, key = args[0], args[1]
    if not isinstance(key, MalStr):
        throw_str("contains? called on hash-map with non-string/non-keyword key")
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
    return ml

def list_Q(args):
    return wrap_tf(types._list_Q(args[0]))

def do_vector(ml):
    return MalVector(ml.values)

def vector_Q(args):
    return wrap_tf(types._vector_Q(args[0]))

def empty_Q(args):
    seq = args[0]
    if isinstance(seq, MalList):
        return wrap_tf(len(seq) == 0)
    elif seq is nil:
        return true
    else:
        throw_str("empty? called on non-sequence")

def count(args):
    seq = args[0]
    if isinstance(seq, MalList):
        return MalInt(len(seq))
    elif seq is nil:
        return MalInt(0)
    else:
        throw_str("count called on non-sequence")

def sequential_Q(args):
    return wrap_tf(types._sequential_Q(args[0]))

def cons(args):
    x, seq = args[0], args[1]
    if not isinstance(seq, MalList):
        throw_str("cons called with non-list/non-vector")
    return MalList([x] + seq.values)

def concat(args):
    new_lst = []
    for l in args.values:
        if not isinstance(l, MalList):
            throw_str("concat called with non-list/non-vector")
        new_lst = new_lst + l.values
    return MalList(new_lst)

def nth(args):
    lst, idx = args[0], args[1]
    if not isinstance(lst, MalList):
        throw_str("nth called with non-list/non-vector")
    if not isinstance(idx, MalInt):
        throw_str("nth called with non-int index")
    if idx.value < len(lst): return lst[idx.value]
    else: throw_str("nth: index out of range")

def first(args):
    a0 = args[0]
    if a0 is nil:
        return nil
    elif not isinstance(a0, MalList):
        throw_str("first called with non-list/non-vector")
    if len(a0) == 0: return nil
    else:            return a0[0]

def rest(args):
    a0 = args[0]
    if a0 is nil:
        return MalList([])
    elif not isinstance(a0, MalList):
        throw_str("rest called with non-list/non-vector")
    if len(a0) == 0: return MalList([])
    else:            return a0.rest()

def apply(args):
    f, fargs = args[0], args.rest()
    last_arg = fargs.values[-1]
    if not isinstance(last_arg, MalList):
        throw_str("map called with non-list")
    all_args = fargs.values[0:-1] + last_arg.values
    return f.apply(MalList(all_args))

def mapf(args):
    f, lst = args[0], args[1]
    if not isinstance(lst, MalList):
        throw_str("map called with non-list")
    res = []
    for a in lst.values:
        res.append(f.apply(MalList([a])))
    return MalList(res)

# retains metadata
def conj(args):
    lst, args = args[0], args.rest()
    new_lst = None
    if types._list_Q(lst):
        vals = args.values[:]
        vals.reverse()
        new_lst = MalList(vals + lst.values)
    elif types._vector_Q(lst):
        new_lst = MalVector(lst.values + list(args.values))
    else:
        throw_str("conj on non-list/non-vector")
    new_lst.meta = lst.meta
    return new_lst

def seq(args):
    a0 = args[0]
    if isinstance(a0, MalVector):
        if len(a0) == 0: return nil
        return MalList(a0.values)
    elif isinstance(a0, MalList):
        if len(a0) == 0: return nil
        return a0
    elif types._string_Q(a0):
        assert isinstance(a0, MalStr)
        if len(a0) == 0: return nil
        return MalList([MalStr(unicode(c)) for c in a0.value])
    elif a0 is nil:
        return nil
    else:
        throw_str("seq: called on non-sequence")

# Metadata functions
def with_meta(args):
    obj, meta = args[0], args[1]
    if isinstance(obj, MalMeta):
        new_obj = types._clone(obj)
        new_obj.meta = meta
        return new_obj
    else:
        throw_str("with-meta not supported on type")

def meta(args):
    obj = args[0]
    if isinstance(obj, MalMeta):
        return obj.meta
    else:
        throw_str("meta not supported on type")


# Atoms functions
def do_atom(args):
    return MalAtom(args[0])
def atom_Q(args):
    return wrap_tf(types._atom_Q(args[0]))
def deref(args):
    atm = args[0]
    if not isinstance(atm, MalAtom):
        throw_str("deref called on non-atom")
    return atm.value
def reset_BANG(args):
    atm, val = args[0], args[1]
    if not isinstance(atm, MalAtom):
        throw_str("reset! called on non-atom")
    atm.value = val
    return atm.value
def swap_BANG(args):
    atm, f, fargs = args[0], args[1], args.slice(2)
    if not isinstance(atm, MalAtom):
        throw_str("swap! called on non-atom")
    if not isinstance(f, MalFunc):
        throw_str("swap! called with non-function")
    all_args = [atm.value] + fargs.values
    atm.value = f.apply(MalList(all_args))
    return atm.value


ns = {
        '=': do_equal,
        'throw': throw,
        'nil?': nil_Q,
        'true?': true_Q,
        'false?': false_Q,
        'string?': string_Q,
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
        'apply': apply,
        'map': mapf,

        'conj': conj,
        'seq': seq,

        'with-meta': with_meta,
        'meta': meta,
        'atom': do_atom,
        'atom?': atom_Q,
        'deref': deref,
        'reset!': reset_BANG,
        'swap!': swap_BANG
    }

