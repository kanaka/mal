import copy, time

import mal_types as types
from mal_types import (MalType, nil, true, false,
                       MalInt, MalSym, MalStr, MalList)
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


## Hash map functions
#def assoc(src_hm, *key_vals):
#    hm = copy.copy(src_hm)
#    for i in range(0,len(key_vals),2): hm[key_vals[i]] = key_vals[i+1]
#    return hm
#
#def dissoc(src_hm, *keys):
#    hm = copy.copy(src_hm)
#    for key in keys:
#        if key in hm: del hm[key]
#    return hm
#
#def get(hm, key):
#    if hm and key in hm:
#        return hm[key]
#    else:
#        return None
#
#def contains_Q(hm, key): return key in hm
#
#def keys(hm): return types._list(*hm.keys())
#
#def vals(hm): return types._list(*hm.values())
#

# Sequence functions
def do_list(ml):
    assert isinstance(ml, MalList)
    return ml

def list_Q(args):
    return wrap_tf(isinstance(args[0], MalList))

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

#def coll_Q(coll): return sequential_Q(coll) or hash_map_Q(coll)
#
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

## retains metadata
#def conj(lst, *args):
#    if types._list_Q(lst): 
#        new_lst = List(list(reversed(list(args))) + lst)
#    else:
#        new_lst = Vector(lst + list(args))
#    if hasattr(lst, "__meta__"):
#        new_lst.__meta__ = lst.__meta__
#    return new_lst

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


## Metadata functions
#def with_meta(obj, meta):
#    new_obj = types._clone(obj)
#    new_obj.__meta__ = meta
#    return new_obj
#
#def meta(obj):
#    if hasattr(obj, "__meta__"): return obj.__meta__
#    else:                        return None
#
#
## Atoms functions
#def deref(atm):    return atm.val
#def reset_BANG(atm,val):
#    atm.val = val
#    return atm.val
#def swap_BANG(atm,f,*args):
#    atm.val = f(atm.val,*args)
#    return atm.val


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
#        'readline': lambda prompt: mal_readline.readline(prompt),
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
#        'time-ms': lambda : int(time.time() * 1000),
#
        'list': do_list,
        'list?': list_Q,
#        'vector': types._vector,
#        'vector?': types._vector_Q,
#        'hash-map': types._hash_map,
#        'map?': types._hash_map_Q,
#        'assoc': assoc,
#        'dissoc': dissoc,
#        'get': get,
#        'contains?': contains_Q,
#        'keys': keys,
#        'vals': vals,
#
#        'sequential?': types._sequential_Q,
        'cons': cons,
        'concat': concat,
        'nth': nth,
        'first': first,
        'rest': rest,
        'empty?': empty_Q,
        'count': count,
#        'conj': conj,
        'apply': apply,
        'map': mapf,
#
#        'with-meta': with_meta,
#        'meta': meta,
#        'atom': types._atom,
#        'atom?': types._atom_Q,
#        'deref': deref,
#        'reset!': reset_BANG,
#        'swap!': swap_BANG
    }

