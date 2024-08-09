import operator
import time
from itertools import chain

import mal_types as types
from mal_types import MalException, List, Vector
import mal_readline
import reader
import printer

# Errors/Exceptions
def throw(obj): raise MalException(obj)


# String functions
def pr_str(*args):
    return printer.pr_list(args, " ", True)

def do_str(*args):
    return printer.pr_list(args, "", False)

def prn(*args):
    print(printer.pr_list(args, " ", True))
    return None

def println(*args):
    print(printer.pr_list(args, " ", False))
    return None

def core_readline(prompt):
    try:
        return mal_readline.readline(prompt)
    except EOFError:
        return None

def slurp(path):
    with open(path) as f:
        return f.read()

# Hash map functions
def assoc(src_hm, *key_vals):
    hm = types.Hash_Map(src_hm)
    hm.update(types.asPairs(key_vals))
    return hm

def dissoc(src_hm, *keys):
    hm = types.Hash_Map(src_hm)
    for key in keys:
        hm.pop(key, None)
    return hm

def get(hm, key):
    if hm is not None:
        return hm.get(key)
    else:
        return None

contains_Q = types.Hash_Map.__contains__

keys = List

def vals(hm): return List(hm.values())


# Sequence functions
def cons(x, seq): return concat((x,), seq)

def concat(*lsts): return List(chain(*lsts))

nth = tuple.__getitem__

def first(lst):
    if lst:
        return lst[0]
    else:                       # lst is nil or empty
        return None

def rest(lst):
    if lst:
        it = iter(lst)
        next(it)
        return List(it)
    else:                       # lst is nil or empty
        return List()

empty_Q = operator.not_

def count(lst):
    if types._nil_Q(lst): return 0
    else: return len(lst)

def apply(f, *args): return f(*chain(args[:-1], args[-1]))

def mapf(f, lst): return List(map(f, lst))

def conj(lst, *args):
    if types._list_Q(lst):
        return concat(reversed(args), lst)
    else:
        return Vector(chain(lst, args))

def seq(obj):
    if not obj:
        return None             # obj is nil, (), [] or ""
    if types._list_Q(obj):
        return obj
    elif types._vector_Q(obj) or types._string_Q(obj):
        return List(obj)
    else: throw ("seq: called on non-sequence")

# Metadata functions
def with_meta(obj, meta):
    new_obj = types._clone(obj)
    new_obj.__meta__ = meta
    return new_obj

def meta(obj):
    return getattr(obj, "__meta__", None)


# Atoms functions
def deref(atm):    return atm.val
def reset_BANG(atm,val):
    atm.val = val
    return atm.val
def swap_BANG(atm,f,*args):
    atm.val = f(atm.val,*args)
    return atm.val


ns = {
        '=': types._equal_Q,
        'throw': throw,
        'nil?': types._nil_Q,
        'true?': types._true_Q,
        'false?': types._false_Q,
        'number?': types._number_Q,
        'string?': types._string_Q,
        'symbol': types._symbol,
        'symbol?': types._symbol_Q,
        'keyword': types._keyword,
        'keyword?': types._keyword_Q,
        'fn?': lambda x: (types._function_Q(x) and not hasattr(x, '_ismacro_')),
        'macro?': lambda x: (types._function_Q(x) and
                             hasattr(x, '_ismacro_')),

        'pr-str': pr_str,
        'str': do_str,
        'prn': prn,
        'println': println,
        'readline': core_readline,
        'read-string': reader.read_str,
        'slurp': slurp,
        '<':  operator.lt,
        '<=': operator.le,
        '>':  operator.gt,
        '>=': operator.ge,
        '+':  operator.add,
        '-':  operator.sub,
        '*':  operator.mul,
        '/':  operator.floordiv,
        'time-ms': lambda : int(time.time() * 1000),

        'list': types._list,
        'list?': types._list_Q,
        'vector': types._vector,
        'vector?': types._vector_Q,
        'hash-map': types._hash_map,
        'map?': types._hash_map_Q,
        'assoc': assoc,
        'dissoc': dissoc,
        'get': get,
        'contains?': contains_Q,
        'keys': keys,
        'vals': vals,

        'sequential?': types._sequential_Q,
        'cons': cons,
        'concat': concat,
        'vec': Vector,
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
        'atom': types._atom,
        'atom?': types._atom_Q,
        'deref': deref,
        'reset!': reset_BANG,
        'swap!': swap_BANG,
}
