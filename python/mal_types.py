import copy
from itertools import chain

# General functions

def _pr_str(obj, print_readably=True):
    _r = print_readably
    if list_Q(obj):
        return "(" + " ".join(map(lambda e: _pr_str(e,_r), obj)) + ")"
    elif vector_Q(obj):                                    
        return "[" + " ".join(map(lambda e: _pr_str(e,_r), obj)) + "]"
    elif hash_map_Q(obj):
        ret = []
        for k in obj.keys():
            ret.extend((_pr_str(k), _pr_str(obj[k],_r)))
        return "{" + " ".join(ret) + "}"
    elif string_Q(obj):
        if print_readably:
            return '"' + obj.encode('unicode_escape').replace('"', '\\"') + '"'
        else:
            return obj
    elif nil_Q(obj):
        return "nil"
    elif true_Q(obj):
        return "true"
    elif false_Q(obj):
        return "false"
    elif atom_Q(obj):
        return "(atom " + _pr_str(obj.val,_r) + ")"
    else:
        return obj.__str__()

def pr_str(*args):
    return " ".join(map(lambda exp: _pr_str(exp, True), args))

def do_str(*args):
    return "".join(map(lambda exp: _pr_str(exp, False), args))

def prn(*args):
    print " ".join(map(lambda exp: _pr_str(exp, True), args))
    return None

def println(*args):
    line = " ".join(map(lambda exp: _pr_str(exp, False), args))
    print line.replace('\\n', '\n')
    return None

def with_meta(obj, meta):
    new_obj = copy.copy(obj)
    new_obj.__meta__ = meta
    return new_obj

def meta(obj):
    if hasattr(obj, "__meta__"): return obj.__meta__
    else:                        return None

def equal_Q(a, b):
    ota, otb = type(a), type(b)
    if not (ota == otb or (sequential_Q(a) and sequential_Q(b))):
        return False;
    if symbol_Q(a):
        return a == b
    elif list_Q(a) or vector_Q(a):
        if len(a) != len(b): return False
        for i in range(len(a)):
            if not equal_Q(a[i], b[i]): return False
        return True
    elif hash_map_Q(a):
        akeys = a.keys()
        akeys.sort()
        bkeys = b.keys()
        bkeys.sort()
        if len(akeys) != len(bkeys): return False
        for i in range(len(akeys)):
            if akeys[i] != bkeys[i]: return False
            if not equal_Q(a[akeys[i]], b[bkeys[i]]): return False
        return True
    else:
        return a == b

# nil, true, false
def nil_Q(exp):    return exp is None
def true_Q(exp):   return exp is True
def false_Q(exp):  return exp is False
def string_Q(exp): return type(exp) in [str, unicode]

# numbers
int_plus =     lambda a,b: a+b
int_minus =    lambda a,b: a-b
int_multiply = lambda a,b: a*b
int_divide =   lambda a,b: a/b
int_lt =       lambda a,b: a<b
int_lte =      lambda a,b: a<=b
int_gt =       lambda a,b: a>b
int_gte =      lambda a,b: a>=b

# symbols
class Symbol(str): pass
def new_symbol(str): return Symbol(str)
def symbol_Q(exp): return type(exp) == Symbol


# functions
def new_function(func, exp, env, params):
    def f(*args):
        return func(exp, Env(env, params, args))
    f.__meta__ = {"exp": exp, "env": env, "params": params}
    return f
def function_Q(f): return type(f) == type(function_Q)

# hash maps
class Hash_Map(dict): pass
def new_hash_map(*key_vals):
    hm = Hash_Map()
    for i in range(0,len(key_vals),2): hm[key_vals[i]] = key_vals[i+1]
    return hm
def hash_map_Q(exp): return type(exp) == Hash_Map

def assoc(src_hm, *key_vals):
    hm = copy.copy(src_hm)
    for i in range(0,len(key_vals),2): hm[key_vals[i]] = key_vals[i+1]
    return hm

def dissoc(src_hm, *keys):
    hm = copy.copy(src_hm)
    for key in keys: del hm[key]
    return hm

def get(hm, key):
    if key in hm:
        return hm[key]
    else:
        return None

def contains_Q(hm, key): return key in hm

def keys(hm): return new_list(*hm.keys())

def vals(hm): return new_list(*hm.values())


# errors/exceptions
def throw(exc): raise Exception(exc)


# lists
class List(list):
    def __add__(self, rhs): return List(list.__add__(self, rhs))
    def __getitem__(self, i):
        if type(i) == slice: return List(list.__getitem__(self, i))
        elif i >= len(self): return None
        else:                return list.__getitem__(self, i)
    def __getslice__(self, *a): return List(list.__getslice__(self, *a))
def new_list(*vals): return List(vals)
def list_Q(exp):   return type(exp) == List


# vectors
class Vector(list):
    def __add__(self, rhs): return Vector(list.__add__(self, rhs))
    def __getitem__(self, i):
        if type(i) == slice: return Vector(list.__getitem__(self, i))
        elif i >= len(self): return None
        else:                return list.__getitem__(self, i)
    def __getslice__(self, *a): return Vector(list.__getslice__(self, *a))
def new_vector(*vals): return Vector(vals)
def vector_Q(exp): return type(exp) == Vector


# atoms
class Atom(object):
    def __init__(self, val):
        self.val = val
def new_atom(val): return Atom(val)
def atom_Q(exp):   return type(exp) == Atom
def deref(atm):    return atm.val
def reset_BANG(atm,val):
    atm.val = val
    return atm.val
def swap_BANG(atm,f,*args):
    atm.val = f(atm.val,*args)
    return atm.val



# Sequence operations
def sequential_Q(seq): return list_Q(seq) or vector_Q(seq)

def coll_Q(coll): return sequential_Q(coll) or hash_map_Q(coll)

def cons(x, seq): return List([x]) + List(seq)

def nth(lst, idx): return lst[idx]

def count(lst): return len(lst)

def empty_Q(lst): return len(lst) == 0

def concat(*lsts): return List(chain(*lsts))

# retains metadata
def conj(lst, *args):
    new_lst = List(lst + list(args))
    if hasattr(lst, "__meta__"):
        new_lst.__meta__ = lst.__meta__
    return new_lst

def first(lst): return lst[0]

def rest(lst): return List(lst[1:])

def apply(f, *args):
    return f(*(list(args[0:-1])+args[-1]))

def mapf(f, lst):
    return List(map(f, lst))


# Environment

class Env():
    def __init__(self, outer=None, binds=None, exprs=None):
        self.data = {}
        self.outer = outer or None

        if binds:
            for i in range(len(binds)):
                if binds[i] == "&":
                    self.data[binds[i+1]] = exprs[i:]
                    break
                else:
                    self.data[binds[i]] = exprs[i]

    def find(self, key):
        if key in self.data: return self
        elif self.outer:     return self.outer.find(key)
        else:                return None

    def set(self, key, value):
        self.data[key] = value
        return value

    def get(self, key):
        env = self.find(key)
        if not env: raise Exception("'" + key + "' not found")
        return env.data[key]

types_ns = { 
        'pr-str': pr_str, 'str': do_str, 'prn': prn, 'println': println,
        'with-meta': with_meta, 'meta': meta,
        '=': equal_Q,
        'nil?': nil_Q, 'true?': true_Q, 'false?': false_Q,
        'symbol?': symbol_Q,
        '<': int_lt, '<=': int_lte, '>': int_gt, '>=': int_gte,
        '+': int_plus, '-': int_minus, '*': int_multiply, '/': int_divide,
        'hash-map': new_hash_map, 'map?': hash_map_Q,
        'assoc': assoc, 'dissoc': dissoc, 'get': get,
        'contains?': contains_Q, 'keys': keys, 'vals': vals,
        'throw': throw,
        'list': new_list, 'list?': list_Q,
        'vector': new_vector, 'vector?': vector_Q,
        'atom': new_atom, 'atom?': atom_Q, 'deref': deref,
        'reset!': reset_BANG, 'swap!': swap_BANG,
        'sequential?': sequential_Q,
        'cons': cons, 'nth': nth, 'count': count, 'empty?': empty_Q,
        'concat': concat, "conj": conj, "first": first, "rest": rest,
        'apply': apply, 'map': mapf}

