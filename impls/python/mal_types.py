# General functions

def _equal_Q(a, b):
    if _sequential_Q(a):
        return _sequential_Q(b) \
            and len(a) == len(b) \
            and all(_equal_Q(a[k], b[k]) for k in range(len(a)))
    elif _hash_map_Q(a):
        return _hash_map_Q(b) \
            and len(a) == len(b) \
            and all(k in b and _equal_Q(v, b[k]) for k, v in a.items())
    else:
        return type(a) == type(b) \
            and a == b

def _sequential_Q(seq): return _list_Q(seq) or _vector_Q(seq)

#
# Exception type
#

class MalException(Exception):
    def __init__(self, object):
        self.object = object

# Scalars
def _nil_Q(exp):    return exp is None
def _true_Q(exp):   return exp is True
def _false_Q(exp):  return exp is False
def _string_Q(exp):
    return type(exp) == str and not exp.startswith(keywordPrefix)
def _number_Q(exp): return type(exp) == int

# Symbols
class Symbol(str): pass
_symbol = Symbol
def _symbol_Q(exp): return type(exp) == Symbol

# Keywords
# A specially prefixed string
keywordPrefix = '\x7F'
def _keyword(str):
    if str.startswith(keywordPrefix):
        return str
    else:
        return keywordPrefix + str
def _keyword_Q(exp):
    return type(exp) == str and exp.startswith(keywordPrefix)

# Functions
class Function(object):
    """Adding metadata to python standard functions is not easy, so we
    use a wrapper class."""
    def __init__(self, py_fn):
        self._py_fn = py_fn
    def __call__(self, *args):
        return self._py_fn(*args)
class Fn(Function):
    """Function defined with fn*"""
    def __init__(self, py_fn, ast, gen_env):
        super(Fn, self).__init__(py_fn)
        self.ast = ast
        self.gen_env = gen_env
def _function_Q(f):
    return type(f) in (Function, Fn)

class Macro(Function):
    pass
def _macro_Q(exp):
    return type(exp) == Macro

# lists
class List(tuple):
    pass
def _list(*vals): return List(vals)
def _list_Q(exp):   return type(exp) == List


# vectors
class Vector(tuple):
    pass
def _vector(*vals): return Vector(vals)
def _vector_Q(exp): return type(exp) == Vector

# Hash maps
class Hash_Map(dict): pass
def _hash_map(*key_vals):
    return Hash_Map(asPairs(key_vals))
def _hash_map_Q(exp): return type(exp) == Hash_Map

def asPairs(iterable):
    """ k0, v0, k1, v1..  ->  (k0, v0), (k1, v1).. """
    it = iter(iterable)
    while True:
        try:
            k = next(it)
        except StopIteration:
            break
        yield k, next(it)

# atoms
class Atom(object):
    def __init__(self, val):
        self.val = val
_atom = Atom
def _atom_Q(exp):   return type(exp) == Atom

def py_to_mal(obj):
        if type(obj) == list:   return List(obj)
        if type(obj) == tuple:  return List(obj)
        elif type(obj) == dict: return Hash_Map(obj)
        else:                   return obj
