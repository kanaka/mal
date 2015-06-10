import sys, copy, types as pytypes

# General functions

def _equal_Q(a, b):
    assert isinstance(a, MalType) and isinstance(b, MalType)
    ota, otb = a.__class__, b.__class__
    if not (ota is otb or (_sequential_Q(a) and _sequential_Q(b))):
        return False
    if _symbol_Q(a):
        assert isinstance(a, MalSym) and isinstance(b, MalSym)
        return a.value == b.value
    elif _string_Q(a):
        assert isinstance(a, MalStr) and isinstance(b, MalStr)
        return a.value == b.value
    elif _int_Q(a):
        assert isinstance(a, MalInt) and isinstance(b, MalInt)
        return a.value == b.value
##    elif _list_Q(a) or _vector_Q(a):
    elif _list_Q(a):
        if len(a) != len(b): return False
        for i in range(len(a)):
            if not _equal_Q(a[i], b[i]): return False
        return True
##    elif _hash_map_Q(a):
##        akeys = a.keys()
##        akeys.sort()
##        bkeys = b.keys()
##        bkeys.sort()
##        if len(akeys) != len(bkeys): return False
##        for i in range(len(akeys)):
##            if akeys[i] != bkeys[i]: return False
##            if not equal_Q(a[akeys[i]], b[bkeys[i]]): return False
##        return True
    elif a is b:
    #elif ((a is nil and a is nil) or (a is true and b is true) or (a
    #    is false and b is false)):
        return True
    else:
        throw_str("no = op defined for %s" % a.__class__.__name__)

##def _sequential_Q(seq): return _list_Q(seq) or _vector_Q(seq)
def _sequential_Q(seq): return _list_Q(seq)

##def _clone(obj):
##    #if type(obj) == type(lambda x:x):
##    if type(obj) == pytypes.FunctionType:
##        if obj.__code__:
##            return pytypes.FunctionType(
##                    obj.__code__, obj.__globals__, name = obj.__name__,
##                    argdefs = obj.__defaults__, closure = obj.__closure__)
##        else:
##            return pytypes.FunctionType(
##                    obj.func_code, obj.func_globals, name = obj.func_name,
##                    argdefs = obj.func_defaults, closure = obj.func_closure)
##    else:
##        return copy.copy(obj)

def _replace(match, sub, old_str):
    new_str = u""
    idx = 0
    while idx < len(old_str):
        midx = old_str.find(match, idx)
        if midx < 0: break
        assert midx >= 0 and midx < len(old_str)
        new_str = new_str + old_str[idx:midx]
        new_str = new_str + sub
        idx = midx + len(match)
    new_str = new_str + old_str[idx:]
    return new_str

#
# Mal Types
#

class MalException(Exception):
    def __init__(self, object):
        self.object = object

def throw_str(s):
    raise MalException(MalStr(unicode(s)))


### Parent type
class MalType(): pass

### Scalars
class MalNil(MalType): pass
nil = MalNil()
def _nil_Q(exp):
    assert isinstance(exp, MalType)
    return exp is nil

class MalTrue(MalType): pass
true = MalTrue()
def _true_Q(exp):
    assert isinstance(exp, MalType)
    return exp is true

class MalFalse(MalType): pass
false = MalFalse()
def _false_Q(exp):
    assert isinstance(exp, MalType)
    return exp is false

# Numbers
class MalInt(MalType):
    def __init__(self, value):
        assert isinstance(value, int)
        self.value = value
def _int_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalInt

# String
class MalStr(MalType):
    def __init__(self, value):
        assert isinstance(value, unicode)
        self.value = value
    def __len__(self):
        return len(self.value)
def _string_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__  is MalStr

# Symbols
class MalSym(MalType):
    def __init__(self, value):
        assert isinstance(value, unicode)
        self.value = value
def _symbol(strn):
    assert isinstance(strn, unicode)
    return MalSym(strn)
def _symbol_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalSym

# Keywords
# A specially prefixed string
def _keyword(mstr):
    assert isinstance(mstr, MalType)
    if isinstance(mstr, MalStr):
        val = mstr.value
        if val[0] == u"\u029e": return mstr
        else:                   return MalStr(u"\u029e" + val)
    else:
        throw_str("_keyword called on non-string")
# Create keyword from unicode string
def _keywordu(strn):
    assert isinstance(strn, unicode)
    return MalStr(u"\u029e" + strn)
def _keyword_Q(exp):
    if isinstance(exp, MalStr):
        return exp.value[0] == u"\u029e"
    else:
        return False

# lists
class MalList(MalType):
    def __init__(self, vals):
        assert isinstance(vals, list)
        self.values = vals
    def append(self, val):
        self.values.append(val)
    def rest(self):
        return MalList(self.values[1:])
    def __len__(self):
        return len(self.values)
    def __getitem__(self, i):
        assert isinstance(i, int)
        return self.values[i]
    def slice(self, start):
        return MalList(self.values[start:len(self.values)])
    def slice2(self, start, end):
        assert end >= 0
        return MalList(self.values[start:end])
##    def __add__(self, rhs): return List(list.__add__(self, rhs))
##    def __getitem__(self, i):
##        if type(i) == slice: return List(list.__getitem__(self, i))
##        elif i >= len(self): return None
##        else:                return list.__getitem__(self, i)
def _list(*vals): return MalList(list(vals))
def _listl(l): return MalList(l.values)
#def _list_Q(exp): return exp.__class__ == MalList
def _list_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalList


### vectors
##class Vector(list):
##    def __add__(self, rhs): return Vector(list.__add__(self, rhs))
##    def __getitem__(self, i):
##        if type(i) == slice: return Vector(list.__getitem__(self, i))
##        elif i >= len(self): return None
##        else:                return list.__getitem__(self, i)
##    def __getslice__(self, *a): return Vector(list.__getslice__(self, *a))
##def _vector(*vals): return Vector(vals)
##def _vector_Q(exp): return type(exp) == Vector
##
### Hash maps
##class Hash_Map(dict): pass
##def _hash_map(*key_vals):
##    hm = Hash_Map()
##    for i in range(0,len(key_vals),2): hm[key_vals[i]] = key_vals[i+1]
##    return hm
##def _hash_map_Q(exp): return type(exp) == Hash_Map

# Functions
# env import must happen after MalSym and MalList definitions to allow
# circular dependency
from env import Env
class MalFunc(MalType):
    def __init__(self, fn, ast=None, env=None, params=None,
                 EvalFunc=None, ismacro=False):
        if fn is None and EvalFunc is None:
            throw_str("MalFunc requires either fn or EvalFunc")
        self.fn = fn
        #assert isinstance(ast, MalType) or ast is None
        self.ast = ast
        self.env = env
        self.params = params
        self.EvalFunc = EvalFunc
        self.ismacro = ismacro
    def apply(self, args):
        if self.EvalFunc:
            return self.EvalFunc(self.ast, self.gen_env(args))
        else:
            return self.fn(args)
    def gen_env(self, args):
        return Env(self.env, self.params, args)
def _function_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalFunc

##
### atoms
##class Atom(object):
##    def __init__(self, val):
##        self.val = val
##def _atom(val): return Atom(val)
##def _atom_Q(exp):   return type(exp) == Atom
