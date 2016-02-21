import sys, copy, types as pytypes
IS_RPYTHON = sys.argv[0].endswith('rpython')

if IS_RPYTHON:
    from rpython.rlib.listsort import TimSort
else:
    import re

# General functions

class StringSort(TimSort):
    def lt(self, a, b):
        assert isinstance(a, unicode)
        assert isinstance(b, unicode)
        return a < b

def _equal_Q(a, b):
    assert isinstance(a, MalType) and isinstance(b, MalType)
    ota, otb = a.__class__, b.__class__
    if not (ota is otb or (_sequential_Q(a) and _sequential_Q(b))):
        return False
    if isinstance(a, MalSym) and isinstance(b, MalSym):
        return a.value == b.value
    elif isinstance(a, MalStr) and isinstance(b, MalStr):
        return a.value == b.value
    elif isinstance(a, MalInt) and isinstance(b, MalInt):
        return a.value == b.value
    elif _list_Q(a) or _vector_Q(a):
        if len(a) != len(b): return False
        for i in range(len(a)):
            if not _equal_Q(a[i], b[i]): return False
        return True
    elif _hash_map_Q(a):
        assert isinstance(a, MalHashMap)
        assert isinstance(b, MalHashMap)
        akeys = a.dct.keys()
        bkeys = b.dct.keys()
        if len(akeys) != len(bkeys): return False

        StringSort(akeys).sort()
        StringSort(bkeys).sort()
        for i in range(len(akeys)):
            ak, bk = akeys[i], bkeys[i]
            assert isinstance(ak, unicode)
            assert isinstance(bk, unicode)
            if ak != bk: return False
            av, bv = a.dct[ak], b.dct[bk]
            if not _equal_Q(av, bv): return False
        return True
    elif a is b:
        return True
    else:
        throw_str("no = op defined for %s" % a.__class__.__name__)

def _sequential_Q(seq): return _list_Q(seq) or _vector_Q(seq)

def _clone(obj):
    if isinstance(obj, MalFunc):
        return MalFunc(obj.fn, obj.ast, obj.env, obj.params,
                 obj.EvalFunc, obj.ismacro)
    elif isinstance(obj, MalList):
        return obj.__class__(obj.values)
    elif isinstance(obj, MalHashMap):
        return MalHashMap(obj.dct)
    elif isinstance(obj, MalAtom):
        return MalAtom(obj.value)
    else:
        raise Exception("_clone on invalid type")

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


### Parent types
class MalType(): pass
class MalMeta(MalType): pass

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

# Symbols
class MalSym(MalMeta):
    def __init__(self, value):
        assert isinstance(value, unicode)
        self.value = value
        self.meta = nil
def _symbol(strn):
    assert isinstance(strn, unicode)
    return MalSym(strn)
def _symbol_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalSym

# lists
class MalList(MalMeta):
    def __init__(self, vals):
        assert isinstance(vals, list)
        self.values = vals
        self.meta = nil
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
def _list(*vals): return MalList(list(vals))
def _listl(lst): return MalList(lst)
def _list_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalList

### vectors
class MalVector(MalList):
    pass
def _vector(*vals): return MalVector(list(vals))
def _vectorl(lst): return MalVector(lst)
def _vector_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalVector

### hash maps
class MalHashMap(MalMeta):
    def __init__(self, dct):
        self.dct = dct
        self.meta = nil
    def append(self, val):
        self.dct.append(val)
    def __getitem__(self, k):
        assert isinstance(k, unicode)
        if not isinstance(k, unicode):
            throw_str("hash-map lookup by non-string/non-keyword")
        return self.dct[k]
    def __setitem__(self, k, v):
        if not isinstance(k, unicode):
            throw_str("hash-map key must be string or keyword")
        assert isinstance(v, MalType)
        self.dct[k] = v
        return v
def _hash_mapl(kvs):
    dct = {}
    for i in range(0, len(kvs), 2):
        k = kvs[i]
        if not isinstance(k, MalStr):
            throw_str("hash-map key must be string or keyword")
        v = kvs[i+1]
        dct[k.value] = v
    return MalHashMap(dct)
def _hash_map_Q(exp):
    assert isinstance(exp, MalType)
    return exp.__class__ is MalHashMap

# Functions
# env import must happen after MalSym and MalList definitions to allow
# circular dependency
from env import Env
class MalFunc(MalMeta):
    def __init__(self, fn, ast=None, env=None, params=None,
                 EvalFunc=None, ismacro=False):
        if fn is None and EvalFunc is None:
            throw_str("MalFunc requires either fn or EvalFunc")
        self.fn = fn
        self.ast = ast
        self.env = env
        self.params = params
        self.EvalFunc = EvalFunc
        self.ismacro = ismacro
        self.meta = nil
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


# atoms
class MalAtom(MalMeta):
    def __init__(self, value):
        self.value = value
        self.meta = nil
    def get_value(self):
        return self.value
def _atom(val): return MalAtom(val)
def _atom_Q(exp): return exp.__class__ is MalAtom
