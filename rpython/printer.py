import sys
IS_RPYTHON = sys.argv[0].endswith('rpython')

if IS_RPYTHON:
    from rpython.rlib.rsre import rsre_re as re
else:
    import re

import mal_types as types
from mal_types import (MalType, MalStr, MalSym, MalInt)

def _pr_a_str(s, print_readably=True):
    if len(s) > 0 and s[0] == u'\u029e':
        return u':' + s[1:]
    elif print_readably:
        return u'"' + types._replace(u'\\n', u'\\n',
                        types._replace(u'\"', u'\\"',
                        types._replace(u'\\', u'\\\\', s))) + u'"'
    else:
        return s

def _pr_str(obj, print_readably=True):
    assert isinstance(obj, MalType)
    _r = print_readably
    if types._list_Q(obj):
        res = []
        for e in obj.values:
            res.append(_pr_str(e,_r))
        return u"(" + u" ".join(res) + u")"
    elif types._vector_Q(obj):
        res = []
        for e in obj.values:
            res.append(_pr_str(e,_r))
        return u"[" + u" ".join(res) + u"]"
    elif types._hash_map_Q(obj):
        ret = []
        for k in obj.dct.keys():
            ret.append(_pr_a_str(k,_r))
            ret.append(_pr_str(obj.dct[k],_r))
        return u"{" + u" ".join(ret) + u"}"
    elif types._string_Q(obj):
        assert isinstance(obj, MalStr)
        return _pr_a_str(obj.value,_r)
    elif types._nil_Q(obj):
        return u"nil"
    elif types._true_Q(obj):
        return u"true"
    elif types._false_Q(obj):
        return u"false"
##    elif types._atom_Q(obj):
##        return "(atom " + _pr_str(obj.val,_r) + ")"
    elif types._symbol_Q(obj):
        assert isinstance(obj, MalSym)
        return obj.value
    elif types._int_Q(obj):
        assert isinstance(obj, MalInt)
        return unicode(str(obj.value))
    elif types._function_Q(obj):
        return u"#<function>"
    else:
        return u"unknown"

