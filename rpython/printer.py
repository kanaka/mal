import sys
IS_RPYTHON = sys.argv[0].endswith('rpython')

if IS_RPYTHON:
    from rpython.rlib.rsre import rsre_re as re
else:
    import re

import mal_types as types
from mal_types import (MalType, MalStr, MalSym, MalInt)

def _pr_str(obj, print_readably=True):
    assert isinstance(obj, MalType)
    _r = print_readably
    if types._list_Q(obj):
        #return "(" + " ".join(map(lambda e: _pr_str(e,_r), obj.values)) + ")"
        res = []
        for e in obj.values:
            res.append(_pr_str(e,_r))
        return u"(" + u" ".join(res) + u")"
##    elif types._vector_Q(obj):                                    
##        return "[" + " ".join(map(lambda e: _pr_str(e,_r), obj)) + "]"
##    elif types._hash_map_Q(obj):
##        ret = []
##        for k in obj.keys():
##            ret.extend((_pr_str(k), _pr_str(obj[k],_r)))
##        return "{" + " ".join(ret) + "}"
    elif types._string_Q(obj):
        assert isinstance(obj, MalStr)
        val = obj.value
        if len(val) > 0 and val[0] == u'\u029e':
            return u':' + val[1:]
        elif print_readably:
            return u'"' + types._replace(u'\\n', u'\\n',
                          types._replace(u'\"', u'\\"',
                           types._replace(u'\\\\', u'\\\\', val))) + u'"'
        else:
            return val
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
    else:
        return u"unknown"

