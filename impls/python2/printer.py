from itertools import chain

import mal_types as types

def _escape(s):
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')

def _pr_str(obj, print_readably=True):
    _r = print_readably
    if types._list_Q(obj):
        return "(" + pr_list(obj, " ", _r) + ")"
    elif types._vector_Q(obj):
        return "[" + pr_list(obj, " ", _r) + "]"
    elif types._hash_map_Q(obj):
        ret = pr_list(chain.from_iterable(obj.items()), " ", _r)
        return "{" + ret + "}"
    elif types._keyword_Q(obj):
        return ':' + obj[1:]
    elif types._string_Q(obj):
        if _r:
            return '"' + _escape(obj) + '"'
        else:
            return obj
    elif types._nil_Q(obj):
        return "nil"
    elif types._true_Q(obj):
        return "true"
    elif types._false_Q(obj):
        return "false"
    elif types._atom_Q(obj):
        return "(atom " + _pr_str(obj.val,_r) + ")"
    else:
        return str(obj)

def pr_list(iterable, separator, readably):
    return separator.join(_pr_str(exp, readably) for exp in iterable)
