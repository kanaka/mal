from itertools import chain

import mal_types as types

def _escape(s):
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')

def pr_list(iterable, separator, readably):
    return separator.join(_pr_str(exp, readably) for exp in iterable)

def _pr_str(obj, readably=True):
    if types._list_Q(obj):
        return "(" + pr_list(obj, " ", readably) + ")"
    elif types._vector_Q(obj):
        return "[" + pr_list(obj, " ", readably) + "]"
    elif types._hash_map_Q(obj):
        return "{" \
            + pr_list(chain.from_iterable(obj.items()), " ", readably) \
            + "}"
    elif types._string_Q(obj) and readably:
        return '"' + _escape(obj) + '"'
    elif types._keyword_Q(obj):
        return ':' + obj[1:]
    elif types._nil_Q(obj):
        return "nil"
    elif types._true_Q(obj):
        return "true"
    elif types._false_Q(obj):
        return "false"
    elif types._atom_Q(obj):
        return "(atom " + _pr_str(obj.val, readably) + ")"
    else:
        return str(obj)
