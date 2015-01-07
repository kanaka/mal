import mal_types as types

def _pr_str(obj, print_readably=True):
    _r = print_readably
    if types._list_Q(obj):
        return "(" + " ".join(map(lambda e: _pr_str(e,_r), obj)) + ")"
    elif types._vector_Q(obj):                                    
        return "[" + " ".join(map(lambda e: _pr_str(e,_r), obj)) + "]"
    elif types._hash_map_Q(obj):
        ret = []
        for k in obj.keys():
            ret.extend((_pr_str(k), _pr_str(obj[k],_r)))
        return "{" + " ".join(ret) + "}"
    elif types._string_Q(obj):
        if len(obj) > 0 and obj[0] == types.u('\u029e'):
            return ':' + obj[1:]
        elif print_readably:
            return '"' + obj.encode('unicode_escape').decode('latin1').replace('"', '\\"') + '"'
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
        return obj.__str__()

