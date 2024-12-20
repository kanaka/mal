#import sys, traceback
import mal_readline
import mal_types as types
from mal_types import (MalSym, MalInt, MalStr,
                       _keywordu,
                       MalList, _list, MalVector, MalHashMap, MalFunc)
import reader, printer

# read
def READ(str):
    return reader.read_str(str)

# eval
def EVAL(ast, env):
    # print(u"EVAL: " + printer._pr_str(ast))
    if types._symbol_Q(ast):
        assert isinstance(ast, MalSym)
        if ast.value in env:
            return env[ast.value]
        else:
            raise Exception(u"'" + ast.value + u"' not found")
    elif types._vector_Q(ast):
        res = []
        for a in ast.values:
            res.append(EVAL(a, env))
        return MalVector(res)
    elif types._hash_map_Q(ast):
        new_dct = {}
        for k in ast.dct.keys():
            new_dct[k] = EVAL(ast.dct[k], env)
        return MalHashMap(new_dct)
    elif not types._list_Q(ast):
        return ast  # primitive value, return unchanged
    else:
        # apply list
        if len(ast) == 0: return ast
        f = EVAL(ast[0], env)
        args_list = []
        for i in range(1, len(ast)):
            args_list.append(EVAL(ast[i], env))
        args = MalList(args_list)
        if isinstance(f, MalFunc):
            return f.apply(args)
        else:
            raise Exception("%s is not callable" % f)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
repl_env = {}
def REP(str, env):
    return PRINT(EVAL(READ(str), env))

def plus(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value+b.value)
def minus(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value-b.value)
def multiply(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(a.value*b.value)
def divide(args):
    a, b = args[0], args[1]
    assert isinstance(a, MalInt)
    assert isinstance(b, MalInt)
    return MalInt(int(a.value/b.value))
repl_env[u'+'] = MalFunc(plus)
repl_env[u'-'] = MalFunc(minus)
repl_env[u'*'] = MalFunc(multiply)
repl_env[u'/'] = MalFunc(divide)

def entry_point(argv):
    while True:
        try:
            line = mal_readline.readline("user> ")
            if line == "": continue
            print(REP(line, repl_env))
        except EOFError as e:
            break
        except reader.Blank:
            continue
        except types.MalException as e:
            print(u"Error: %s" % printer._pr_str(e.object, False))
        except Exception as e:
            print("Error: %s" % e)
            #print("".join(traceback.format_exception(*sys.exc_info())))
    return 0

# _____ Define and setup target ___
def target(*args):
    return entry_point

# Just run entry_point if not RPython compilation
import sys
if not sys.argv[0].endswith('rpython'):
    entry_point(sys.argv)
