#import sys, traceback
import mal_readline
import mal_types as types
from mal_types import (MalSym, MalInt, MalStr,
                       _symbol, _keywordu,
                       nil, false, throw_str,
                       MalList, _list, MalVector, MalHashMap, MalFunc)
import reader, printer
from env import Env

# read
def READ(str):
    return reader.read_str(str)

# eval
def EVAL(ast, env):
    if env.get(u"DEBUG-EVAL") not in (None, nil, false):
        print(u"EVAL: " + printer._pr_str(ast))
    if types._symbol_Q(ast):
        assert isinstance(ast, MalSym)
        value = env.get(ast.value)
        if value is None:
            throw_str("'" + str(ast.value) + "' not found")
        return value
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
        a0 = ast[0]
        if not isinstance(a0, MalSym):
            raise Exception("attempt to apply on non-symbol")

        if u"def!" == a0.value:
            a1, a2 = ast[1], ast[2]
            res = EVAL(a2, env)
            return env.set(a1, res)
        elif u"let*" == a0.value:
            a1, a2 = ast[1], ast[2]
            let_env = Env(env)
            for i in range(0, len(a1), 2):
                let_env.set(a1[i], EVAL(a1[i+1], let_env))
            return EVAL(a2, let_env)
        else:
            f = EVAL(a0, env)
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
repl_env = Env()
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
repl_env.set(_symbol(u'+'), MalFunc(plus))
repl_env.set(_symbol(u'-'), MalFunc(minus))
repl_env.set(_symbol(u'*'), MalFunc(multiply))
repl_env.set(_symbol(u'/'), MalFunc(divide))

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
