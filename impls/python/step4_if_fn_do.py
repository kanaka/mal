import sys, traceback
import mal_readline
import mal_types as types
import reader, printer
from env import Env
import core

# read
READ = reader.read_str

# eval
def EVAL(ast, env):
    # print("EVAL " + printer._pr_str(ast))

    if types._symbol_Q(ast):
        return env.get(ast)
    elif types._vector_Q(ast):
        return types.Vector(EVAL(a, env) for a in ast)
    elif types._hash_map_Q(ast):
        return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
    elif not types._list_Q(ast) or len(ast) == 0:
        return ast  # primitive value, return unchanged

    # From now on, ast is a non-empty list
    a0 = ast[0]

    # Search special forms
    if types._symbol_Q(a0):
        if "def!" == a0:
            a1, a2 = ast[1], ast[2]
            res = EVAL(a2, env)
            return env.set(a1, res)
        elif "let*" == a0:
            a1, a2 = ast[1], ast[2]
            let_env = Env(env)
            for k, v in types.asPairs(a1):
                let_env.set(k, EVAL(v, let_env))
            return EVAL(a2, let_env)
        elif "do" == a0:
            for i in range(1, len(ast) - 1):
                EVAL(ast[i], env)
            return EVAL(ast[-1], env)
        elif "if" == a0:
            a1, a2 = ast[1], ast[2]
            cond = EVAL(a1, env)
            if cond is None or cond is False:
                if len(ast) > 3:
                    return EVAL(ast[3], env)
                else:
                    return None
            else:
                return EVAL(a2, env)
        elif "fn*" == a0:
            a1, a2 = ast[1], ast[2]
            def f(*args):
                return EVAL(a2, Env(env, a1, args))
            return f


    # a0 is not a special form
    el = (EVAL(e, env) for e in ast)
    f = next(el)
    return f(*el)

# print
PRINT = printer._pr_str

# repl
repl_env = Env()
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# core.py: defined using python
for k, v in core.ns.items(): repl_env.set(types._symbol(k), v)

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))")

# repl loop
while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except reader.Blank: continue
    except Exception as e:
        traceback.print_exception(*sys.exc_info())
