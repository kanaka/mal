import sys, traceback
import mal_readline
import mal_types as types
import reader, printer
from env import Env

# read
READ = reader.read_str

# eval
def EVAL(ast, env):
    dbgeval = env.get(types._symbol('DEBUG-EVAL'), return_nil=True)
    if dbgeval is not None and dbgeval is not False:
        print('EVAL: ' + printer._pr_str(ast))

    if types._symbol_Q(ast):
        return env.get(ast)
    elif types._vector_Q(ast):
        return types.Vector(EVAL(a, env) for a in ast)
    elif types._hash_map_Q(ast):
        return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
    elif not types._list_Q(ast):
        return ast  # primitive value, return unchanged
    else:

        # apply list
        if len(ast) == 0: return ast
        a0 = ast[0]

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

    f = EVAL(a0, env)
    if types._function_Q(f):
            args = ast[1:]
            return f(*(EVAL(a, env) for a in args))
    else:
        raise Exception('Can only apply functions')

# print
PRINT = printer._pr_str

# repl
repl_env = Env()
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

repl_env.set(types._symbol('+'), lambda a,b: a+b)
repl_env.set(types._symbol('-'), lambda a,b: a-b)
repl_env.set(types._symbol('*'), lambda a,b: a*b)
repl_env.set(types._symbol('/'), lambda a,b: a//b)

# repl loop
while True:
    try:
        line = mal_readline.readline("user> ")
        print(REP(line))
    except EOFError:
        print()
        break
    except reader.Blank: continue
    except Exception:
        print("".join(traceback.format_exception(*sys.exc_info())))
