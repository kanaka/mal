import sys, traceback
import mal_readline
from mal_types import (pr_str, sequential_Q, symbol_Q, coll_Q, list_Q,
                       vector_Q, hash_map_Q, new_symbol, new_function,
                       new_list, new_vector, new_hash_map, Env, types_ns)
from reader import (read_str, Blank)

# read
def READ(str):
    return read_str(str)

# eval
def eval_ast(ast, env):
    if symbol_Q(ast):
        return env.get(ast)
    elif list_Q(ast):
        return new_list(*map(lambda a: EVAL(a, env), ast))
    elif vector_Q(ast):
        return new_vector(*map(lambda a: EVAL(a, env), ast))
    elif hash_map_Q(ast):
        keyvals = []
        for k in ast.keys():
            keyvals.append(EVAL(k, env))
            keyvals.append(EVAL(ast[k], env))
        return new_hash_map(*keyvals)
    else:
        return ast  # primitive value, return unchanged

def EVAL(ast, env):
    #print("EVAL %s" % ast)
    if not list_Q(ast):
        return eval_ast(ast, env)

    # apply list
    if len(ast) == 0: return ast
    a0 = ast[0]

    if "def!" == a0:
        a1, a2 = ast[1], ast[2]
        res = EVAL(a2, env)
        return env.set(a1, res)
    elif "let*" == a0:
        a1, a2 = ast[1], ast[2]
        let_env = Env(env)
        for i in range(0, len(a1), 2):
            let_env.set(a1[i], EVAL(a1[i+1], let_env))
        return EVAL(a2, let_env)
    else:
        el = eval_ast(ast, env)
        f = el[0]
        return f(*el[1:])

# print
def PRINT(exp):
    return pr_str(exp)

# repl
repl_env = Env()
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))
def _ref(k,v): repl_env.set(k, v)

_ref('+', lambda a,b: a+b)
_ref('-', lambda a,b: a-b)
_ref('*', lambda a,b: a*b)
_ref('/', lambda a,b: a/b)

while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except Blank: continue
    except Exception as e:
        print "".join(traceback.format_exception(*sys.exc_info()))
