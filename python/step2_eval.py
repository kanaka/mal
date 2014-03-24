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
        return env[ast]
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
    el = eval_ast(ast, env)
    f = el[0]
    return f(*el[1:])

def PRINT(exp):
    return pr_str(exp)

# repl
repl_env = {} 
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

repl_env['+'] = lambda a,b: a+b
repl_env['-'] = lambda a,b: a-b
repl_env['*'] = lambda a,b: a*b
repl_env['/'] = lambda a,b: a/b

while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except Blank: continue
    except Exception as e:
        print "".join(traceback.format_exception(*sys.exc_info()))
