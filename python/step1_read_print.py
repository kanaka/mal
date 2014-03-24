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
def EVAL(ast, env):
    #print("EVAL %s" % ast)
    return ast

def PRINT(exp):
    return pr_str(exp)

# repl
def REP(str):
    return PRINT(EVAL(READ(str), {}))

while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except Blank: continue
    except Exception as e:
        print "".join(traceback.format_exception(*sys.exc_info()))
