import sys, traceback
import mal_readline
import mal_types as types
import reader, printer

# read
def READ(str):
    return reader.read_str(str)

# eval
def eval_ast(ast, env):
    if types._symbol_Q(ast):
        try:
            return env[ast]
        except:
            raise Exception("'" + ast + "' not found")
    elif types._list_Q(ast):
        return types._list(*map(lambda a: EVAL(a, env), ast))
    elif types._vector_Q(ast):
        return types._vector(*map(lambda a: EVAL(a, env), ast))
    elif types._hash_map_Q(ast):
        keyvals = []
        for k in ast.keys():
            keyvals.append(EVAL(k, env))
            keyvals.append(EVAL(ast[k], env))
        return types._hash_map(*keyvals)
    else:
        return ast  # primitive value, return unchanged

def EVAL(ast, env):
        #print("EVAL %s" % printer._pr_str(ast))
        if not types._list_Q(ast):
            return eval_ast(ast, env)

        # apply list
        el = eval_ast(ast, env)
        f = el[0]
        return f(*el[1:])

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
repl_env = {} 
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

repl_env['+'] = lambda a,b: a+b
repl_env['-'] = lambda a,b: a-b
repl_env['*'] = lambda a,b: a*b
repl_env['/'] = lambda a,b: int(a/b)

# repl loop
while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except reader.Blank: continue
    except Exception as e:
        print("".join(traceback.format_exception(*sys.exc_info())))
