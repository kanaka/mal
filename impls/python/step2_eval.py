import sys, traceback
import mal_readline
import mal_types as types
import reader, printer

# read
READ = reader.read_str

# eval
def EVAL(ast, env):
    # print("EVAL " + printer._pr_str(ast))

    if types._symbol_Q(ast):
        try:
            return env[ast]
        except:
            raise Exception("'" + ast + "' not found")
    elif types._vector_Q(ast):
        return types.Vector(EVAL(a, env) for a in ast)
    elif types._hash_map_Q(ast):
        return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
    elif not types._list_Q(ast) or len(ast) == 0:
        return ast  # primitive value, return unchanged

    # From now on, ast is a non-empty list
    el = (EVAL(e, env) for e in ast)
    f = next(el)
    return f(*el)

# print
PRINT = printer._pr_str

# repl
repl_env = {}
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

repl_env['+'] = lambda a,b: a+b
repl_env['-'] = lambda a,b: a-b
repl_env['*'] = lambda a,b: a*b
repl_env['/'] = lambda a,b: a//b

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
