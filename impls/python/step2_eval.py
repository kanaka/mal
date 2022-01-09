import sys, traceback
import mal_readline
import mal_types as types
import reader, printer

# read
def READ(str):
    return reader.read_str(str)

# eval
def EVAL(ast, env):
    # print('EVAL: ' + printer._pr_str(ast))

    if types._symbol_Q(ast):
        try:
            return env[ast]
        except:
            raise Exception("'" + ast + "' not found")
    elif types._vector_Q(ast):
        return types._vector(*map(lambda a: EVAL(a, env), ast))
    elif types._hash_map_Q(ast):
        return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
    elif not types._list_Q(ast):
        return ast  # primitive value, return unchanged
    else:

        # apply list
        if len(ast) == 0: return ast
        f = EVAL(ast[0], env)
        args = ast[1:]
        return f(*(EVAL(a, env) for a in args))

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
