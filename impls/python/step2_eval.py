import operator
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

repl_env['+'] = operator.add
repl_env['-'] = operator.sub
repl_env['*'] = operator.mul
repl_env['/'] = operator.floordiv

# repl loop
while True:
    try:
        print(REP((raw_input if sys.version_info[0] < 3 else input)("user> ")))
    except EOFError:
        print()
        break
    except reader.Blank:
        pass
    except Exception:
        traceback.print_exc()
