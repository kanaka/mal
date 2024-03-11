import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer
from env import Env

# debug
_line_history = []
_ast_history = []
_wake_up_command = ";()"
sys.ps1 = "PY> "
sys.ps2 = "  > "

# read
def READ(str):
    return reader.read_str(str)

# # TODO Cheat a bit here. Remove later..
# def eval_ast(ast, env):
#     if types._symbol_Q(ast):
#         return env.get(ast)
#     elif types._list_Q(ast):
#         return types._list(*map(lambda a: EVAL(a, env), ast))
#     elif types._vector_Q(ast):
#         return types._vector(*map(lambda a: EVAL(a, env), ast))
#     elif types._hash_map_Q(ast):
#         return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
#     else:
#         return ast  # primitive value, return unchanged

def EXEC_COMPILE (ast, env):
    def COMPILE (ast, env):
        print("[DEBUG] (ast):", ast)
        if types._symbol_Q(ast):
            compiled_string="_RETURNED_OBJECT = exec_env.get(exec_ast)"
        elif not types._list_Q(ast):
            compiled_string="_RETURNED_OBJECT = exec_ast"
        elif ast[0] == "def!":
            compiled_string="_RETURNED_OBJECT = exec_env.set(exec_ast[1], EXEC_COMPILE(exec_ast[2], exec_env))"
        else:
            compiled_string="_RETURNED_OBJECT = exec_ast"
        print("[DEBUG] (compiled_string):", compiled_string)
        return compiled_string

    def EXEC (compiled_string):
        exec_ast, exec_env = ast, env
        global_bindings, local_bindings = globals().copy(), locals().copy()
        exec(compiled_string, global_bindings, local_bindings)
        return local_bindings.get('_RETURNED_OBJECT')

    return EXEC(COMPILE(ast, env))

# eval
def EVAL(ast, env):
    global _ast_history
    _ast_history.append(ast)
    #print("EVAL %s" % printer._pr_str(ast))
    return ast

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
repl_env = Env()
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# lisp repl loop
def REPL():
    global _line_history
    print(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline("LISP> ")
            if line == None: break
            if line == "": continue
            if line == _wake_up_command:
                print(f"Hint: Use `LISP()` to get into LISP.")
                break # debug
            _line_history.append(line) # debug
            print(REP(line))
        except reader.Blank: continue
        except Exception as e:
            print("".join(traceback.format_exception(*sys.exc_info())))

LISP = REPL
REPL()

# python repl loop
code.interact(local=locals())
