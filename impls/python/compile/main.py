import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer
from env import Env

# debug
from loguru import logger
logger.info("LOGURU as debugger.")
_line_history, _ast_history = [], []
sys.ps1, sys.ps2 = "PY> ", "  > "
_wake_up_command = ";()"

# read
def READ(str):
    return reader.read_str(str)

# compile and execute
def EXEC_COMPILE (ast, env):
    def COMPILE (ast, env):
        logger.debug(f"ast: {ast}")
        if types._symbol_Q(ast):
            compiled_string="""
_RETURNED_OBJECT = exec_env.get(exec_ast)
            """
        elif types._list_Q(ast):
            # Special Forms
            if ast[0] == "def!":
                compiled_string="""
_A1 = exec_ast[1]
_A2 = exec_ast[2]
_RETURNED_OBJECT = EXEC_COMPILE(_A2, exec_env)
_RETURNED_OBJECT = exec_env.set(_A1, _RETURNED_OBJECT)
                """
            # Non-Special Forms
            else:
                compiled_string="""
_ARGUMENTS = []
for _exec_sub_ast in exec_ast[1:]:
    _ARGUMENTS.append(EVAL(_exec_sub_ast, exec_env))
_OPERATOR = EVAL(exec_ast[0], exec_env)
_RETURNED_OBJECT = _OPERATOR(*_ARGUMENTS)
                """
        elif types._vector_Q(ast): # TODO
            logger.debug(f"Unsupported Type: {type(ast)}")
            compiled_string="_RETURNED_OBJECT = exec_ast"
        elif types._hash_map_Q(ast): # TODO
            logger.debug(f"Unsupported Type: {type(ast)}")
            compiled_string="_RETURNED_OBJECT = exec_ast"
        else:
            compiled_string="_RETURNED_OBJECT = exec_ast"
        logger.debug(f"compiled_string: {compiled_string}")
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
    # return ast
    return EXEC_COMPILE(ast, env)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# environment
repl_env = Env()

# arithmetic
def multiply (args):
    if args == []:
        return 1
    if len(args) == 1:
        return args[0]
    else:
        return args[0] * multiply(args[1:])
repl_env.set(types._symbol('+'), lambda *args: sum(args))
repl_env.set(types._symbol('-'), lambda *args: args[0] - sum(args[1:]))
repl_env.set(types._symbol('*'), lambda *args: multiply(args))
repl_env.set(types._symbol('/'), lambda a,b: int(a/b))

# repl
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# lisp repl loop
def REPL():
    global _line_history
    logger.info(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline("LISP> ")
            if line == None: break
            if line == "": continue
            if line == _wake_up_command:
                logger.info(f"Hint: Use `LISP()` to get into LISP.")
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
