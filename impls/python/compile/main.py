import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer
from env import Env

# debug
from loguru import logger
logger.info("LOGURU as debugger.")
_line_history = []
_ast_history = []
_wake_up_command = ";()"
sys.ps1 = "PY> "
sys.ps2 = "  > "

# read
def READ(str):
    return reader.read_str(str)

def EXEC_COMPILE (ast, env):
    def COMPILE (ast, env):
        logger.debug(f"ast: {ast}")
        if types._symbol_Q(ast):
            compiled_string="_RETURNED_OBJECT = exec_env.get(exec_ast)"
        elif types._list_Q(ast):
            if ast[0] == "def!":
                compiled_string="_RETURNED_OBJECT = exec_env.set(exec_ast[1], EXEC_COMPILE(exec_ast[2], exec_env))"
            else:
                compiled_string="_RETURNED_OBJECT = exec_ast"
        else:
            logger.debug(f"TODO Unsupported Area")
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

# repl
repl_env = Env()
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
