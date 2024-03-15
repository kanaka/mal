import sys, traceback, code, functools
import mal_readline
import mal_types as types
import reader, printer
import core
from env import Env
from compiler import COMPILE, _consts
from debugger import logger, DEBUG, TEST

sys.setrecursionlimit(100000)
_line_history, _ast_history = [], []
sys.ps1, sys.ps2, _wake_up_command = "[PYTHON]> ", "        > ", ";()"
_lisp_prompt = "user> "
# _lisp_prompt = " (LISP) > "
repl_env = Env()

# read
def READ(str):
    return reader.read_str(str)

def EXEC (compiled_strings, env):
    compiled_strings += ["\nTOP_LEVEL_RETURNED_OBJECT = _blk()(env)\n"] # This is executed later in the for loop, using the ast and env in the input.
    logger.debug(f"[BEGIN] Code to Execute\n")
    for code in compiled_strings:
        logger.debug(f"[.....] Code to Execute\n{code}")
    logger.debug(f"[ END ] Code to Execute\n")
    bindings = globals().copy()
    bindings.update(locals())
    for code in compiled_strings:
        exec(code, bindings, bindings)
    return bindings["TOP_LEVEL_RETURNED_OBJECT"]

# eval
def EVAL(ast, env):
    logger.debug(f"EVAL AST: {ast}\n")
    _ast_history.append(ast)
    return EXEC(COMPILE(ast, env), env)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# lisp repl loop
def REPL():
    global _line_history
    logger.info(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline(_lisp_prompt)
            if line == None: break
            if line == "": continue
            if line == _wake_up_command:
                logger.info(f"Hint: Use `LISP()` to get into LISP.")
                break # debug
            _line_history.append(line) # debug
            print(REP(line))
        except reader.Blank: continue
        except types.MalException as e:
            print("Error:", printer._pr_str(e.object))
        except Exception as e:
            print("".join(traceback.format_exception(*sys.exc_info())))

# load from core
for k, v in core.ns.items(): repl_env.set(types._symbol(k), v)
repl_env.set(types._symbol('eval'), lambda ast: EVAL(ast, repl_env))
repl_env.set(types._symbol('vector'), lambda *vector_elements: types.Vector(vector_elements))
repl_env.set(types._symbol('hashmap'), lambda *dict_pairs: types.Hash_map()) # TODO FIXME
repl_env.set(types._symbol('*ARGV*'), types._list(*sys.argv[2:]))
repl_env.set(types._symbol('debug'), DEBUG)
repl_env.set(types._symbol('test'), TEST)
repl_env.set(types._symbol('set-ismacro'), lambda fn: setattr(fn, '_ismacro_', True))
repl_env.set(types._symbol('unset-ismacro'), lambda fn: setattr(fn, '_ismacro_', False))
repl_env.set(types._symbol('ismacro'), lambda fn: getattr(fn, '_ismacro_', False))
repl_env.set(types._symbol('clone'), types._clone)

REP("(def! *host-language* \"python-compiled\")")
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
REP("(def! defmacro! (fn* (name function-body-ast) (list 'do (list 'def! name (list 'clone function-body-ast)) (list 'set-ismacro name))))") # TODO Rewrite after having quasiquote.
REP("(set-ismacro defmacro!)")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

if len(sys.argv) >= 2:
    REP('(load-file "' + sys.argv[1] + '")')
    sys.exit(0)

LISP = REPL

def main ():
    LISP()

main()
code.interact(local=locals()) # python repl loop
