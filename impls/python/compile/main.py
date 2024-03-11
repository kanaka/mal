import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer

# debug
_line_history = []
_ast_history = []
_wake_up_command = ";()"
sys.ps1 = "PY> "
sys.ps2 = "  > "

# read
def READ(str):
    return reader.read_str(str)

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
def REP(str):
    return PRINT(EVAL(READ(str), {}))

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
