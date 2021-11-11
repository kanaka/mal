import sys, traceback
import mal_readline

# read
def READ(str):
    return str

# eval
def EVAL(ast):
    return ast

# print
def PRINT(exp):
    return exp

# repl
def REP(str):
    return PRINT(EVAL(READ(str)))

# repl loop
while True:
    try:
        print(REP((raw_input if sys.version_info[0] < 3 else input)("user> ")))
    except EOFError:
        print()
        break
    except Exception:
        traceback.print_exc()
