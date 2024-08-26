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
        line = mal_readline.readline("user> ")
        print(REP(line))
    except EOFError:
        print()
        break
    except Exception:
        print("".join(traceback.format_exception(*sys.exc_info())))
