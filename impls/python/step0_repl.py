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
        if line == None: break
        if line == "": continue
        print(REP(line))
    except Exception as e:
        traceback.print_exception(*sys.exc_info())
