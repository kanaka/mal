import sys, traceback
import mal_readline

# read
def READ(str):
    return str

# eval
def EVAL(ast, env):
    # try it as an expression then a statement
    try:
        return eval(ast)
    except SyntaxError:
        exec compile(ast, '', 'single') in globals()
        return None

# print
def PRINT(exp):
    return exp

# repl
def REP(str):
    return PRINT(EVAL(READ(str), {}))

# repl loop
while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except Exception as e:
        print("".join(traceback.format_exception(*sys.exc_info())))
