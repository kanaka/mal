import sys, traceback
import mal_readline
import mal_types as types
import reader, printer

# read
def READ(str):
    return reader.read_str(str)

# eval
def EVAL(ast, env):
        #print("EVAL %s" % printer._pr_str(ast))
        return ast

# print
def PRINT(exp):
    return printer._pr_str(exp)

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
    except reader.Blank: continue
    except Exception as e:
        print("".join(traceback.format_exception(*sys.exc_info())))
