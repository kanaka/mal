#import sys, traceback
import mal_readline

# read
def READ(str):
    return str

# eval
def EVAL(ast, env):
        #print("EVAL %s" % printer._pr_str(ast))
        return ast

# print
def PRINT(exp):
    return exp

# repl
def REP(str):
    return PRINT(EVAL(READ(str), {}))

def entry_point(argv):
    #mal_readline.init()
    while True:
        try:
            line = mal_readline.readline("user> ")
            if line == "": continue
            print(REP(line))
        except EOFError as e:
            break
        except Exception as e:
            print("Error: %s" % e)
            #print("".join(traceback.format_exception(*sys.exc_info())))
    return 0

# _____ Define and setup target ___
def target(*args):
    return entry_point

# Just run entry_point if not RPython compilation
import sys
if not sys.argv[0].endswith('rpython'):
    entry_point(sys.argv)
