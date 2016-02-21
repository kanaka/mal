#import sys, traceback
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

def entry_point(argv):
    #mal_readline.init()
    while True:
        try:
            line = mal_readline.readline("user> ")
            if line == "": continue
            print(REP(line))
        except EOFError as e:
            break
        except reader.Blank:
            continue
        except types.MalException as e:
            print(u"Error: %s" % printer._pr_str(e.object, False))
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
