import sys, traceback
import mal_readline
import reader, printer

# read
READ = reader.read_str

# eval
def EVAL(ast):
        return ast

# print
PRINT = printer._pr_str

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
    except reader.Blank: continue
    except Exception:
        print("".join(traceback.format_exception(*sys.exc_info())))
