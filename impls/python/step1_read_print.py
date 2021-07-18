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
        if line == None: break
        if line == "": continue
        print(REP(line))
    except reader.Blank: continue
    except Exception as e:
        traceback.print_exception(*sys.exc_info())
