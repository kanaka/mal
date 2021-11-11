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
        print(REP((raw_input if sys.version_info[0] < 3 else input)("user> ")))
    except EOFError:
        print()
        break
    except reader.Blank:
        pass
    except Exception:
        traceback.print_exc()
