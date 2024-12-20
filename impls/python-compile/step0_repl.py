import mal_readline

_lisp_prompt = "user> "

def READ(str):
    return str

def EVAL(ast, env):
    return ast

def PRINT(exp):
    return exp

def REP(str):
    return PRINT(EVAL(READ(str), None))

def REPL():
    while True:
        try:
            line = mal_readline.readline(_lisp_prompt)
            print(REP(line))
        except EOFError:
            break

LISP = REPL

if __name__ == "__main__":
    LISP()
