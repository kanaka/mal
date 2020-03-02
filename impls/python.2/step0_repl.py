import readline


def READ(x: str) -> str:
    return x


def EVAL(x: str) -> str:
    return x


def PRINT(x: str) -> str:
    return x


def rep(x: str) -> str:
    return PRINT(EVAL(READ(x)))


# repl loop
eof: bool = False
while not eof:
    try:
        line = input("user> ")
        readline.add_history(line)
        print(rep(line))
    except EOFError:
        eof = True
