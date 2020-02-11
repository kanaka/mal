import readline

import reader
from mal_types import MalExpression, MalSyntaxException


def READ(x: str) -> MalExpression:
    return reader.read(x)


def EVAL(x: MalExpression) -> MalExpression:
    return x


def PRINT(x: MalExpression) -> str:
    return str(x)


def rep(x: str) -> str:
    try:
        return PRINT(EVAL(READ(x)))
    except BaseException:
        return "Expression is invalid or unbalanced: " + x


if __name__ == "__main__":
    # repl loop
    eof: bool = False
    while not eof:
        try:
            line = input("user> ")
            readline.add_history(line)
            try:
                print(rep(line))
            except MalSyntaxException as e:
                print("ERROR: invalid syntax: " + str(e))
        except EOFError:
            eof = True
