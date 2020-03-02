import readline
from typing import Dict

import reader
from mal_types import MalExpression, MalSymbol
from mal_types import MalFunctionCompiled, MalInt
from mal_types import MalList, MalVector, MalHash_map
from mal_types import MalUnknownSymbolException, MalSyntaxException

repl_env = {
    "+": MalFunctionCompiled(lambda a: MalInt(a[0].native() + a[1].native())),
    "-": MalFunctionCompiled(lambda a: MalInt(a[0].native() - a[1].native())),
    "*": MalFunctionCompiled(lambda a: MalInt(a[0].native() * a[1].native())),
    "/": MalFunctionCompiled(lambda a: MalInt(int(a[0].native() / a[1].native()))),
}


def READ(x: str) -> MalExpression:
    return reader.read(x)


def eval_ast(ast: MalExpression, env: Dict[str, MalFunctionCompiled]) -> MalExpression:
    if isinstance(ast, MalSymbol):
        try:
            return env[str(ast)]
        except KeyError:
            raise MalUnknownSymbolException(str(ast))
    if isinstance(ast, MalList):
        return MalList([EVAL(x, env) for x in ast.native()])
    if isinstance(ast, MalVector):
        return MalVector([EVAL(x, env) for x in ast.native()])
    if isinstance(ast, MalHash_map):
        new_dict = {}  # type: Dict[str, MalExpression]
        for key in ast.native():
            new_dict[key] = EVAL(ast.native()[key], env)
        return MalHash_map(new_dict)
    return ast


def EVAL(ast: MalExpression, env: Dict[str, MalFunctionCompiled]) -> MalExpression:
    if not isinstance(ast, MalList):
        return eval_ast(ast, env)
    if len(ast.native()) == 0:
        return ast
    eval_result = eval_ast(ast, env)
    f = eval_result.native()[0]
    args = eval_result.native()[1:]
    return f.call(args)


def PRINT(exp: MalExpression) -> str:
    return str(exp)


def rep(x: str) -> str:
    return PRINT(EVAL(READ(x), repl_env))


if __name__ == "__main__":
    # repl loop
    eof: bool = False
    while not eof:
        try:
            line = input("user> ")
            readline.add_history(line)
            try:
                print(rep(line))
            except MalUnknownSymbolException as e:
                print("'" + e.func + "' not found")
            except MalSyntaxException as e:
                print("ERROR: invalid syntax: " + str(e))
        except EOFError:
            eof = True
