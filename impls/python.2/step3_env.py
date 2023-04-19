import readline
from typing import Dict

import reader
from env import Env
from mal_types import (
    MalExpression,
    MalBoolean, MalNil, MalSymbol,
    MalInvalidArgumentException,
    MalUnknownSymbolException,
    MalSyntaxException,
)
from mal_types import MalInt, MalList, MalFunctionCompiled, MalVector, MalHash_map

repl_env = Env(None)
repl_env.set("+", MalFunctionCompiled(lambda a: MalInt(a[0].native() + a[1].native())))
repl_env.set("-", MalFunctionCompiled(lambda a: MalInt(a[0].native() - a[1].native())))
repl_env.set("*", MalFunctionCompiled(lambda a: MalInt(a[0].native() * a[1].native())))
repl_env.set(
    "/", MalFunctionCompiled(lambda a: MalInt(int(a[0].native() / a[1].native())))
)


def READ(x: str) -> MalExpression:
    return reader.read(x)


def EVAL(ast: MalExpression, env: Env) -> MalExpression:
    dbgeval = env.get("DEBUG-EVAL")
    if (dbgeval is not None
        and not isinstance(dbgeval, MalNil)
        and (not isinstance(dbgeval, MalBoolean) or dbgeval.native())):
        print("EVAL: " + str(ast))
    if isinstance(ast, MalSymbol):
        key = str(ast)
        val = env.get(key)
        if val is None: raise MalUnknownSymbolException(key)
        return val
    if isinstance(ast, MalVector):
        return MalVector([EVAL(x, env) for x in ast.native()])
    if isinstance(ast, MalHash_map):
        new_dict = {}  # type: Dict[str, MalExpression]
        for key in ast.native():
            new_dict[key] = EVAL(ast.native()[key], env)
        return MalHash_map(new_dict)
    if not isinstance(ast, MalList):
        return ast
    if len(ast.native()) == 0:
        return ast
    first = str(ast.native()[0])
    rest = ast.native()[1:]
    if first == "def!":
        key = str(ast.native()[1])
        value = EVAL(ast.native()[2], env)
        return env.set(key, value)
    if first == "let*":
        assert len(rest) == 2
        let_env = Env(env)
        bindings = rest[0]
        assert isinstance(bindings, MalList) or isinstance(bindings, MalVector)
        bindings_list = bindings.native()
        assert len(bindings_list) % 2 == 0
        for i in range(0, len(bindings_list), 2):
            assert isinstance(bindings_list[i], MalSymbol)
            assert isinstance(bindings_list[i + 1], MalExpression)
            let_env.set(str(bindings_list[i]), EVAL(bindings_list[i + 1], let_env))
        expr = rest[1]
        return EVAL(expr, let_env)
    f, *args = (EVAL(form, env) for form in ast.native())
    try:
        return f.call(args)
    except AttributeError:
        raise MalInvalidArgumentException(f, "attribute error")


def PRINT(x: MalExpression) -> str:
    return str(x)


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
