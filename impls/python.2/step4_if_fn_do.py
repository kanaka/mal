import readline
from typing import Dict

import core
import reader
from env import Env
from mal_types import (
    MalExpression,
    MalSymbol,
    MalInvalidArgumentException,
    MalUnknownSymbolException,
    MalSyntaxException,
)
from mal_types import (
    MalList,
    MalNil,
    MalBoolean,
    MalFunctionCompiled,
    MalVector,
    MalHash_map,
)

repl_env = Env(None)
for key in core.ns:
    repl_env.set(key, core.ns[key])


def READ(x: str) -> MalExpression:
    return reader.read(x)


def eval_ast(ast: MalExpression, env: Env) -> MalExpression:
    if isinstance(ast, MalSymbol):
        return env.get(ast)
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


def EVAL(ast: MalExpression, env: Env) -> MalExpression:
    # print("EVAL: " + str(ast))
    if not isinstance(ast, MalList):
        return eval_ast(ast, env)
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
    if first == "do":
        for x in range(0, len(rest) - 1):
            EVAL(rest[x], env)
        return EVAL(rest[len(rest) - 1], env)
    if first == "if":
        condition = EVAL(rest[0], env)

        if isinstance(condition, MalNil) or (
            isinstance(condition, MalBoolean) and condition.native() is False
        ):
            if len(rest) >= 3:
                return EVAL(rest[2], env)
            else:
                return MalNil()
        else:
            return EVAL(rest[1], env)
    if first == "fn*":

        def func_body(x):
            func_env = Env(outer=env, binds=rest[0].native(), exprs=x)
            return EVAL(rest[1], func_env)

        return MalFunctionCompiled(func_body)

    evaled_ast = eval_ast(ast, env)
    f = evaled_ast.native()[0]
    args = evaled_ast.native()[1:]
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
