import functools
import readline
import sys
from typing import List, Dict

import core
import reader
from env import Env
from mal_types import MalExpression, MalSymbol
from mal_types import (
    MalList,
    MalNil,
    MalBoolean,
    MalFunctionCompiled,
    MalFunctionRaw,
    MalAtom,
    MalVector,
    MalHash_map,
)
from mal_types import (
    MalUnknownSymbolException,
    MalSyntaxException,
    MalInvalidArgumentException,
    MalString,
)

repl_env = Env(None)
for key in core.ns:
    repl_env.set(key, core.ns[key])


def eval_func(args: List[MalExpression]) -> MalExpression:
    a0 = args[0]
    assert isinstance(a0, MalExpression)
    return EVAL(a0, repl_env)


repl_env.set("eval", MalFunctionCompiled(lambda args: eval_func(args)))


def swap(args: List[MalExpression]) -> MalExpression:
    atom = args[0]
    assert isinstance(atom, MalAtom)
    func = args[1]
    atom.reset(EVAL(MalList([func, atom.native()] + args[2:]), repl_env))
    return atom.native()


def READ(x: str) -> MalExpression:
    return reader.read(x)


def qq_loop(acc: MalList, elt: MalExpression) -> MalList:
    if isinstance(elt, MalList):
        lst = elt.native()
        if len(lst) == 2:
            fst = lst[0]
            if isinstance(fst, MalSymbol) and fst.native() == u"splice-unquote":
                return MalList([MalSymbol(u"concat"), lst[1], acc])
    return MalList([MalSymbol(u"cons"), quasiquote(elt), acc])

def qq_foldr(xs: List[MalExpression]) -> MalList:
    return functools.reduce(qq_loop, reversed(xs), MalList([]))

def quasiquote(ast: MalExpression) -> MalExpression:
    if isinstance(ast, MalList):
        lst = ast.native()
        if len(lst) == 2:
            fst = lst[0]
            if isinstance(fst, MalSymbol) and fst.native() == u'unquote':
                return lst[1]
        return qq_foldr(lst)
    elif isinstance(ast, MalVector):
        return MalList([MalSymbol("vec"), qq_foldr(ast.native())])
    elif isinstance(ast, MalSymbol) or isinstance(ast, MalHash_map):
        return MalList([MalSymbol("quote"), ast])
    else:
        return ast


def EVAL(ast: MalExpression, env: Env) -> MalExpression:
    while True:
        dbgeval = env.get("DEBUG-EVAL")
        if (dbgeval is not None
            and not isinstance(dbgeval, MalNil)
            and (not isinstance(dbgeval, MalBoolean) or dbgeval.native())):
            print("EVAL: " + str(ast))
        ast_native = ast.native()
        if isinstance(ast, MalSymbol):
            key = str(ast)
            val = env.get(key)
            if val is None: raise MalUnknownSymbolException(key)
            return val
        if isinstance(ast, MalVector):
            return MalVector([EVAL(x, env) for x in ast_native])
        if isinstance(ast, MalHash_map):
            new_dict = {}  # type: Dict[str, MalExpression]
            for key in ast_native:
                new_dict[key] = EVAL(ast_native[key], env)
            return MalHash_map(new_dict)
        if not isinstance(ast, MalList):
            return ast
        elif len(ast_native) == 0:
            return ast

        first_str = str(ast_native[0])
        if first_str == "def!":
            name: str = str(ast_native[1])
            value: MalExpression = EVAL(ast_native[2], env)
            return env.set(name, value)
        elif first_str == "let*":
            assert len(ast_native) == 3
            let_env = Env(env)
            bindings: MalExpression = ast_native[1]
            assert isinstance(bindings, MalList) or isinstance(bindings, MalVector)
            bindings_list: List[MalExpression] = bindings.native()
            assert len(bindings_list) % 2 == 0
            for i in range(0, len(bindings_list), 2):
                assert isinstance(bindings_list[i], MalSymbol)
                assert isinstance(bindings_list[i + 1], MalExpression)
                let_env.set(str(bindings_list[i]), EVAL(bindings_list[i + 1], let_env))
            env = let_env
            ast = ast_native[2]
            continue
        elif first_str == "do":
            for x in range(1, len(ast_native) - 1):
                EVAL(ast_native[x], env)
            ast = ast_native[len(ast_native) - 1]
            continue
        elif first_str == "if":
            condition = EVAL(ast_native[1], env)

            if isinstance(condition, MalNil) or (
                isinstance(condition, MalBoolean) and condition.native() is False
            ):
                if len(ast_native) >= 4:
                    ast = ast_native[3]
                    continue
                else:
                    return MalNil()
            else:
                ast = ast_native[2]
                continue
        elif first_str == "fn*":
            raw_ast = ast_native[2]
            raw_params = ast_native[1]

            def fn(args: List[MalExpression]) -> MalExpression:
                f_ast = raw_ast
                f_env = Env(outer=env, binds=raw_params.native(), exprs=args)
                return EVAL(f_ast, f_env)

            return MalFunctionRaw(fn=fn, ast=raw_ast, params=raw_params, env=env)
        elif first_str == "quote":
            return (
                MalList(ast_native[1].native())
                if isinstance(ast_native[1], MalVector)
                else ast_native[1]
            )
        elif first_str == "quasiquote":
            ast = quasiquote(ast_native[1])
            continue
        else:
            f, *args = (EVAL(form, env) for form in ast_native)
            if isinstance(f, MalFunctionRaw):
                ast = f.ast()

                env = Env(
                    outer=f.env(),
                    binds=f.params().native(),
                    exprs=args,
                )
                continue
            elif isinstance(f, MalFunctionCompiled):
                return f.call(args)
            else:
                raise MalInvalidArgumentException(f, "not a function")


def PRINT(x: MalExpression) -> str:
    return str(x)


def rep(x: str) -> str:
    return PRINT(EVAL(READ(x), repl_env))


if __name__ == "__main__":
    # repl loop
    eof: bool = False
    rep(
        '(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))'
    )
    mal_argv = MalList([MalString(x) for x in sys.argv[2:]])
    repl_env.set("*ARGV*", mal_argv)

    if len(sys.argv) >= 2:
        file_str = sys.argv[1]
        rep('(load-file "' + file_str + '")')
        exit(0)

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
