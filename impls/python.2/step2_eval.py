import traceback
from collections.abc import Mapping, Sequence

import mal_readline

from mal_types import (Error, Fn, Form, List,
                       Map, Number, Symbol,
                       Vector, pr_seq)

import reader

Env = Mapping[str, Fn]


def eval_(ast: Form, env: Env) -> Form:
    # print(f'EVAL: {ast}', repr(ast)
    match ast:
        case Symbol():
            if (value := env.get(ast)) is not None:
                return value
            raise Error(f"'{ast}' not found")
        case Map():
            return Map((k, eval_(v, env)) for k, v in ast.items())
        case Vector():
            return Vector(eval_(x, env) for x in ast)
        case List([first, *args]):
            match eval_(first, env):
                case Fn(call):
                    return call(tuple(eval_(x, env) for x in args))
                case not_fun:
                    raise Error(f'cannot apply {not_fun}')
        case _:
            return ast


def add(args: Sequence[Form]) -> Form:
    match args:
        case [Number(left), Number(right)]:
            return Number(left + right)
        case _:
            raise Error('+: bad arguments' + pr_seq(args))


def sub(args: Sequence[Form]) -> Form:
    match args:
        case [Number(left), Number(right)]:
            return Number(left - right)
        case _:
            raise Error('-: bad arguments' + pr_seq(args))


def mul(args: Sequence[Form]) -> Form:
    match args:
        case [Number(left), Number(right)]:
            return Number(left * right)
        case _:
            raise Error('*: bad arguments' + pr_seq(args))


def floordiv(args: Sequence[Form]) -> Form:
    match args:
        case [Number(left), Number(right)]:
            return Number(left // right)
        case _:
            raise Error('/: bad arguments' + pr_seq(args))


def rep(source: str, env: Env) -> str:
    return str(eval_(reader.read(source), env))


def main() -> None:
    repl_env: Env = {
        '+': Fn(add), '-': Fn(sub), '*': Fn(mul), '/': Fn(floordiv),
    }

    while True:
        try:
            print(rep(mal_readline.input_('user> '), repl_env))
        except EOFError:
            break
        # pylint: disable-next=broad-exception-caught
        except Exception as exc:
            traceback.print_exception(exc, limit=10)


if __name__ == '__main__':
    main()
