import traceback
from collections.abc import Sequence

import mal_readline

from mal_types import (Boolean, Env, Error, Fn, Form, List,
                       Map, Nil, Number, Symbol,
                       Vector, pr_seq)

import reader


def eval_def(args: Sequence[Form], env: Env) -> Form:
    match args:
        case [Symbol() as key, form]:
            value = eval_(form, env)
            env[key] = value
            return value
        case _:
            raise Error('def!: bad arguments: ' + pr_seq(args))


def eval_let(args: Sequence[Form], env: Env) -> Form:
    match args:
        case [List() | Vector() as binds, form]:
            if len(binds) % 2:
                raise Error('let*: odd bind count: ' + pr_seq(binds))
            let_env = env.new_child()
            for i in range(0, len(binds), 2):
                key = binds[i]
                if not isinstance(key, Symbol):
                    raise Error(f'let*: {key} is not a symbol')
                let_env[key] = eval_(binds[i + 1], let_env)
            return eval_(form, let_env)
        case _:
            raise Error('let*: bad arguments: ' + pr_seq(args))


specials = {
    'def!': eval_def,
    'let*': eval_let,
}


def eval_(ast: Form, env: Env) -> Form:
    if env.get('DEBUG-EVAL') not in (None, Nil.NIL, Boolean.FALSE):
        print(f'EVAL: {ast}')  # , repr(ast))
        for outer in env.maps:
            print('  ENV:', ' '.join(f'{k}: {v}'
                  for k, v in reversed(outer.items()))[:75])
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
            if isinstance(first, Symbol) and (spec := specials.get(first)):
                return spec(args, env)
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
    repl_env = Env({
        '+': Fn(add), '-': Fn(sub), '*': Fn(mul), '/': Fn(floordiv),
    })

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
