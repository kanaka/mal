import collections.abc
import dataclasses
import itertools
import time
import operator
import typing
from collections.abc import Callable, Sequence

import mal_readline

from mal_types import (Atom, Boolean, Error, Fn, Form, Keyword, List,
                       Macro, Map, Nil, Number, PythonCall, String,
                       Symbol, ThrownException, Vector, pr_seq)

import reader


ns: dict[str, Form] = {}


def built_in(name: str) -> Callable[[PythonCall], None]:
    """Register in ns and add context to Errors."""

    def decorate(old_f: PythonCall) -> None:

        def new_f(args: Sequence[Form]) -> Form:
            try:
                return old_f(args)
            except Error as exc:
                exc.add_note('The ' + name + ' core function received ['
                             + pr_seq(args) + ' ] as arguments.')
                raise

        ns[name] = Fn(new_f)

    return decorate


def equality(value: Form) -> PythonCall:

    def new_f(args: Sequence[Form]) -> Form:
        match args:
            case [form]:
                return Boolean(form == value)
            case _:
                raise Error('bad arguments')

    return new_f


built_in('nil?')(equality(Nil.NIL))
built_in('false?')(equality(Boolean.FALSE))
built_in('true?')(equality(Boolean.TRUE))


def membership(*classes: type) -> PythonCall:

    def new_f(args: Sequence[Form]) -> Form:
        match args:
            case [form]:
                return Boolean(isinstance(form, classes))
            case _:
                raise Error('bad arguments')

    return new_f


built_in('number?')(membership(Number))
built_in('symbol?')(membership(Symbol))
built_in('keyword?')(membership(Keyword))
built_in('string?')(membership(String))
built_in('list?')(membership(List))
built_in('map?')(membership(Map))
built_in('atom?')(membership(Atom))
built_in('vector?')(membership(Vector))
built_in('macro?')(membership(Macro))
built_in('sequential?')(membership(List, Vector))
built_in('fn?')(membership(Fn))


def arithmetic(old_f: Callable[[int, int], int]) -> PythonCall:

    def new_f(args: Sequence[Form]) -> Form:
        match args:
            case [Number() as left, Number() as right]:
                return Number(old_f(left, right))
            case _:
                raise Error('bad arguments')

    return new_f


built_in('+')(arithmetic(operator.add))
built_in('-')(arithmetic(operator.sub))
built_in('*')(arithmetic(operator.mul))
built_in('/')(arithmetic(operator.floordiv))


def comparison(old_f: Callable[[int, int], bool]) -> PythonCall:

    def new_f(args: Sequence[Form]) -> Form:
        match args:
            case [Number() as left, Number() as right]:
                return Boolean(old_f(left, right))
            case _:
                raise Error('bad arguments')

    return new_f


built_in('<')(comparison(operator.lt))
built_in('<=')(comparison(operator.le))
built_in('>')(comparison(operator.gt))
built_in('>=')(comparison(operator.ge))


@built_in('=')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [left, right]:
            return Boolean(left == right)
        case _:
            raise Error('bad arguments')


built_in('list')(List)
built_in('vector')(Vector)


@built_in('prn')
def _(args: Sequence[Form]) -> Form:
    print(pr_seq(args))
    return Nil.NIL


@built_in('pr-str')
def _(args: Sequence[Form]) -> Form:
    return String(pr_seq(args))


@built_in('println')
def _(args: Sequence[Form]) -> Form:
    print(pr_seq(args, readably=False))
    return Nil.NIL


@built_in('empty?')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() | Vector() as seq]:
            return Boolean(not seq)
        case _:
            raise Error('bad arguments')


@built_in('count')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() | Vector() as seq]:
            return Number(len(seq))
        case [Nil()]:
            return Number(0)
        case _:
            raise Error('bad arguments')


@built_in('read-string')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(line)]:
            return reader.read(line)
        case _:
            raise Error('bad arguments')


@built_in('slurp')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(file_name)]:
            with open(file_name, 'r', encoding='utf-8') as the_file:
                return String(the_file.read())
        case _:
            raise Error('bad arguments')


@built_in('str')
def _(args: Sequence[Form]) -> Form:
    return String(pr_seq(args, readably=False, sep=''))


@built_in('atom')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [form]:
            return Atom(form)
        case _:
            raise Error('bad arguments')


@built_in('deref')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Atom(val)]:
            return val
        case _:
            raise Error('bad arguments')


@built_in('reset!')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Atom() as atm, form]:
            atm.val = form
            return form
        case _:
            raise Error('bad arguments')


@built_in('vec')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() as seq]:
            return Vector(seq)
        case [Vector() as seq]:
            return seq
        case _:
            raise Error('bad arguments')


@built_in('cons')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [head, List() | Vector() as tail]:
            return List((head, *tail))
        case _:
            raise Error('bad arguments')


def cast_sequence(arg: Form) -> List | Vector:
    match arg:
        case List() | Vector():
            return arg
        case _:
            raise Error(f'{arg} is not a sequence')


@built_in('concat')
def _(args: Sequence[Form]) -> Form:
    return List(itertools.chain.from_iterable(cast_sequence(x) for x in args))


@built_in('nth')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() | Vector() as seq, Number() as idx]:
            # Python would accept index = -1.
            if 0 <= idx < len(seq):
                return seq[idx]
            raise Error(f'index {idx} not in range of {seq}')
        case _:
            raise Error('bad arguments')


@built_in('apply')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Fn(call) | Macro(call), *some,
              List() | Vector() as more]:
            return call((*some, *more))
        case _:
            raise Error('bad arguments')


@built_in('map')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Fn(call), List() | Vector() as seq]:
            return List(call((x, )) for x in seq)
        case _:
            raise Error('bad arguments')


@built_in('throw')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [form]:
            raise ThrownException(form)
        case _:
            raise Error('bad arguments')


@built_in('keyword')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(string)]:
            return Keyword(string)
        case [Keyword() as keyword]:
            return keyword
        case _:
            raise Error('bad arguments')


@built_in('symbol')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(string)]:
            return Symbol(string)
        case [Symbol() as symbol]:
            return symbol
        case _:
            raise Error('bad arguments')


@built_in('readline')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(prompt)]:
            try:
                return String(mal_readline.input_(prompt))
            except EOFError:
                return Nil.NIL
        case _:
            raise Error('bad arguments')


@built_in('time-ms')
def _(args: Sequence[Form]) -> Form:
    if args:
        raise Error('bad arguments')
    return Number(time.time() * 1000.0)


@built_in('meta')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Fn() | List() | Vector() | Map() as form]:
            return form.meta
        case _:
            raise Error('bad arguments')


@built_in('with-meta')
def _(args: Sequence[Form]) -> Form:
    #  container = type(container)(container, meta=meta) confuses mypy.
    match args:
        case [List() as container, meta]:
            return List(container, meta=meta)
        case [Vector() as container, meta]:
            return Vector(container, meta=meta)
        case [Map() as container, meta]:
            return Map(container, meta)
        case [Fn() as container, meta]:
            return dataclasses.replace(container, meta=meta)
        case _:
            raise Error('bad arguments')


@built_in('seq')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() as seq]:
            return seq if seq else Nil.NIL
        case [Vector() as seq]:
            return List(seq) if seq else Nil.NIL
        case [String(string)]:
            return List(String(c) for c in string) if string else Nil.NIL
        case [Nil()]:
            return Nil.NIL
        case _:
            raise Error('bad arguments')


@built_in('conj')
def conj(args: Sequence[Form]) -> Form:
    match args:
        case [Vector() as seq, *forms]:
            return Vector((*seq, *forms))
        case [List() as seq, *forms]:
            return List((*reversed(forms), *seq))
        case _:
            raise Error('bad arguments')


@built_in('get')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping, Keyword() | String() as key]:
            return mapping.get(key, Nil.NIL)
        case [Nil(), Keyword() | String()]:
            return Nil.NIL
        case _:
            raise Error('bad arguments')


@built_in('first')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() | Vector() as seq]:
            return seq[0] if seq else Nil.NIL
        case [Nil()]:
            return Nil.NIL
        case _:
            raise Error('bad arguments')


@built_in('rest')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [List() | Vector() as seq]:
            return List(seq[1:])
        case [Nil()]:
            return List()
        case _:
            raise Error('bad arguments')


@built_in('hash-map')
def _(args: Sequence[Form]) -> Form:
    return Map(Map.cast_items(args))


@built_in('assoc')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping, *binds]:
            return Map(itertools.chain(mapping.items(), Map.cast_items(binds)))
        case _:
            raise Error('bad arguments')


@built_in('contains?')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping, Keyword() | String() as key]:
            return Boolean(key in mapping)
        case _:
            raise Error('bad arguments')


@built_in('keys')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping]:
            return List(mapping.keys())
        case _:
            raise Error('bad arguments')


@built_in('vals')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping]:
            return List(mapping.values())
        case _:
            raise Error('bad arguments')


@built_in('dissoc')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Map() as mapping, *keys]:
            result = Map(mapping)
            for key in keys:
                if not isinstance(key, (Keyword, String)):
                    raise Error(f'{key} is not a valid map key')
                if key in result:
                    del result[key]
            return result
        case _:
            raise Error('bad arguments')


@built_in('swap!')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [Atom(old) as atm, Fn(call), *more]:
            new = call((old, *more))
            atm.val = new
            return new
        case _:
            raise Error('bad arguments')


@built_in('py!*')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(python_statement)]:
            # pylint: disable-next=exec-used
            exec(compile(python_statement, '', 'single'), globals())
            return Nil.NIL
        case _:
            raise Error('bad arguments')


def py2mal(obj: typing.Any) -> Form:
    match obj:
        case None:
            return Nil.NIL
        case bool():
            return Boolean(obj)
        case int():
            return Number(obj)
        case str():
            return String(obj)
        case Sequence():
            return List(py2mal(x) for x in obj)
        case collections.abc.Mapping():
            result = Map()
            for py_key, py_val in obj.items():
                key = py2mal(py_key)
                if not isinstance(key, (Keyword, String)):
                    raise Error(f'{key} is not a valid map key')
                result[key] = py2mal(py_val)
            return Map()
        case _:
            raise Error(f'failed to translate {obj}')


@built_in('py*')
def _(args: Sequence[Form]) -> Form:
    match args:
        case [String(python_expression)]:
            # pylint: disable-next=eval-used
            return py2mal(eval(python_expression))
        case _:
            raise Error('bad arguments')
