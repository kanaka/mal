# Named mal_types because 'types' is already a standard python module.

import collections
import dataclasses
import enum
import itertools
import re
import typing
from collections.abc import Callable, Iterable, Iterator, Mapping, Sequence

# The selected representations ensure that the Python == equality
# matches the MAL = equality.

# pr_str is implemented here without printer.py because
# __str__ is idiomatic and gives formatted error messages soon
# (that is, without circular dependencies or evil tricks).
# So there are three ways to format a MAL object.
# str(form)
#     the default, used by pr_seq or format strings like f'{form}'
#     implemented by form.__str__(readably=True)
# form.__str__(readably=False)
#     used by some core functions via pr_seq
#     implemented by form.__str__(readably=False)
# repr(form)
#     the python representation for debugging


class Nil(enum.Enum):
    NIL = None

    def __str__(self, readably: bool = True) -> str:
        return 'nil'


class Boolean(enum.Enum):
    FALSE = False
    TRUE = True

    def __str__(self, readably: bool = True) -> str:
        return 'true' if self is self.TRUE else 'false'


class Number(int):

    def __str__(self, readably: bool = True) -> str:
        return super().__str__()


class Symbol(str):

    def __str__(self, readably: bool = True) -> str:
        # pylint: disable=invalid-str-returned
        return self


# The two other string types are wrapped in dataclasses in order to
# avoid problems with == (symbols) and pattern matching (list and
# vectors).
@dataclasses.dataclass(frozen=True, slots=True)
class String:
    val: str

    @staticmethod
    def _repl(match: re.Match[str]) -> str:
        char = match.group()
        return '\\' + ('n' if char == '\n' else char)

    def __str__(self, readably: bool = True) -> str:
        return '"' + re.sub(r'[\\"\n]', String._repl, self.val) + '"' \
            if readably else self.val


@dataclasses.dataclass(frozen=True, slots=True)
class Keyword:
    val: str

    def __str__(self, readably: bool = True) -> str:
        return ':' + self.val


class List(tuple['Form', ...]):
    # Avoid a name clash with typing.List. This improves mypy output.

    def __init__(self, _: Iterable['Form'] = (),
                 meta: 'Form' = Nil.NIL) -> None:
        """Add a meta field, tuple.__new__ does the rest."""
        self.meta = meta

    def __str__(self, readably: bool = True) -> str:
        return '(' + pr_seq(self, readably) + ')'


class Vector(tuple['Form', ...]):

    def __init__(self, _: Iterable['Form'] = (),
                 meta: 'Form' = Nil.NIL) -> None:
        """Add a meta field, tuple.__new__ does the rest."""
        self.meta = meta

    def __str__(self, readably: bool = True) -> str:
        return '[' + pr_seq(self, readably) + ']'


class Map(dict[Keyword | String, 'Form']):

    def __init__(self,
                 arg: Iterable[tuple[Keyword | String, 'Form']]
                 | Mapping[Keyword | String, 'Form'] = (),
                 meta: 'Form' = Nil.NIL,
                 ) -> None:
        dict.__init__(self, arg)
        self.meta = meta

    def __str__(self, readably: bool = True) -> str:
        return '{' + pr_seq(itertools.chain.from_iterable(self.items()),
                            readably) + '}'

    @staticmethod
    def cast_items(args: Iterable['Form']
                   ) -> Iterator[tuple[Keyword | String, 'Form']]:
        key: Keyword | String | None = None
        for form in args:
            if key:
                yield key, form
                key = None
            elif isinstance(form, (Keyword, String)):
                key = form
            else:
                raise Error(f'{key} is not a valid map key')
        if key:
            raise Error(f'odd count in map binds, no value for {form}')


Env = collections.ChainMap[str, 'Form']
PythonCall = Callable[[Sequence['Form']], 'Form']


class TCOEnv(typing.NamedTuple):
    body: 'Form'
    fenv: Callable[[Sequence['Form']], Env]


@dataclasses.dataclass(frozen=True, slots=True)
class Fn:
    call: PythonCall
    tco_env: TCOEnv | None = None
    meta: 'Form' = Nil.NIL

    def __str__(self, readably: bool = True) -> str:
        return '#<function>'


@dataclasses.dataclass(frozen=True, slots=True)
class Macro:
    call: PythonCall

    def __str__(self, readably: bool = True) -> str:
        return '#<macro>'


@dataclasses.dataclass(slots=True)
class Atom:
    val: 'Form'

    def __str__(self, readably: bool = True) -> str:
        return f'(atom {self.val})'


Form = (Atom | Boolean | Fn | Keyword | Macro | List
        | Map | Nil | Number | String | Symbol | Vector)


class Error(Exception):
    """Local exceptions, as recommended by pylint."""


@dataclasses.dataclass(frozen=True, slots=True)
class ThrownException(Exception):
    form: Form


def pr_seq(args: Iterable[Form], readably: bool = True, sep: str = ' ') -> str:
    # This would be OK if the signature was usual.
    # pylint: disable-next=unnecessary-dunder-call
    return sep.join(x.__str__(readably) for x in args)
