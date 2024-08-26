import re
from collections.abc import Callable, Iterator, Mapping
from re import Match

from mal_types import (Boolean, Error, Form, Keyword, List, Map, Nil,
                       Number, String, Symbol, Vector)

# The `token` decorator adds regular expression groups all along this file.
# The name of a group is the name of the decorated funtion, allowing
# `read_form` to call it when it founds the token.
# The global regular expression is compiled once when the module is loaded.
token_groups: list[str] = []


class Lexer:
    # Consume unnamed groups, but do not report them.
    # Report None at the end of the input.

    def __init__(self, source: str) -> None:
        self._tokens = (t for t in pattern.finditer(source) if t.lastgroup)
        self._peek: Match[str] | None = None
        self.consume()

    def consume(self) -> None:
        try:
            self._peek = next(self._tokens)
        except StopIteration:
            self._peek = None

    def peek(self) -> re.Match[str] | None:
        return self._peek


def token(regex: str):
    """Bind a regular expression to a function in this module. Form constuctor.

    The lexer does not report tokens with None as constructor.
    """

    def decorator(fun: Callable[[Lexer, Match[str]], Form] | None):
        if fun:
            group = f'(?P<{fun.__name__}>{regex})'
        else:
            group = f'(?:{regex})'
        token_groups.append(group)
        return fun

    return decorator


def context(match: Match[str]) -> str:
    """Format some information for error reporting."""
    start_idx = match.start() - 10
    if 0 < start_idx:
        start = '...' + match.string[start_idx:match.start()]
    else:
        start = match.string[:match.start()]
    end_idx = match.end() + 20
    if end_idx < len(match.string):
        end = match.string[match.end():end_idx] + '...'
    else:
        end = match.string[match.end():]
    return f': {start}<BETWEEN THIS>{match.group()}<AND THIS>{end}'


token(r'(?:[\s,]|;[^\n\r]*)+')(None)


def unescape(match: Match[str]) -> str:
    """Map a backslash sequence to a character for strings."""
    char = match.string[match.end() - 1]
    return '\n' if char == 'n' else char


@token(r'"(?:(?:[^"\\]|\\.)*")?')
def string(_: Lexer, tok: Match[str]) -> Form:
    start, end = tok.span()
    if end - start == 1:
        raise Error('read: unbalanced string delimiter' + context(tok))
    return String(re.sub(r'\\.', unescape, tok.string[start + 1:end - 1]))


def read_list(lexer: Lexer, closing: str, pos: Match[str]) -> Iterator[Form]:
    while not ((tok := lexer.peek()) and tok.group() == closing):
        yield read_form(lexer, pos)
    lexer.consume()


@token(r'\(')
def list_start(lexer: Lexer, tok: Match[str]) -> Form:
    return List(read_list(lexer, ')', tok))


@token(r'\[')
def vector_start(lexer: Lexer, tok: Match[str]) -> Form:
    return Vector(read_list(lexer, ']', tok))


@token(r'\{')
def map_start(lexer: Lexer, tok: Match[str]) -> Form:
    return Map(Map.cast_items(read_list(lexer, '}', tok)))


single_macros = {
    "'": 'quote',
    '`': 'quasiquote',
    '@': 'deref',
    '~': 'unquote',
    '~@': 'splice-unquote',
}


@token("['`@]|~@?")
def macro(lexer: Lexer, tok: Match[str]) -> Form:
    return List((Symbol(single_macros[tok.group()]), read_form(lexer, tok)))


@token(r'\^')
def with_meta(lexer: Lexer, tok: Match[str]) -> Form:
    tmp = read_form(lexer, tok)
    return List((Symbol('with-meta'), read_form(lexer, tok), tmp))


@token('[])}]')
def list_end(_: Lexer, tok: Match[str]) -> Form:
    raise Error('read: unbalanced list/vector/map terminator' + context(tok))


@token(r'-?\d+')
def number(_: Lexer, tok: Match[str]) -> Form:
    return Number(tok.group())


almost_symbols: Mapping[str, Form] = {
    'nil': Nil.NIL,
    'false': Boolean.FALSE,
    'true': Boolean.TRUE,
}


@token(r"""[^]\s"'(),;@[^`{}~]+""")
def symbol(_: Lexer, tok: Match[str]) -> Form:
    start, end = tok.span()
    if tok.string[start] == ':':
        return Keyword(tok.string[start + 1:end])
    value = tok.group()
    return almost_symbols.get(value) or Symbol(value)


@token('.')
def should_never_match(lexer: Lexer, tok: Match[str]) -> Form:
    assert False, f'{lexer} {tok}'


def read_form(lexer: Lexer, pos: Match[str] | None) -> Form:
    """Parse a form from `lexer`, reporting errors as if started from `pos`."""
    if (tok := lexer.peek()):
        lexer.consume()
        assert tok.lastgroup, f'{lexer} {tok}'
        assert tok.lastgroup in globals(), f'{lexer} {tok}'
        return globals()[tok.lastgroup](lexer, tok)
    if pos:
        raise Error('read: unbalanced form, started' + context(pos))
    raise Error('read: the whole input was empty')


def read(source: str) -> Form:
    lexer = Lexer(source)
    result = read_form(lexer, None)
    if tok := lexer.peek():
        raise Error('read: trailing items after the form' + context(tok))
    return result


pattern = re.compile('|'.join(token_groups))
