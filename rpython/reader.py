import sys
IS_RPYTHON = sys.argv[0].endswith('rpython')

if IS_RPYTHON:
    from rpython.rlib.rsre import rsre_re as re
else:
    import re

import mal_types as types
from mal_types import (MalSym, MalInt, MalStr, _keywordu,
                       _list, _listl, _vectorl, _hash_mapl)

class Blank(Exception): pass

class Reader():
    def __init__(self, tokens, position=0):
        self.tokens = tokens
        self.position = position

    def next(self):
        self.position += 1
        return self.tokens[self.position-1]

    def peek(self):
        if len(self.tokens) > self.position:
            return self.tokens[self.position]
        else:
            return None

def tokenize(str):
    re_str = "[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\\\].|[^\\\\\"])*\"|;.*|[^\s\[\]{}()'\"`@,;]+)"
    if IS_RPYTHON:
        tok_re = re_str
    else:
        tok_re = re.compile(re_str)
    return [t for t in re.findall(tok_re, str) if t[0] != ';']

def read_atom(reader):
    if IS_RPYTHON:
        int_re = '-?[0-9]+$'
        float_re = '-?[0-9][0-9.]*$'
    else:
        int_re = re.compile('-?[0-9]+$')
        float_re = re.compile('-?[0-9][0-9.]*$')
    token = reader.next()
    if re.match(int_re, token):     return MalInt(int(token))
##    elif re.match(float_re, token): return int(token)
    elif token[0] == '"':
        end = len(token)-1
        if end < 2:
            return MalStr(u"")
        else:
            return MalStr(types._replace(u'\\"', u'"', unicode(token[1:end])))
    elif token[0] == ':':           return _keywordu(unicode(token[1:]))
    elif token == "nil":            return types.nil
    elif token == "true":           return types.true
    elif token == "false":          return types.false
    else:                           return MalSym(unicode(token))

def read_sequence(reader, start='(', end=')'):
    ast = []
    token = reader.next()
    if token != start: types.throw_str("expected '" + start + "'")

    token = reader.peek()
    while token != end:
        if not token: types.throw_str("expected '" + end + "', got EOF")
        ast.append(read_form(reader))
        token = reader.peek()
    reader.next()
    return ast

def read_list(reader):
    lst = read_sequence(reader, '(', ')')
    return _listl(lst)

def read_vector(reader):
    lst = read_sequence(reader, '[', ']')
    return _vectorl(lst)

def read_hash_map(reader):
    lst = read_sequence(reader, '{', '}')
    return _hash_mapl(lst)

def read_form(reader):
    token = reader.peek()
    # reader macros/transforms
    if token[0] == ';':
        reader.next()
        return None
    elif token == '\'':
        reader.next()
        return _list(MalSym(u'quote'), read_form(reader))
    elif token == '`':
        reader.next()
        return _list(MalSym(u'quasiquote'), read_form(reader))
    elif token == '~':
        reader.next()
        return _list(MalSym(u'unquote'), read_form(reader))
    elif token == '~@':
        reader.next()
        return _list(MalSym(u'splice-unquote'), read_form(reader))
    elif token == '^':
        reader.next()
        meta = read_form(reader)
        return _list(MalSym(u'with-meta'), read_form(reader), meta)
    elif token == '@':
        reader.next()
        return _list(MalSym(u'deref'), read_form(reader))

    # list
    elif token == ')': types.throw_str("unexpected ')'")
    elif token == '(': return read_list(reader)

    # vector
    elif token == ']': types.throw_str("unexpected ']'");
    elif token == '[': return read_vector(reader);

    # hash-map
    elif token == '}': types.throw_str("unexpected '}'");
    elif token == '{': return read_hash_map(reader);

    # atom
    else:              return read_atom(reader);

def read_str(str):
    tokens = tokenize(str)
    if len(tokens) == 0: raise Blank("Blank Line")
    return read_form(Reader(tokens))
