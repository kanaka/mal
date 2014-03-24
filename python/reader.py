import re
from mal_types import (new_symbol, Symbol, new_hash_map, List, new_list, Vector)

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
    tre = re.compile(r"""[\s,]*(~@|[\[\]{}()'`~^@]|"(?:[\\].|[^\\"])*"|;.*|[^\s\[\]{}()'"`@,;]+)""");
    return [t for t in re.findall(tre, str) if t[0] != ';']

def read_atom(reader):
    int_re = re.compile(r"-?[0-9]+$")
    float_re = re.compile(r"-?[0-9][0-9.]*$")
    token = reader.next()
    if re.match(int_re, token):     return int(token)
    elif re.match(float_re, token): return int(token)
    elif token[0] == '"':           return token[1:-1].replace('\\"', '"')
    elif token == "nil":            return None
    elif token == "true":           return True
    elif token == "false":          return False
    else:                           return Symbol(token)

def read_sequence(reader, typ=list, start='(', end=')'):
    ast = typ()
    token = reader.next()
    if token != start: raise Exception("expected '" + start + "'")

    token = reader.peek()
    while token != end:
        if not token: raise Exception("expected '" + end + "', got EOF")
        ast.append(read_form(reader))
        token = reader.peek()
    reader.next()
    return ast

def read_hash_map(reader):
    lst = read_sequence(reader, list, '{', '}')
    return new_hash_map(*lst)

def read_list(reader):
    return read_sequence(reader, List, '(', ')')

def read_vector(reader):
    return read_sequence(reader, Vector, '[', ']')

def read_form(reader):
    token = reader.peek()
    # reader macros/transforms
    if token[0] == ';':
        reader.next()
        return None
    elif token == '\'':
        reader.next()
        return new_list(Symbol('quote'), read_form(reader))
    elif token == '`':
        reader.next()
        return new_list(Symbol('quasiquote'), read_form(reader))
    elif token == '~':
        reader.next()
        return new_list(Symbol('unquote'), read_form(reader))
    elif token == '~@':
        reader.next()
        return new_list(Symbol('splice-unquote'), read_form(reader))
    elif token == '^':
        reader.next()
        meta = read_form(reader)
        return new_list(Symbol('with-meta'), read_form(reader), meta)
    elif token == '@':
        reader.next()
        return new_list(Symbol('deref'), read_form(reader))

    # list
    elif token == ')': raise Exception("unexpected ')'")
    elif token == '(': return read_list(reader)

    # vector
    elif token == ']': raise Exception("unexpected ']'");
    elif token == '[': return read_vector(reader);

    # hash-map
    elif token == '}': raise Exception("unexpected '}'");
    elif token == '{': return read_hash_map(reader);

    # atom
    else:              return read_atom(reader);

def read_str(str):
    tokens = tokenize(str)
    if len(tokens) == 0: raise Blank
    return read_form(Reader(tokens))
