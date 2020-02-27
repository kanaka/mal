import time
from typing import List, Union, Dict

import reader
from mal_types import (
    MalInt,
    MalNil,
    MalList,
    MalBoolean,
    MalExpression,
    MalFunctionCompiled,
    MalAtom,
    MalFunctionRaw,
    MalHash_map,
    MalVector,
)
from mal_types import (
    MalInvalidArgumentException,
    MalString,
    MalException,
    MalSymbol,
    MalNotImplementedException,
    MalIndexError,
)


def prn(args: List[MalExpression]) -> MalNil:
    result_string = " ".join(map(lambda x: x.readable_str(), args))
    print(result_string)
    return MalNil()


def pr_str(args: List[MalExpression]) -> MalString:
    result_string = " ".join(map(lambda x: x.readable_str(), args))
    return MalString(result_string)


def println(args: List[MalExpression]) -> MalNil:
    result_string = " ".join(map(lambda x: x.unreadable_str(), args))
    print(result_string)
    return MalNil()


def list_q(x: MalExpression) -> MalBoolean:
    if isinstance(x, MalList):
        return MalBoolean(True)
    return MalBoolean(False)


def empty_q(x: MalExpression) -> MalBoolean:
    if sequential_q(x):
        return MalBoolean(len(x.native()) == 0)
    raise MalInvalidArgumentException(x, "not a list")


def count(x: MalExpression) -> MalInt:
    if isinstance(x, MalList) or isinstance(x, MalVector):
        return MalInt(len(x.native()))
    elif isinstance(x, MalNil):
        return MalInt(0)
    raise MalInvalidArgumentException(x, "not a list")


def equal(a: MalExpression, b: MalExpression) -> MalBoolean:
    if (isinstance(a, MalList) or isinstance(a, MalVector)) and (
        isinstance(b, MalList) or isinstance(b, MalVector)
    ):
        a_native = a.native()
        b_native = b.native()
        if len(a_native) != len(b_native):
            return MalBoolean(False)
        for x in range(0, len(a_native)):
            if not equal(a_native[x], b_native[x]):
                return MalBoolean(False)
        return MalBoolean(True)
    if type(a) == type(b) and a.native() == b.native():
        return MalBoolean(True)
    return MalBoolean(False)


def less(a: MalExpression, b: MalExpression) -> MalBoolean:
    if not isinstance(a, MalInt):
        raise MalInvalidArgumentException(a, "not an int")
    if not isinstance(b, MalInt):
        raise MalInvalidArgumentException(b, "not an int")
    return MalBoolean(a.native() < b.native())


def less_equal(a: MalExpression, b: MalExpression) -> MalBoolean:
    if not isinstance(a, MalInt):
        raise MalInvalidArgumentException(a, "not an int")
    if not isinstance(b, MalInt):
        raise MalInvalidArgumentException(b, "not an int")
    return MalBoolean(a.native() <= b.native())


def read_string(a: MalExpression) -> MalExpression:
    if isinstance(a, MalString):
        result = reader.read(a.native())
        return result
    raise MalInvalidArgumentException(a, "not a string")


def slurp(filename: MalExpression) -> MalString:
    assert isinstance(filename, MalString)
    with open(filename.native(), "r") as the_file:
        contents = the_file.read()
    return MalString(contents)


def core_str(args: List[MalExpression]) -> MalString:
    result = ""
    for a in args:
        result += a.unreadable_str()
    return MalString(result)


def deref_q(atom: MalExpression) -> MalExpression:
    assert isinstance(atom, MalAtom)
    return atom.native()


def reset(atom: MalExpression, val: MalExpression) -> MalExpression:
    assert isinstance(atom, MalAtom)
    atom.reset(val)
    return val


def cons(first: MalExpression, rest: MalExpression) -> MalExpression:
    assert isinstance(rest, MalList) or isinstance(rest, MalVector)
    return MalList([first] + rest.native())


def concat(args: List[MalExpression]) -> MalExpression:
    result_list: List[MalExpression] = []
    for x in args:
        assert isinstance(x, MalList) or isinstance(x, MalVector)
        result_list = result_list + x.native()
    return MalList(result_list)


def not_(expr: MalExpression) -> MalExpression:
    if isinstance(expr, MalNil) or (
        isinstance(expr, MalBoolean) and expr.native() is False
    ):
        return MalBoolean(True)
    else:
        return MalBoolean(False)


def nth(list_: MalExpression, index: MalExpression) -> MalExpression:
    assert isinstance(list_, MalList) or isinstance(list_, MalVector)
    assert isinstance(index, MalInt)
    list_native = list_.native()
    if index.native() > len(list_native) - 1:
        raise MalIndexError(index.native())
    return list_native[index.native()]


def apply(args: List[MalExpression]) -> MalExpression:
    func = args[0]
    assert isinstance(func, MalFunctionCompiled) or isinstance(func, MalFunctionRaw)
    rest_args: List[MalExpression] = []
    for i in range(1, len(args) - 1):
        rest_args.append(args[i])
    last_arg = args[len(args) - 1]
    assert isinstance(last_arg, MalList) or isinstance(last_arg, MalVector)
    rest_args = rest_args + last_arg.native()
    return func.call(rest_args)


def map_(func: MalExpression, map_list: MalExpression) -> MalExpression:
    assert isinstance(func, MalFunctionCompiled) or isinstance(func, MalFunctionRaw)
    assert isinstance(map_list, MalList) or isinstance(map_list, MalVector)
    result_list: List[MalExpression] = []
    for i in range(len(map_list.native())):
        elem = map_list.native()[i]
        result_list.append(func.call([elem]))
    return MalList(result_list)


def throw(exception: MalExpression) -> MalExpression:
    raise MalException(exception)


def nil_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalNil))


def true_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalBoolean) and arg.native())


def false_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalBoolean) and not arg.native())


def symbol_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalSymbol))


def keyword_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalString) and arg.is_keyword())


def keyword(arg: MalExpression) -> MalExpression:
    assert isinstance(arg, MalString)
    if arg.is_keyword():
        return arg
    else:
        return MalString(arg.unreadable_str(), keyword=True)


def symbol(arg: MalExpression) -> MalExpression:
    assert isinstance(arg, MalString)
    return MalSymbol(arg.unreadable_str())


def readline(arg: MalExpression) -> Union[MalString, MalNil]:
    try:
        assert isinstance(arg, MalString)
        line = input(arg.native())
    except EOFError:
        return MalNil()
    return MalString(line)


def not_implemented(func: str) -> MalExpression:
    raise MalNotImplementedException(func)


def get(map: MalExpression, key: MalExpression) -> MalExpression:
    if isinstance(map, MalNil):
        return MalNil()
    if not isinstance(map, MalHash_map):
        raise MalInvalidArgumentException(map, "not a hash map")
    if key.native() in map.native():
        return map.native()[key.native()]
    else:
        return MalNil()


def first(args: List[MalExpression]) -> MalExpression:
    try:
        if isinstance(args[0], MalNil):
            return MalNil()
        return args[0].native()[0]
    except IndexError:
        return MalNil()
    except TypeError:
        raise MalInvalidArgumentException(args[0], "not a list")


def rest(args: List[MalExpression]) -> MalExpression:
    try:
        if isinstance(args[0], MalNil):
            return MalList([])
        return MalList(args[0].native()[1:])
    except TypeError:
        raise MalInvalidArgumentException(args[0], "not a list or vector")


def vector_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalVector))


def map_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalHash_map))


def sequential_q(arg: MalExpression) -> MalExpression:
    return MalBoolean(isinstance(arg, MalList) or isinstance(arg, MalVector))


def vector(args: List[MalExpression]) -> MalExpression:
    return MalVector(args)


def hash_map(args: List[MalExpression]) -> MalExpression:
    assert len(args) % 2 == 0
    map_ = {}  # type: Dict[str, MalExpression]
    for i in range(0, len(args) - 1, 2):
        assert isinstance(args[i], MalString)
        map_[args[i].native()] = args[i + 1]
    return MalHash_map(map_)


def assoc(args: List[MalExpression]) -> MalExpression:
    if len(args) == 0:
        raise MalInvalidArgumentException(MalNil(), "no arguments supplied to assoc")
    elif len(args) == 1:
        return args[0]
    if not isinstance(args[0], MalHash_map):
        raise MalInvalidArgumentException(args[0], "not a hash map")
    dict_a_copy: Dict[str, MalExpression] = args[0].native().copy()
    dict_b: Dict[str, MalExpression] = hash_map(args[1:]).native()
    for key in dict_b:
        dict_a_copy[key] = dict_b[key]
    return MalHash_map(dict_a_copy)


def contains_q(args: List[MalExpression]) -> MalExpression:
    if len(args) < 2:
        raise MalInvalidArgumentException(MalNil(), "contains? requires two arguments")
    if not isinstance(args[0], MalHash_map):
        raise MalInvalidArgumentException(args[0], "not a hash-map")
    if not isinstance(args[1], MalString):
        return MalBoolean(False)
    return MalBoolean(args[1].native() in args[0].native())


def keys(args: List[MalExpression]) -> MalExpression:
    if len(args) != 1:
        raise MalInvalidArgumentException(
            MalNil(), "keys requires exactly one argument"
        )
    if not isinstance(args[0], MalHash_map):
        raise MalInvalidArgumentException(args[0], "not a hash map")
    return MalList([MalString(x, is_already_encoded=True) for x in args[0].native()])


def vals(args: List[MalExpression]) -> MalExpression:
    if len(args) != 1:
        raise MalInvalidArgumentException(
            MalNil(), "vals requires exactly one argument"
        )
    if not isinstance(args[0], MalHash_map):
        raise MalInvalidArgumentException(args[0], "not a hash map")
    return MalList([args[0].native()[x] for x in args[0].native()])


def dissoc(args: List[MalExpression]) -> MalExpression:
    if len(args) == 0:
        raise MalInvalidArgumentException(MalNil(), "no arguments supplied to dissoc")
    elif len(args) == 1:
        return args[0]
    if not isinstance(args[0], MalHash_map):
        raise MalInvalidArgumentException(args[0], "not a hash map")
    dict_a_copy: Dict[str, MalExpression] = args[0].native().copy()
    list_b: List[MalExpression] = MalList(args[1:]).native()
    for key in list_b:
        try:
            del dict_a_copy[key.unreadable_str()]
        except KeyError:
            pass
    return MalHash_map(dict_a_copy)


def swap(args: List[MalExpression]) -> MalExpression:
    atom = args[0]
    assert isinstance(atom, MalAtom)
    func = args[1]
    assert isinstance(func, MalFunctionCompiled) or isinstance(func, MalFunctionRaw)
    atom.reset(func.call([atom.native()] + args[2:]))
    return atom.native()


ns = {
    "+": MalFunctionCompiled(lambda args: MalInt(args[0].native() + args[1].native())),
    "-": MalFunctionCompiled(lambda args: MalInt(args[0].native() - args[1].native())),
    "*": MalFunctionCompiled(lambda args: MalInt(args[0].native() * args[1].native())),
    "/": MalFunctionCompiled(
        lambda args: MalInt(int(args[0].native() / args[1].native()))
    ),
    "prn": MalFunctionCompiled(lambda args: prn(args)),
    "pr-str": MalFunctionCompiled(lambda args: pr_str(args)),
    "println": MalFunctionCompiled(lambda args: println(args)),
    "list": MalFunctionCompiled(lambda args: MalList(args)),
    "list?": MalFunctionCompiled(lambda args: list_q(args[0])),
    "empty?": MalFunctionCompiled(lambda args: empty_q(args[0])),
    "count": MalFunctionCompiled(lambda args: count(args[0])),
    "=": MalFunctionCompiled(lambda args: equal(args[0], args[1])),
    "<": MalFunctionCompiled(lambda args: less(args[0], args[1])),
    "<=": MalFunctionCompiled(lambda args: less_equal(args[0], args[1])),
    ">": MalFunctionCompiled(lambda args: less(args[1], args[0])),
    ">=": MalFunctionCompiled(lambda args: less_equal(args[1], args[0])),
    "read-string": MalFunctionCompiled(lambda args: read_string(args[0])),
    "slurp": MalFunctionCompiled(lambda args: slurp(args[0])),
    "str": MalFunctionCompiled(lambda args: core_str(args)),
    "atom": MalFunctionCompiled(lambda args: MalAtom(args[0])),
    "atom?": MalFunctionCompiled(lambda args: MalBoolean(isinstance(args[0], MalAtom))),
    "deref": MalFunctionCompiled(lambda args: deref_q(args[0])),
    "reset!": MalFunctionCompiled(lambda args: reset(args[0], args[1])),
    "cons": MalFunctionCompiled(lambda args: cons(args[0], args[1])),
    "concat": MalFunctionCompiled(concat),
    "not": MalFunctionCompiled(lambda args: not_(args[0])),
    "nth": MalFunctionCompiled(lambda args: nth(args[0], args[1])),
    "apply": MalFunctionCompiled(lambda args: apply(args)),
    "map": MalFunctionCompiled(lambda args: map_(args[0], args[1])),
    "throw": MalFunctionCompiled(lambda args: throw(args[0])),
    "nil?": MalFunctionCompiled(lambda args: nil_q(args[0])),
    "true?": MalFunctionCompiled(lambda args: true_q(args[0])),
    "false?": MalFunctionCompiled(lambda args: false_q(args[0])),
    "symbol": MalFunctionCompiled(lambda args: symbol(args[0])),
    "symbol?": MalFunctionCompiled(lambda args: symbol_q(args[0])),
    "readline": MalFunctionCompiled(lambda args: readline(args[0])),
    "time-ms": MalFunctionCompiled(lambda args: MalInt(int(time.time() * 1000))),
    "meta": MalFunctionCompiled(lambda args: not_implemented("meta")),
    "with-meta": MalFunctionCompiled(lambda args: not_implemented("with-meta")),
    "fn?": MalFunctionCompiled(lambda args: not_implemented("fn?")),
    "string?": MalFunctionCompiled(lambda args: not_implemented("string?")),
    "number?": MalFunctionCompiled(lambda args: not_implemented("number?")),
    "seq": MalFunctionCompiled(lambda args: not_implemented("seq")),
    "conj": MalFunctionCompiled(lambda args: not_implemented("conj")),
    "get": MalFunctionCompiled(lambda args: get(args[0], args[1])),
    "first": MalFunctionCompiled(lambda args: first(args)),
    "rest": MalFunctionCompiled(lambda args: rest(args)),
    "keyword?": MalFunctionCompiled(lambda args: keyword_q(args[0])),
    "keyword": MalFunctionCompiled(lambda args: keyword(args[0])),
    "vector?": MalFunctionCompiled(lambda args: vector_q(args[0])),
    "map?": MalFunctionCompiled(lambda args: map_q(args[0])),
    "sequential?": MalFunctionCompiled(lambda args: sequential_q(args[0])),
    "vector": MalFunctionCompiled(lambda args: vector(args)),
    "hash-map": MalFunctionCompiled(lambda args: hash_map(args)),
    "assoc": MalFunctionCompiled(lambda args: assoc(args)),
    "contains?": MalFunctionCompiled(lambda args: contains_q(args)),
    "keys": MalFunctionCompiled(lambda args: keys(args)),
    "vals": MalFunctionCompiled(lambda args: vals(args)),
    "dissoc": MalFunctionCompiled(lambda args: dissoc(args)),
    "swap!": MalFunctionCompiled(lambda args: swap(args)),
}
