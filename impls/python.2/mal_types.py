from typing import Callable, Dict, List, Any


class MalExpression(object):
    def __init__(self):
        assert False  # cannot instantiate

    def native(self) -> Any:
        """Return a shallow native Python equivalent for the expression.

        For example, (1 2 3) might become [MalInt(1), MalInt(2), MalInt(3)]"""
        pass

    def __str__(self) -> str:
        return self.readable_str()

    def readable_str(self) -> str:
        """Return a human-readable (preferably Mal input format) form of the expression."""
        pass

    def unreadable_str(self) -> str:
        """Returns an unescaped/raw str. Defaults to being the same as readable_str."""
        return self.readable_str()


class MalString(MalExpression):
    def __init__(
        self, input_value: str, is_already_encoded: bool = False, keyword: bool = False
    ) -> None:
        # print("STR: " + input_value)
        if is_already_encoded:
            self._value = input_value
        if keyword:
            self._value = "\u029e" + input_value
        else:
            self._value = input_value

    def readable_str(self) -> str:
        if self.is_keyword():
            return ":" + self._value[1:]
        else:
            val = self._value

            val = val.replace("\\", "\\\\")  # escape backslashes
            val = val.replace("\n", "\\n")  # escape newlines
            val = val.replace('"', '\\"')  # escape quotes
            val = '"' + val + '"'  # add surrounding quotes
            return val

    def unreadable_str(self) -> str:
        if self.is_keyword():
            return ":" + self._value[1:]
        else:
            return self._value

    def native(self) -> Any:
        return self._value

    def is_keyword(self) -> bool:
        return len(self._value) > 1 and self._value[0] == "\u029e"


class MalList(MalExpression):
    def __init__(self, values: List[MalExpression]) -> None:
        for x in values:
            assert isinstance(x, MalExpression)
        self._values = values

    def readable_str(self) -> str:
        return "(" + " ".join(map(lambda x: x.readable_str(), self._values)) + ")"

    def unreadable_str(self) -> str:
        return "(" + " ".join(map(lambda x: x.unreadable_str(), self._values)) + ")"

    def native(self) -> List[MalExpression]:
        return self._values


class MalSymbol(MalExpression):
    def __init__(self, value: str) -> None:
        assert type(value) is str

        self._value = str(value)

    def readable_str(self) -> str:
        return str(self._value)

    def eval(self, environment) -> MalExpression:
        # print("Evaluating: " + repr(self))
        return environment.get(self)

    def native(self) -> str:
        return self._value


class MalException(Exception, MalExpression):
    def __init__(self, value: MalExpression) -> None:
        self._value = value

    def readable_str(self) -> str:
        return str(self._value)

    def native(self) -> MalExpression:
        return self._value


class MalIndexError(MalException):
    def __init__(self, index: int) -> None:
        super().__init__(MalString("Index out of bounds: " + str(index)))


class MalSyntaxException(MalException):
    def __init__(self, message) -> None:
        super().__init__(MalString(message))


class MalUnknownTypeException(MalException):
    def __init__(self, message) -> None:
        super().__init__(MalString(message))


class MalInvalidArgumentException(MalException):
    def __init__(self, arg: MalExpression, reason: str) -> None:
        super().__init__(
            MalString(arg.readable_str() + ": invalid argument: " + reason)
        )


class MalUnknownSymbolException(MalException):
    def __init__(self, func: str) -> None:
        super().__init__(MalString("'" + func + "' not found"))
        self.func = func


class MalNotImplementedException(MalException):
    def __init__(self, func: str) -> None:
        super().__init__(MalString("not implemented: " + func))


class MalFunctionCompiled(MalExpression):
    def __init__(
        self, native_function: Callable[[List[MalExpression]], MalExpression]
    ) -> None:
        self._native_function = native_function
        self._is_macro = False

    def readable_str(self):
        return "#<macro>" if self._is_macro else "#<function>"

    def native(self) -> Callable[[List[MalExpression]], MalExpression]:
        return self._native_function

    def call(self, args: List[MalExpression]) -> MalExpression:
        # print("CALL: " + str([str(arg) for arg in args]))
        return self._native_function(args)

    def is_macro(self) -> bool:
        return self._is_macro

    def make_macro(self) -> None:
        self._is_macro = True


class MalFunctionRaw(MalExpression):
    def __init__(
        self,
        fn: Callable[[List[MalExpression]], MalExpression],
        ast: MalExpression,
        params: MalList,
        env,
    ) -> None:
        self._ast = ast
        self._params = params
        self._env = env
        self._native_function = fn
        self._is_macro = False

    def readable_str(self):
        return "#<macro>" if self._is_macro else "#<function>"

    def ast(self) -> MalExpression:
        return self._ast

    def params(self) -> MalList:
        return self._params

    def env(self):
        return self._env

    def native(self) -> Callable[[List[MalExpression]], MalExpression]:
        return self._native_function

    def call(self, args: List[MalExpression]) -> MalExpression:
        return self._native_function(args)

    def is_macro(self) -> bool:
        return self._is_macro

    def make_macro(self) -> None:
        self._is_macro = True


class MalInt(MalExpression):
    def __init__(self, value: int) -> None:
        assert type(value) is int
        self._value = value

    def readable_str(self) -> str:
        return str(self._value)

    def native(self) -> int:
        return self._value


class MalVector(MalExpression):
    def __init__(self, values: List[MalExpression]) -> None:
        self._values = values

    def readable_str(self) -> str:
        return "[" + " ".join(map(lambda x: x.readable_str(), self._values)) + "]"

    def unreadable_str(self) -> str:
        return "[" + " ".join(map(lambda x: x.unreadable_str(), self._values)) + "]"

    def native(self) -> List[MalExpression]:
        return self._values


class MalHash_map(MalExpression):
    def __init__(self, values: Dict[str, MalExpression]) -> None:
        self._dict = values.copy()

    def readable_str(self) -> str:
        result_list: List[str] = []
        for x in self._dict:
            result_list.append(MalString(x).readable_str())
            result_list.append(self._dict[x].readable_str())
        return "{" + " ".join(result_list) + "}"

    def unreadable_str(self) -> str:
        result_list: List[str] = []
        for x in self._dict:
            result_list.append(MalString(x, is_already_encoded=True).unreadable_str())
            result_list.append(self._dict[x].unreadable_str())
        return "{" + " ".join(result_list) + "}"

    def native(self) -> Dict[str, MalExpression]:
        return self._dict


class MalNil(MalExpression):
    def __init__(self) -> None:
        pass

    def readable_str(self) -> str:
        return "nil"

    def eval(self, environment) -> MalExpression:
        return self

    def native(self) -> None:
        return None


class MalBoolean(MalExpression):
    def __init__(self, value: bool) -> None:
        self._value = value

    def readable_str(self) -> str:
        if self._value:
            return "true"
        return "false"

    def native(self) -> bool:
        return self._value


class MalAtom(MalExpression):
    def __init__(self, value: MalExpression) -> None:
        self._value = value

    def native(self) -> MalExpression:
        return self._value

    def readable_str(self) -> str:
        return "(atom " + str(self._value) + ")"

    def reset(self, value: MalExpression) -> None:
        self._value = value
