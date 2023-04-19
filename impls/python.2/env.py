from typing import Optional, Dict, List

from mal_types import MalExpression, MalSymbol, MalList, MalUnknownSymbolException


class Env(object):
    """MAL Environment"""

    def __init__(
        self,
        outer: Optional["Env"],
        binds: Optional[List[MalExpression]] = None,
        exprs: Optional[List[MalExpression]] = None,
    ) -> None:
        self._outer = outer
        self._data: Dict[str, MalExpression] = {}
        if binds is not None and exprs is not None:
            for x in range(0, len(binds)):
                assert isinstance(binds[x], MalSymbol)
                if binds[x].native() == "&":
                    self.set(str(binds[x + 1]), MalList(exprs[x:]))
                    break
                else:
                    self.set(str(binds[x]), exprs[x])

    def set(self, key: str, value: MalExpression) -> MalExpression:
        self._data[key] = value
        return value

    def get(self, key: str) -> Optional["Env"]:
        if key in self._data:
            return self._data[key]
        if self._outer is not None:
            return self._outer.get(key)
        return None

    def __repr__(self) -> str:
        env_str = "{"
        for d in self._data:
            env_str += str(d) + ": " + str(self._data[d]) + ", "
        env_str += "}"
        return f"environment: (data: {env_str} outer: {repr(self._outer) if self._outer is not None else 'None'})"
