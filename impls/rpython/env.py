from mal_types import MalType, MalSym, MalList, throw_str

# Environment
class Env():
    def __init__(self, outer=None, binds=None, exprs=None):
        self.data = {}
        self.outer = outer or None

        if binds:
            assert isinstance(binds, MalList) and isinstance(exprs, MalList)
            for i in range(len(binds)):
                bind = binds[i]
                if not isinstance(bind, MalSym):
                    throw_str("env bind value is not a symbol")
                if bind.value == u"&":
                    bind = binds[i+1]
                    if not isinstance(bind, MalSym):
                        throw_str("env bind value is not a symbol")
                    self.data[bind.value] = exprs.slice(i)
                    break
                else:
                    self.data[bind.value] = exprs[i]

    def find(self, key):
        assert isinstance(key, MalSym)
        if key.value in self.data: return self
        elif self.outer:           return self.outer.find(key)
        else:                      return None

    def set(self, key, value):
        assert isinstance(key, MalSym)
        assert isinstance(value, MalType)
        self.data[key.value] = value
        return value

    def get(self, key):
        assert isinstance(key, MalSym)
        env = self.find(key)
        if not env: throw_str("'" + str(key.value) + "' not found")
        return env.data[key.value]
