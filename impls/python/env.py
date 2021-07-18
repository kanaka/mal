# Environment

import mal_types

class Env():
    def __init__(self, outer=None, binds=None, exprs=None):
        self.data = {}
        self.outer = outer

        if binds:
            for i in range(len(binds)):
                if binds[i] == "&":
                    self.data[binds[i+1]] = mal_types.List(exprs[i:])
                    break
                else:
                    self.data[binds[i]] = exprs[i]

    def set(self, key, value):
        self.data[key] = value
        return value

    def get(self, key):
        env = self
        while env:
            try:
                return env.data[key]
            except KeyError:
                pass
            env = env.outer
        raise Exception("'" + key + "' not found")
