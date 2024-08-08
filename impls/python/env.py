# Environment
from mal_types import List

class Env():
    def __init__(self, outer=None, binds=None, exprs=None):
        """If binds is not None, exprs must be an iterable.."""
        self.data = {}
        self.outer = outer

        if binds:
            exprs_it = iter(exprs)
            for i in range(len(binds)):
                if binds[i] == "&":
                    # binds may be a non-list iterable
                    self.data[binds[i+1]] = List(exprs_it)
                    break
                else:
                    self.data[binds[i]] = next(exprs_it)

    def set(self, key, value):
        self.data[key] = value
        return value

    def get(self, key, return_nil=False):
        # Python prefers iteration over recursion.
        env = self
        while key not in env.data:
            env = env.outer
            if env is None:
                if return_nil:
                    return None
                raise Exception("'" + key + "' not found")
        return env.data[key]
