# Env is defined in mal_types.py in order to avoid a circular dependency.
from collections.abc import Sequence

from mal_types import Env, Error, Form, List, pr_seq


def call_env(env: Env, parms: Sequence[str], args: Sequence[Form]) -> Env:
    match parms:
        case [*required, '&', rest]:
            if len(args) < len(required):
                raise Error('not enough arguments for fn*['
                            + ' '.join(parms) + ']: ' + pr_seq(args))
            fn_env = env.new_child(dict(zip(required, args)))
            fn_env[rest] = List(args[len(required):])
            return fn_env
        case _:
            if len(args) != len(parms):
                raise Error('bad argument count for fn*['
                            + ' '.join(parms) + ']: ' + pr_seq(args))
            return env.new_child(dict(zip(parms, args)))
