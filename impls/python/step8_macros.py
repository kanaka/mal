import functools
import sys, traceback
import mal_readline
import mal_types as types
import reader, printer
from env import Env
import core

# read
READ = reader.read_str

# eval
def qq_loop(acc, elt):
    if types._list_Q(elt) \
       and len(elt) == 2 \
       and types._symbol_Q(elt[0]) \
       and elt[0] == 'splice-unquote':
        return types._list(types._symbol('concat'), elt[1], acc)
    else:
        return types._list(types._symbol('cons'), quasiquote(elt), acc)

def qq_foldr(seq):
    return functools.reduce(qq_loop, reversed(seq), types._list())

def quasiquote(ast):
    if types._list_Q(ast):
        if len(ast) == 2 \
           and types._symbol_Q(ast[0]) \
           and ast[0] == 'unquote':
            return ast[1]
        else:
            return qq_foldr(ast)
    elif types._hash_map_Q(ast) or types._symbol_Q(ast):
        return types._list(types._symbol('quote'), ast)
    elif types._vector_Q(ast):
        return types._list(types._symbol('vec'), qq_foldr(ast))
    else:
        return ast

def macroexpand(ast, env):
    while types._list_Q(ast) and ast:
        it = iter(ast)
        a0 = next(it)
        if not types._symbol_Q(a0):
            break
        try:
            mac = env.get(a0)
        except:
            break
        if not types._macro_Q(mac):
            break
        ast = mac(*it)
    return ast

def EVAL(ast, env):
  while True:
    # print("EVAL " + printer._pr_str(ast))

    ast = macroexpand(ast, env)

    if types._symbol_Q(ast):
        return env.get(ast)
    elif types._vector_Q(ast):
        return types.Vector(EVAL(a, env) for a in ast)
    elif types._hash_map_Q(ast):
        return types.Hash_Map((k, EVAL(v, env)) for k, v in ast.items())
    elif not types._list_Q(ast) or len(ast) == 0:
        return ast  # primitive value, return unchanged

    # From now on, ast is a non-empty list
    a0 = ast[0]

    # Search special forms
    if types._symbol_Q(a0):
        if "def!" == a0:
            a1, a2 = ast[1], ast[2]
            res = EVAL(a2, env)
            return env.set(a1, res)
        elif "let*" == a0:
            a1, a2 = ast[1], ast[2]
            let_env = Env(env)
            for k, v in types.asPairs(a1):
                let_env.set(k, EVAL(v, let_env))
            ast = a2
            env = let_env
            continue # TCO
        elif "quote" == a0:
            return ast[1]
        elif "quasiquoteexpand" == a0:
            return quasiquote(ast[1])
        elif "quasiquote" == a0:
            ast = quasiquote(ast[1])
            continue # TCO
        elif 'defmacro!' == a0:
            func = types.Macro(EVAL(ast[2], env))
            return env.set(ast[1], func)
        elif 'macroexpand' == a0:
            return macroexpand(ast[1], env)
        elif "do" == a0:
            for i in range(1, len(ast) - 1):
                EVAL(ast[i], env)
            ast = ast[-1]
            continue # TCO
        elif "if" == a0:
            a1, a2 = ast[1], ast[2]
            cond = EVAL(a1, env)
            if cond is None or cond is False:
                if len(ast) > 3:
                    ast = ast[3]
                    continue # TCO
                else:
                    return None
            else:
                ast = a2
                continue # TCO
        elif "fn*" == a0:
            a1, a2 = ast[1], ast[2]
            def f(*args):
                return EVAL(a2, Env(env, a1, args))
            def gen_env(*args):
                return Env(env, a1, args)
            return types.Fn(f, a2, gen_env)

    # a0 is not a special form
    el = (EVAL(e, env) for e in ast)
    f = next(el)
    if isinstance(f, types.Fn):
        ast = f.ast
        env = f.gen_env(*el)
        continue # TCO
    return f(*el)

# print
PRINT = printer._pr_str

# repl
repl_env = Env()
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# core.py: defined using python
for k, v in core.ns.items(): repl_env.set(types._symbol(k), v)
repl_env.set(types._symbol('eval'), lambda ast: EVAL(ast, repl_env))
repl_env.set(types._symbol('*ARGV*'), types.List(sys.argv[2:]))

# core.mal: defined using the language itself
REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

if len(sys.argv) >= 2:
    REP('(load-file "' + sys.argv[1] + '")')
    sys.exit(0)

# repl loop
while True:
    try:
        line = mal_readline.readline("user> ")
        if line == None: break
        if line == "": continue
        print(REP(line))
    except reader.Blank: continue
    except Exception as e:
        traceback.print_exception(*sys.exc_info())
