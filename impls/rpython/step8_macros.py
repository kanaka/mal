import sys, traceback
import mal_readline
import mal_types as types
from mal_types import (MalSym, MalInt, MalStr,
                       nil, true, false, _symbol, _keywordu,
                       throw_str,
                       MalList, _list, MalVector, MalHashMap, MalFunc)
import reader, printer
from env import Env
import core

# read
def READ(str):
    return reader.read_str(str)

# eval
def qq_loop(elt, acc):
    if types._list_Q(elt) and len(elt) == 2:
        fst = elt[0]
        if isinstance(fst, MalSym) and fst.value == u"splice-unquote":
            return _list(_symbol(u"concat"), elt[1], acc)
    return _list(_symbol(u"cons"), quasiquote(elt), acc)

def qq_foldr(seq):
    acc = _list()
    for elt in reversed(seq):
        acc = qq_loop (elt, acc)
    return acc

def quasiquote(ast):
    if types._list_Q(ast):
        if len(ast) == 2:
            fst = ast[0]
            if isinstance(fst, MalSym) and fst.value == u"unquote":
                return ast[1]
        return qq_foldr(ast.values)
    elif types._vector_Q(ast):
        return _list(_symbol(u"vec"), qq_foldr(ast.values))
    elif types._symbol_Q(ast) or types._hash_map_Q(ast):
        return _list(_symbol(u"quote"), ast)
    else:
        return ast

def EVAL(ast, env):
  while True:
    if env.get(u"DEBUG-EVAL") not in (None, nil, false):
        print(u"EVAL: " + printer._pr_str(ast))
    if types._symbol_Q(ast):
        assert isinstance(ast, MalSym)
        value = env.get(ast.value)
        if value is None:
            throw_str("'" + str(ast.value) + "' not found")
        return value
    elif types._vector_Q(ast):
        res = []
        for a in ast.values:
            res.append(EVAL(a, env))
        return MalVector(res)
    elif types._hash_map_Q(ast):
        new_dct = {}
        for k in ast.dct.keys():
            new_dct[k] = EVAL(ast.dct[k], env)
        return MalHashMap(new_dct)
    elif not types._list_Q(ast):
        return ast  # primitive value, return unchanged
    else:
        # apply list
        if len(ast) == 0: return ast
        a0 = ast[0]
        if isinstance(a0, MalSym):
            a0sym = a0.value
        else:
            a0sym = u"__<*fn*>__"

        if u"def!" == a0sym:
            a1, a2 = ast[1], ast[2]
            res = EVAL(a2, env)
            return env.set(a1, res)
        elif u"let*" == a0sym:
            a1, a2 = ast[1], ast[2]
            let_env = Env(env)
            for i in range(0, len(a1), 2):
                let_env.set(a1[i], EVAL(a1[i+1], let_env))
            ast = a2
            env = let_env # Continue loop (TCO)
        elif u"quote" == a0sym:
            return ast[1]
        elif u"quasiquote" == a0sym:
            ast = quasiquote(ast[1]) # Continue loop (TCO)
        elif u"defmacro!" == a0sym:
            func = EVAL(ast[2], env)
            return env.set(ast[1],
                           MalFunc(func.fn, ast=func.ast, env=func.env,
                           params=func.params, EvalFunc=func.EvalFunc,
                           ismacro=True))
        elif u"do" == a0sym:
            if len(ast) == 0:
                return nil
            for i in range(1, len(ast) - 1):
                EVAL(ast[i], env)
            ast = ast[-1] # Continue loop (TCO)
        elif u"if" == a0sym:
            a1, a2 = ast[1], ast[2]
            cond = EVAL(a1, env)
            if cond is nil or cond is false:
                if len(ast) > 3: ast = ast[3] # Continue loop (TCO)
                else:            return nil
            else:
                ast = a2 # Continue loop (TCO)
        elif u"fn*" == a0sym:
            a1, a2 = ast[1], ast[2]
            return MalFunc(None, a2, env, a1, EVAL)
        else:
            f = EVAL(a0, env)
            if f.ismacro:
                ast = f.apply(ast.rest()) # Continue loop (TCO)
                continue
            args_list = []
            for i in range(1, len(ast)):
                args_list.append(EVAL(ast[i], env))
            args = MalList(args_list)
            if isinstance(f, MalFunc):
                if f.ast:
                    ast = f.ast
                    env = f.gen_env(args) # Continue loop (TCO)
                else:
                    return f.apply(args)
            else:
                raise Exception("%s is not callable" % f)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
class MalEval(MalFunc):
    def apply(self, args):
        return self.EvalFunc(args[0], self.env)

def entry_point(argv):
    repl_env = Env()
    def REP(str, env):
        return PRINT(EVAL(READ(str), env))

    # core.py: defined using python
    for k, v in core.ns.items():
        repl_env.set(_symbol(unicode(k)), MalFunc(v))
    repl_env.set(types._symbol(u'eval'),
                 MalEval(None, env=repl_env, EvalFunc=EVAL))
    mal_args = []
    if len(argv) >= 3:
        for a in argv[2:]: mal_args.append(MalStr(unicode(a)))
    repl_env.set(_symbol(u'*ARGV*'), MalList(mal_args))

    # core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))", repl_env)
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", repl_env)
    REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", repl_env)

    if len(argv) >= 2:
        REP('(load-file "' + argv[1] + '")', repl_env)
        return 0

    while True:
        try:
            line = mal_readline.readline("user> ")
            if line == "": continue
            print(REP(line, repl_env))
        except EOFError as e:
            break
        except reader.Blank:
            continue
        except types.MalException as e:
            print(u"Error: %s" % printer._pr_str(e.object, False))
        except Exception as e:
            print("Error: %s" % e)
            #print("".join(traceback.format_exception(*sys.exc_info())))
    return 0

# _____ Define and setup target ___
def target(*args):
    return entry_point

# Just run entry_point if not RPython compilation
import sys
if not sys.argv[0].endswith('rpython'):
    entry_point(sys.argv)
