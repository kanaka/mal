import functools
import sys, traceback, code
from loguru import logger
import mal_readline
import mal_types as types
import reader, printer
from env import Env
import core
logger.remove()

# debug
sys.setrecursionlimit(100000)
_line_history, _ast_history = [], []
sys.ps1, sys.ps2, _wake_up_command = "[PYTHON]> ", "        > ", ";()"
_lisp_prompt = "user> "
# _lisp_prompt = " (LISP) > "

# read
def READ(str):
    return reader.read_str(str)

def compile_symbol (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    assert(types._symbol_Q(ast))
    compiled_strings = \
[f"""
# compile_symbol
def _{prefix} ():
    # consts = []
    def {prefix} (env):
        result = env.get("{ast}")
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""]
    return compiled_strings

# NOTE This code is intended for a one-time execution. Upon
# execution, the function _{prefix} will be defined.
# Subsequently, the function _{prefix} should be invoked exactly
# once before the compilation time ends. This will return a
# closure that, when called, represents the execution of this
# Lisp Abstract Syntax Tree (AST). The timing is important (that
# is, it has to be executed before compilation time ends) because
# we need to collect objects into the list consts before the
# "gateway" (in this case, `_{prefix}_0`, which is a global
# variable and may be rebind in the next round of compilation)
# gets corrupted.
#
# NOTE The consts is a list containing the closure
# _{prefix}_0(env) that represents the function object defined.
# The design of this compiler makes sure that _{prefix}_0 will
# have been defined before the following code being executed.
def compile_def (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    assert(types._symbol_Q(ast[1]))
    compiled_strings = COMPILE(ast[2], env, prefix=f"{prefix}_0")
    compiled_strings += \
[f"""
# compile_def
def _{prefix} ():
    consts = [_{prefix}_0()]
    def {prefix} (env):
        result = env.set("{ast[1]}", consts[0](env))
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""]
    return compiled_strings

def compile_let (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    compiled_strings = []
    for i in range(1, len(ast[1]), 2):
        compiled_strings += COMPILE(ast[1][i], env, prefix=f"{prefix}_{(i-1)//2}")
    compiled_strings += COMPILE(ast[2], env, prefix=f"{prefix}_{(i+1)//2}")
    # NOTE Same in compile_symbol, we do not push the parameters
    # into the consts list, as they are just symbols and can
    # easily be serialized.
    compiled_string = \
f"""
# compile_let
def _{prefix} ():
    consts = [
"""
    for i in range(0, len(ast[1]), 2):
        assert(types._symbol_Q(ast[1][i]))
        compiled_string += \
f"""
      _{prefix}_{i//2}(),"""
    compiled_string += \
f"""
      _{prefix}_{(i+2)//2}()"""
    compiled_string += \
f"""
    ]
    def {prefix} (env):
        let_env = Env(env)"""
    for i in range(0, len(ast[1]), 2):
        compiled_string += \
f"""
        let_env.set("{ast[1][i]}", consts[{i//2}](let_env))"""
    compiled_string += \
f"""
        result = consts[{(i+2)//2}](let_env)
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_do (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
# NOTE This code is intended for a one-time execution. Upon
# execution, the function _{prefix} will be defined.
# Subsequently, the function _{prefix} should be invoked at most
# once. This will return a closure that, when called, represents
# the execution of this Lisp Abstract Syntax Tree (AST).
    compiled_strings = []
    for i in range(1, len(ast)):
        compiled_strings += COMPILE(ast[i], env, prefix=f"{prefix}_{i-1}")
    compiled_string = \
f"""
# compile_do
def _{prefix} ():
    consts = ["""
    for i in range(1, len(ast)):
        compiled_string += \
f"""
      _{prefix}_{i-1}(),"""
    compiled_string += \
f"""
    ]
    def {prefix} (env):"""
    i = 0
    for i in range(1, len(ast)-1):
        compiled_string += \
f"""
        consts[{i-1}](env)"""
    compiled_string += \
f"""
        result = consts[{i}](env)
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_if (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    cond_compiled_strings = COMPILE(ast[1], env, prefix=f"{prefix}_0")
    if_compiled_strings   = COMPILE(ast[2], env, prefix=f"{prefix}_1")
    else_compiled_strings = COMPILE(ast[3], env, prefix=f"{prefix}_2")
    compiled_string = \
f"""
# compile_if
def _{prefix} ():
    consts = [_{prefix}_0(), _{prefix}_1(), _{prefix}_2()]
    def {prefix} (env):
        cond = consts[0](env)
        if not (cond is None or cond is False):
            result = consts[1](env)
        else:
            result = consts[2](env)
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = cond_compiled_strings + if_compiled_strings + else_compiled_strings + [compiled_string]
    return compiled_strings

def compile_funcall (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    compiled_string = \
f"""
# compile_funcall
def _{prefix} ():
    consts = ["""
    for i in range(0, len(ast)):
        compiled_string += \
f"""
      _{prefix}_{i}(),"""
    compiled_string += \
f"""
    ]
    def {prefix} (env):
        result = consts[0](env) (
"""
    for i in range(1, len(ast)):
        compiled_string += f"          consts[{i}](env),\n"
    compiled_string += "        )"
    compiled_string += \
f"""
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = [compiled_string]
    for i in range(0, len(ast)):
        compiled_strings = COMPILE(ast[i], env, prefix=f"{prefix}_{i}") + compiled_strings
    return compiled_strings

def compile_literal (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    # Using the global varaible _consts feels buggy, but I think
    # and hope it isn't. There must be a way to pass this ast
    # into the closure that will be generated by the function
    # call _{prefix}(). This closure generation should happen
    # exactly once before the this round of compilation ends.
    _consts[prefix] = ast
    compiled_strings = \
[f"""
# compile_literal
def _{prefix} ():
    consts = [_consts["{prefix}"]]
    def {prefix} (env):
        result = consts[0]
        logger.debug(f"result: {{result}}")
        return result
    # Once _{prefix} is called, the ast is embedded into the closure {prefix}, so it's time to quickly remove the link to that ast object.
    popped = _consts.pop("{prefix}")
    logger.debug(f"popped : {{popped}}")
    logger.debug(f"_consts: {{_consts}}")
    return {prefix}
"""]
    return compiled_strings

def compile_fn (ast, env, prefix):
    logger.debug(f"Compiling AST:\n{ast}\n")
    params, body = ast[1], ast[2]
    compiled_string = \
f"""
# compile_fn
def _{prefix} ():
    consts = [_{prefix}_0()]
    def {prefix} (env):
        def {prefix}_fn (*args):
            params, body = {params}, "{str(body)}"
            logger.debug(f"params: {{params}}")
            logger.debug(f" body : {{body}}")
            logger.debug(f" args : {{args}}")
            local_env = Env(env, params, types.List(args))
            result = consts[0](local_env)
            logger.debug(f"result: {{result}}")
            return result
        result = {prefix}_fn
        logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = COMPILE(body, env, prefix=f"{prefix}_0") + [compiled_string]
    return compiled_strings

def qq_loop(acc, elt):
    if types._list_Q(elt) and len(elt) == 2 and elt[0] == u'splice-unquote':
        return types._list(types._symbol(u'concat'), elt[1], acc)
    else:
        return types._list(types._symbol(u'cons'), quasiquote(elt), acc)

def qq_foldr(seq):
    return functools.reduce(qq_loop, reversed(seq), types._list())

def quasiquote(ast):
    if types._list_Q(ast):
        if len(ast) == 2 and ast[0] == u'unquote':
            return ast[1]
        else:
            return qq_foldr(ast)
    elif types._hash_map_Q(ast) or types._symbol_Q(ast):
        return types._list(types._symbol(u'quote'), ast)
    elif types._vector_Q (ast):
        return types._list(types._symbol(u'vec'), qq_foldr(ast))
    else:
        return ast

def is_macro_call (ast, env):
    return (types._list_Q(ast) and
            types._symbol_Q(ast[0]) and
            env.find(ast[0]) and
            hasattr(env.get(ast[0]), '_ismacro_'))

def macroexpand (ast, env):
    count, unexpanded_ast = 0, ast
    while is_macro_call(ast, env):
        count += 1
        logger.debug(f"Macro Expansion\n> AST:\n{ast}\n")
        macro_fn = env.get(ast[0])
        ast = macro_fn(*ast[1:])
        logger.debug(f"Macro Expansion Finished ({count} fold(s)).\n> New AST:\n{ast}\n> Old AST:\n{unexpanded_ast}\n")
    return ast

_consts = {} # TODO Can I make this non-global? It feels a bit leaky. I may have to make COMPILE a closure for this.

def COMPILE (ast, env, prefix="blk"):
    logger.debug(f"Compiling AST:\n{ast}\n")
    ast = macroexpand(ast, env)
    if types._symbol_Q(ast):
        compiled_strings = compile_symbol(ast, env, prefix)
    elif types._list_Q(ast):
        if len(ast) == 0:        compiled_strings = compile_literal(ast, env, prefix)
        elif ast[0] == "quote":  compiled_strings = compile_literal(ast[1], env, prefix)
        elif ast[0] == "quasiquote":  compiled_strings = COMPILE(quasiquote(ast[1]), env, prefix) # TODO Maybe do it with defmacro!
        elif ast[0] == "quasiquoteexpand":  compiled_strings = compile_literal(quasiquote(ast[1]), env, prefix) # TODO Maybe do it with defmacro!
        elif ast[0] == "if":     compiled_strings = compile_if(ast, env, prefix)
        elif ast[0] == "def!":   compiled_strings = compile_def(ast, env, prefix)
        elif ast[0] == "let*":   compiled_strings = compile_let(ast, env, prefix)
        elif ast[0] == "do":     compiled_strings = compile_do(ast, env, prefix)
        elif ast[0] == "fn*":    compiled_strings = compile_fn(ast, env, prefix)
        else:                    compiled_strings = compile_funcall(ast, env, prefix)
    elif types._vector_Q(ast):   compiled_strings = COMPILE(types.List([types.Symbol("vector")]+list(ast)), env, prefix)
    elif types._hash_map_Q(ast): compiled_strings = COMPILE(types.Hash_Map([types.Symbol("hashmap")]+list(ast.items())), env, prefix)
    elif types._scalar_Q(ast)    or \
         types._keyword_Q(ast)   or \
         types._function_Q(ast): compiled_strings = compile_literal(ast, env, prefix)
    else:
        raise Exception(f"Unknown AST Type: {type(ast)}")
    return compiled_strings

def EXEC (compiled_strings, env):
    compiled_strings += ["\nTOP_LEVEL_RETURNED_OBJECT = _blk()(env)\n"] # This is executed later in the for loop, using the ast and env in the input.
    logger.debug(f"[BEGIN] Code to Execute\n")
    for code in compiled_strings:
        logger.debug(f"[.....] Code to Execute\n{code}")
    logger.debug(f"[ END ] Code to Execute\n")
    bindings = globals().copy()
    bindings.update(locals())
    for code in compiled_strings:
        exec(code, bindings, bindings)
    return bindings["TOP_LEVEL_RETURNED_OBJECT"]

# eval
def EVAL(ast, env):
    logger.debug(f"EVAL AST: {ast}\n")
    _ast_history.append(ast)
    return EXEC(COMPILE(ast, env), env)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# environment
repl_env = Env()

# repl
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# lisp repl loop
def REPL():
    global _line_history
    logger.info(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline(_lisp_prompt)
            if line == None: break
            if line == "": continue
            if line == _wake_up_command:
                logger.info(f"Hint: Use `LISP()` to get into LISP.")
                break # debug
            _line_history.append(line) # debug
            print(REP(line))
        except reader.Blank: continue
        except Exception as e:
            print("".join(traceback.format_exception(*sys.exc_info())))

# debug
def DEBUG (switch="on"):
    if switch:
        logger.add(sys.stderr, level="DEBUG")
        logger.info("Debugger Activated (loguru)!")
    else:
        logger.remove()

def TEST ():
    logger.info("Running tests..")
    assert(EVAL(READ("nil"), repl_env) == None)
    assert(EVAL(READ("true"), repl_env) == True)
    assert(EVAL(READ("false"), repl_env) == False)
    assert(EVAL(READ("123"), repl_env) == 123)
    assert(EVAL(READ("\"This is a string.\""), repl_env) == "This is a string.")
    assert(EVAL(READ("()"), repl_env) == [])
    assert(EVAL(READ("(+ 1 1)"), repl_env) == 2)
    assert(EVAL(READ("(+ (* 2 (+ 3 4)) 1))"), repl_env) == 15)
    assert(EVAL(READ("(+)"), repl_env) == 0)
    assert(EVAL(READ("(-)"), repl_env) == 0)
    assert(EVAL(READ("(- 9)"), repl_env) == -9)
    assert(EVAL(READ("(- 9 2)"), repl_env) == 7)
    assert(EVAL(READ("(*)"), repl_env) == 1)
    assert(EVAL(READ("(let* (a 3 b 4) (+ a b))"), repl_env) == 7)
    assert(EVAL(READ("(let* (a 3 b 4) (+ a (let* (b 0) b)))"), repl_env) == 3)
    assert(EVAL(READ("(let* (a 3 b a) b)"), repl_env) == 3)
    assert(EVAL(READ("(let* (a 3 b a) (let* (a b a a) 3))"), repl_env) == 3)
    assert(EVAL(READ("(do (def! x 1) (def! y 2) (+ x y))"), repl_env) == 3)
    assert(EVAL(READ("(do (def! x 8) x (def! y 9) (let* (y x x y) x))"), repl_env) == 8)
    assert(EVAL(READ("(if          )"), repl_env) == None)
    assert(EVAL(READ("(if 0        )"), repl_env) == None)
    assert(EVAL(READ("(if 0     1  )"), repl_env) == 1)
    assert(EVAL(READ("(if 0     1 2)"), repl_env) == 1)
    assert(EVAL(READ("(if nil   1 2)"), repl_env) == 2)
    assert(EVAL(READ("(if nil   1  )"), repl_env) == None)
    assert(EVAL(READ("(if false 1 2)"), repl_env) == 2)
    assert(EVAL(READ("(if (if false 0 nil) 1 2)"), repl_env) == 2)
    assert(EVAL(READ("(do (def! f (fn* () a)) (def! a 9) (let* (a 0) (f)))"), repl_env) == 9) # NOTE Is this desirable?
    assert(EVAL(READ("((fn* (a) a) 7)"), repl_env) == 7)
    assert(EVAL(READ("((fn* (a) (* (a) (a))) (fn* (a) 3))"), repl_env) == 9)
    assert(EVAL(READ("((fn* (a b) (* (a b) b)) (fn* (a) (+ 2 a)) 7)"), repl_env) == 63)
    assert(EVAL(READ("((let* (a 10000 b -2) (fn* (a c) (+ a b c))) 1 1)"), repl_env) == 0)
    def expect_infinite_loop (lisp_code):
        try:
            EVAL(READ(lisp_code), repl_env)
        except RecursionError:
            pass
        else:
            raise Exception("Should have given an infinite loop.")
    expect_infinite_loop("((fn* (a) (a a)) (fn* (a) (a a)))")
    expect_infinite_loop("(let* (f (fn* (a) (a a))) (f f))")
    expect_infinite_loop("(do (def! f (fn* (a) (a a))) (def! g f) (g g))")
    expect_infinite_loop("(let* (f (fn* (a) (a a)) g f) (g g))")
    assert(types._function_Q(EVAL(READ("(eval +)"), repl_env)))
    assert(EVAL(READ("(eval (list + 1))"), repl_env) == 1)
    assert(EVAL(READ("(let* (x (atom 3)) (atom? x))"), repl_env) == True)
    assert(EVAL(READ("(let* (x (atom 3)) (deref x))"), repl_env) == 3)
    assert(EVAL(READ("(let* (x (atom 3)) @x)"), repl_env) == 3)
    assert(EVAL(READ("(let* (x (atom 3)) (do (swap! x (fn* (n) (+ 1 n))) (deref x)))"), repl_env) == 4)
    assert(EVAL(READ("(let* (x (atom 3)) (do (reset! x 9) (deref x)))"), repl_env) == 9)
    assert(EVAL(READ("(quote (1 2 3))"), repl_env) == [1, 2, 3])
    assert(EVAL(READ("'(1 2 3)"), repl_env) == [1, 2, 3])
    assert(EVAL(READ("'+"), repl_env) == "+")
    assert(EVAL(READ("\"+\""), repl_env) == "+")
    assert(EVAL(READ("(= '+ \"+\")"), repl_env) == False)
    assert(EVAL(READ("(cond true 1 false 2 true 3)"), repl_env) == 1)
    assert(EVAL(READ("(cond false 1 false 2 true 3)"), repl_env) == 3)
    assert(EVAL(READ("(cond false 1 false 2 false 3)"), repl_env) == None)
    assert(EVAL(READ("(do (def! x 1) (def! f (fn* () (do (def! x 2) (let* (x 3) (g))))) (def! g (fn* () x)) (g))"), repl_env) == 1)
    assert(EVAL(READ("(do (def! x 1) (def! f (fn* () (do (def! x 2) (def! g (fn* () x)) (let* (x 3) (g))))) (f))"), repl_env) == 2)
    logger.info("All tests passed!")
    print("All tests passed!")

    # load from core
for k, v in core.ns.items(): repl_env.set(types._symbol(k), v)
repl_env.set(types._symbol('eval'), lambda ast: EVAL(ast, repl_env))
repl_env.set(types._symbol('vector'), lambda *vector_elements: types.Vector(vector_elements))
repl_env.set(types._symbol('hashmap'), lambda *dict_pairs: types.Hash_map()) # TODO FIXME
repl_env.set(types._symbol('*ARGV*'), types._list(*sys.argv[2:]))
repl_env.set(types._symbol('debug'), DEBUG)
repl_env.set(types._symbol('test'), TEST)
repl_env.set(types._symbol('set-ismacro'), lambda fn: setattr(fn, '_ismacro_', True))
repl_env.set(types._symbol('unset-ismacro'), lambda fn: setattr(fn, '_ismacro_', False))
repl_env.set(types._symbol('ismacro'), lambda fn: getattr(fn, '_ismacro_', False))

REP("(def! not (fn* (a) (if a false true)))")
REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
REP("(def! defmacro! (fn* (name function-body-ast) (list 'do (list 'def! name function-body-ast) (list 'set-ismacro name))))") # TODO Rewrite after having quasiquote.
REP("(set-ismacro defmacro!)")
REP("(defmacro! iden (fn* (ast) ast))")
REP("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")


def main ():
    LISP = REPL
    LISP()

main()
code.interact(local=locals()) # python repl loop
