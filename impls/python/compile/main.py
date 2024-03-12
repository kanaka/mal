import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer
from env import Env
import core

# debug
from loguru import logger
logger.info("Debugger Activated: loguru")
_line_history, _ast_history = [], []
sys.ps1, sys.ps2 = "[PYTHON]> ", "        > "
_wake_up_command = ";()"

# read
def READ(str):
    return reader.read_str(str)

def compile_symbol (ast, env, prefix):
    compiled_strings = \
[f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    result = env.get(ast)
    logger.debug(f"result: {{result}}")
    return result
"""]
    return compiled_strings

def compile_def (ast, env, prefix):
    compiled_strings = \
[f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    result = env.set(ast[1], {prefix}_0(ast[2], env))
    logger.debug(f"result: {{result}}")
    return result
"""]
    compiled_strings = COMPILE(ast[2], env, prefix=f"{prefix}_0") + compiled_strings
    return compiled_strings

def compile_let (ast, env, prefix):
    compiled_strings = []
    for i in range(1, len(ast[1]), 2):
        compiled_strings += COMPILE(ast[1][i], env, prefix=f"{prefix}_{(i-1)//2}")
    compiled_strings += COMPILE(ast[2], env, prefix=f"{prefix}_{(i+1)//2}")
    compiled_string = \
f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    let_env = Env(env)"""
    for i in range(0, len(ast[1]), 2):
        compiled_string += \
f"""
    let_env.set(ast[1][{i}], {prefix}_{i//2}(ast[1][{i+1}], let_env))"""
    compiled_string += \
f"""
    result = {prefix}_{(i+2)//2}(ast[2], let_env)
    logger.debug(f"result: {{result}}")
    return result
"""
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_do (ast, env, prefix):
    compiled_string = \
f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
"""
    for i in range(1, len(ast)-1):
        compiled_string += \
f"""
    {prefix}_{i-1}(ast[{i}], env)
"""
    i += 1
    compiled_string += \
f"""
    result = {prefix}_{i-1}(ast[{i}], env)
    logger.debug(f"result: {{result}}")
    return result
"""
    compiled_strings = []
    for i in range(1, len(ast)):
        compiled_strings += COMPILE(ast[i], env, prefix=f"{prefix}_{i-1}")
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_if (ast, env, prefix):
    compiled_string = \
f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    cond = {prefix}_0(ast[1], env)
    if not (cond is None or cond is False):
        result = {prefix}_1 (ast[2], env)
    else:
        result = {prefix}_2 (ast[3], env)
    logger.debug(f"result: {{result}}")
    return result
"""
    cond_compiled_strings = COMPILE(ast[1], env, prefix=f"{prefix}_0")
    if_compiled_strings = COMPILE(ast[2], env, prefix=f"{prefix}_1")
    else_compiled_strings = COMPILE(ast[3], env, prefix=f"{prefix}_2")
    compiled_strings = cond_compiled_strings + if_compiled_strings + else_compiled_strings + [compiled_string]
    return compiled_strings

def compile_regular (ast, env, prefix):
    compiled_string = \
f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    result = {prefix}_{0}(ast[0], env) \\
"""
    compiled_string += "   (\n"
    for i in range(1, len(ast)):
        compiled_string += f"        {prefix}_{i}(ast[{i}], env),\n"
    compiled_string += "   )"
    compiled_string += \
f"""
    logger.debug(f"result: {{result}}")
    return result
"""
    compiled_strings = [compiled_string]
    for i in range(0, len(ast)):
        compiled_strings = COMPILE(ast[i], env, prefix=f"{prefix}_{i}") + compiled_strings
    return compiled_strings

def compile_scalar (ast, env, prefix):
    if types._string_Q(ast): ast = f"\"{ast}\""
    compiled_strings = \
[f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    result = {ast}
    logger.debug(f"result: {{result}}")
    return result
"""]
    return compiled_strings

def compile_fn (ast, env, prefix):
    compiled_strings = \
[f"""
def {prefix} (ast, env):
    logger.debug(f"ast: {{ast}}")
    def {prefix}_lambda (*args):
        logger.debug(f"args: {{args}}")
        params = ast[1]
        logger.debug(f"ast: {{ast}}")
        logger.debug(f"params: {{params}}")
        return EVAL(ast[2], Env(env, params, types.List(args))) # TODO Is this steap cheating?
    result = {prefix}_lambda
    logger.debug(f"result: {{result}}")
    return result
"""]
    return compiled_strings

def COMPILE (ast, env, prefix="blk"):
    logger.debug(f"ast: {ast}")
    if types._symbol_Q(ast):
        return compile_symbol(ast, env, prefix)
    elif types._list_Q(ast):
        if   len(ast) == 0:    return compile_scalar(None, env, prefix)
        elif ast[0] == "def!": return compile_def(ast, env, prefix)
        elif ast[0] == "let*": return compile_let(ast, env, prefix)
        elif ast[0] == "do":   return compile_do(ast, env, prefix)
        elif ast[0] == "if":   return compile_if(ast, env, prefix)
        elif ast[0] == "fn*":  return compile_fn(ast, env, prefix)
        else:                  return compile_regular(ast, env, prefix)
    elif types._scalar_Q(ast): return compile_scalar(ast, env, prefix)
    elif types._vector_Q(ast) or types._hash_map_Q(ast):
        raise Exception("Unsupported Type") # TODO
    else:
        raise Exception(f"Unknown AST Type: {type(ast)}")

def EXEC (compiled_strings, ast, env):
    compiled_strings += ["\nRET = blk(ast, env)\n"]
    logger.debug(f"<<< Compiled Code |\n")
    for s in compiled_strings:
        logger.debug(f"<<< C....... C... |\n{s}")
    logger.debug(f"  | Compiled Code | AST : {ast} >>>\n")
    bindings = globals().copy()
    bindings.update(locals())
    for code in compiled_strings:
        exec(code, bindings, bindings)
    return bindings["RET"]

# eval
def EVAL(ast, env):
    logger.debug(f"EVAL AST: {ast}\n")
    _ast_history.append(ast)
    return EXEC(COMPILE(ast, env), ast, env)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# environment
repl_env = Env()

# load from core
for k, v in core.ns.items(): repl_env.set(types._symbol(k), v)

# repl
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# automatic tests
logger.info("Running tests..")
logger.remove()
assert(EVAL(READ("nil"), repl_env) == None)
assert(EVAL(READ("true"), repl_env) == True)
assert(EVAL(READ("false"), repl_env) == False)
assert(EVAL(READ("123"), repl_env) == 123)
assert(EVAL(READ("\"This is a string.\""), repl_env) == "This is a string.")
assert(EVAL(READ("()"), repl_env) == None)
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
logger.add(sys.stderr, level="DEBUG")
logger.info("All tests passed!")

# lisp repl loop
def REPL():
    global _line_history
    logger.info(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline(" (LISP) > ")
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

LISP = REPL
REPL()

# python repl loop
code.interact(local=locals())
