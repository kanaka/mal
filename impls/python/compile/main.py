import sys, traceback, code
import mal_readline
import mal_types as types
import reader, printer
from env import Env

# debug
from loguru import logger
logger.info("Debugger Activated: loguru")
_line_history, _ast_history = [], []
sys.ps1, sys.ps2 = "PY> ", "  > "
_wake_up_command = ";()"

# read
def READ(str):
    return reader.read_str(str)

# # compile and execute
# def EXEC_COMPILE (ast, env):
#     def COMPILE (ast, env):
#         logger.debug(f"ast: {ast}")

#         if types._symbol_Q(ast):
#             compiled_string="""
# _RETURNED_OBJECT = exec_env.get(exec_ast)
#             """
#         elif types._list_Q(ast):
#             # Primary Operator: 'def!
#             if ast[0] == "def!":
#                 # FIXME I think the string cannot have EXEC_COMPILE; otherwise it's cheating.
#                 compiled_string="""
# _A1, _A2 = exec_ast[1], exec_ast[2]
# _RETURNED_OBJECT = EXEC_COMPILE(_A2, exec_env)
# _RETURNED_OBJECT = exec_env.set(_A1, _RETURNED_OBJECT)
#                 """
#             # Primary Operator: 'let*
#             elif ast[0] == "let*":
#                 compiled_string="""
# _A1, _A2 = exec_ast[1], exec_ast[2]
# _let_env = Env(exec_env)
# for i in range(0, len(_A1), 2):
#     _let_env.set(_A1[i], EXEC_COMPILE(_A1[i+1], exec_env))
# _RETURNED_OBJECT = EXEC_COMPILE(_A2, _let_env)
#                 """
#             # Primary Operator: 'do
#             elif ast[0] == "do":
#                 compiled_string="""
# for _exec_sub_ast in exec_ast[1:-1]:
#     EXEC_COMPILE(_exec_sub_ast, exec_env)
# _RETURNED_OBJECT = EXEC_COMPILE(exec_ast[-1], exec_env)
#                 """
#             # Primary Operator: 'if
#             elif ast[0] == "if":
#                 compiled_string="""
# _A1 = exec_ast[1]
# _COND = EXEC_COMPILE(_A1, exec_env)
# if _COND is None or _COND is False:
#     if len(exec_ast) > 3: _RETURNED_OBJECT = EXEC_COMPILE(exec_ast[3], exec_env)
#     else:                 _RETURNED_OBJECT = None
# else:
#     _RETURNED_OBJECT = EXEC_COMPILE(exec_ast[2], exec_env)
#                 """
#             # Primary Operator: 'fn*
#             elif ast[0] == "fn*":
#                 # FIXME I think the string cannot have _function. Otherwise the function is compiled at runtime, defeating the purpose of a compiler.
#                 compiled_string="""
# _A1, _A2 = exec_ast[1], exec_ast[2]
# _RETURNED_OBJECT = types._function(EXEC_COMPILE, Env, _A2, exec_env, _A1)
#                 """
#             # Non-Special Forms
#             else:
#                 compiled_string="""
# _ARGUMENTS = []
# for _exec_sub_ast in exec_ast[1:]:
#     _ARGUMENTS.append(EVAL(_exec_sub_ast, exec_env))
# _OPERATOR = EVAL(exec_ast[0], exec_env)
# _RETURNED_OBJECT = _OPERATOR(*_ARGUMENTS)
#                 """
#         elif types._vector_Q(ast): # TODO
#             logger.debug(f"Unsupported Type: {type(ast)}")
#             compiled_string="_RETURNED_OBJECT = exec_ast"
#         elif types._hash_map_Q(ast): # TODO
#             logger.debug(f"Unsupported Type: {type(ast)}")
#             compiled_string="_RETURNED_OBJECT = exec_ast"
#         else:
#             compiled_string="_RETURNED_OBJECT = exec_ast"
#         logger.debug(f"compiled_string: {compiled_string}")
#         return compiled_string

#     def EXEC (compiled_string):
#         exec_ast, exec_env = ast, env
#         global_bindings, local_bindings = globals().copy(), locals().copy()
#         exec(compiled_string, global_bindings, local_bindings)
#         return local_bindings.get('_RETURNED_OBJECT')

#     return EXEC(COMPILE(ast, env))

def COMPILE (ast, env, prefix="blk"):
    logger.debug(f"ast: {ast}")

    if types._symbol_Q(ast):
        compiled_strings = \
[f"""
def {prefix} (ast, env):
    return env.get(ast)
"""]

    elif types._list_Q(ast):
        # Primary Operator: 'def!
        if ast[0] == "def!":
            compiled_strings = \
[f"""
def {prefix} (ast, env):
    return env.set(ast[1], {prefix}_{2}(ast, env))
"""]
            compiled_strings = COMPILE(ast[2], env, prefix=f"{prefix}_{2}") + compiled_strings

        # Non-Special Forms
        else:
            compiled_string = \
f"""
def {prefix} (ast, env):
    return {prefix}_{0}(ast[0], env) \\
"""
            compiled_string += "   (\n"
            for counter in range(1,len(ast)):
                compiled_string += f"        {prefix}_{counter}(ast[{counter}], env),\n"
            compiled_string += "   )\n"
            compiled_strings = [compiled_string]
            for counter in range(0,len(ast)):
                add_strings = COMPILE(ast[counter],env,prefix=f"{prefix}_{counter}")
                compiled_strings = add_strings + compiled_strings

    else:
        compiled_strings = \
[f"""
def {prefix} (ast, env):
    return {ast}
"""]

    return compiled_strings

def EXEC (compiled_strings, ast, env):
    compiled_strings += ["\nRET = blk(ast, env)\n"]
    for s in compiled_strings:
        logger.debug(s)
    bindings = globals()
    bindings.update(locals())
    for code in compiled_strings:
        exec(code, bindings, bindings)
    return bindings["RET"]

# eval
def EVAL(ast, env):
    _ast_history.append(ast)
    return EXEC(COMPILE(ast, env), ast, env)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# environment
repl_env = Env()

# arithmetic
def multiply (args):
    if args == []:
        return 1
    if len(args) == 1:
        return args[0]
    else:
        return args[0] * multiply(args[1:])
repl_env.set(types._symbol('+'), lambda *args: sum(args))
repl_env.set(types._symbol('-'), lambda *args: args[0] - sum(args[1:]))
repl_env.set(types._symbol('*'), lambda *args: multiply(args))
repl_env.set(types._symbol('/'), lambda a,b: int(a/b))

# repl
def REP(str):
    return PRINT(EVAL(READ(str), repl_env))

# automatic tests
logger.info("Running tests..")
assert(EVAL(READ("(+ 1 1))"), repl_env) == 2)
assert(EVAL(READ("(+ (* 2 (+ 3 4)) 1))"), repl_env) == 15)
# assert(EVAL(READ("(let* (a 3 b 4) (+ a b))"), repl_env) == 7)
# assert(EVAL(READ("(let* (a 3 b 4) (+ a (let* (b 0) b)))"), repl_env) == 3)
# assert(EVAL(READ("(do (def! x 1) (def! y 2) (+ x y))"), repl_env) == 3)
# assert(EVAL(READ("(if 0     1  )"), repl_env) == 1)
# assert(EVAL(READ("(if 0     1 2)"), repl_env) == 1)
# assert(EVAL(READ("(if nil   1 2)"), repl_env) == 2)
# assert(EVAL(READ("(if nil   1  )"), repl_env) == None)
# assert(EVAL(READ("(if false 1 2)"), repl_env) == 2)
# assert(EVAL(READ("(if (if false 0 nil) 1 2)"), repl_env) == 2)
# assert(EVAL(READ("((fn* (a) a) 7)"), repl_env) == 7)
# assert(EVAL(READ("((fn* (a) (* (a) (a))) (fn* (a) 3))"), repl_env) == 9)
logger.info("All tests passed!")

# lisp repl loop
def REPL():
    global _line_history
    logger.info(f"Hint: Use `{_wake_up_command}` to get into PYTHON.")
    while True:
        try:
            line = mal_readline.readline("LISP> ")
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
