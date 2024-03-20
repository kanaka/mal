import sys, traceback, code, functools
import mal_types as types
from debugger import logger

_consts = {} # TODO Can I make this non-global? It feels a bit leaky. I may have to make COMPILE a closure for this.

def compile_symbol (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    assert(types._symbol_Q(ast))
    compiled_strings = \
[f"""
# compile_symbol
def _{prefix} ():
    # consts = []
    def {prefix} (env):
        result = env.get("{ast}")
        #logger.debug(f"result: {{result}}")
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
    #logger.debug(f"Compiling AST:\n{ast}\n")
    assert(types._symbol_Q(ast[1]))
    compiled_strings = COMPILE(ast[2], env, prefix=f"{prefix}_0")
    compiled_strings += \
[f"""
# compile_def
def _{prefix} ():
    consts = [_{prefix}_0()]
    def {prefix} (env):
        result = env.set("{ast[1]}", consts[0](env))
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""]
    return compiled_strings

def compile_let (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    compiled_strings = []
    for i in range(1, len(ast[1]), 2):
        compiled_strings += COMPILE(ast[1][i], env, prefix=f"{prefix}_{(i-1)//2}")
    compiled_strings += COMPILE(ast[2], env, prefix=f"{prefix}_{(i+1)//2}")
    # NOTE The parameters are not pushed into the consts list, as
    # they are just symbols and can easily be serialized.
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
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_do (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
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
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_if (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    cond_compiled_strings = COMPILE(ast[1], env, prefix=f"{prefix}_0")
    if_compiled_strings   = COMPILE(ast[2], env, prefix=f"{prefix}_1")
    else_compiled_strings = COMPILE(ast[3], env, prefix=f"{prefix}_2")
    compiled_string = \
f"""
# compile_if
def _{prefix} ():
    consts = [
      _{prefix}_0(), # ast[1] .. cond
      _{prefix}_1(), # ast[2] .. if
      _{prefix}_2(), # ast[3] .. else
    ]
    def {prefix} (env):
        cond = consts[0](env)
        if not (cond is None or cond is False):
            result = consts[1](env)
        else:
            result = consts[2](env)
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = cond_compiled_strings + if_compiled_strings + else_compiled_strings + [compiled_string]
    return compiled_strings

def compile_funcall (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    compiled_string = \
f"""
# compile_funcall
def _{prefix} ():
    consts = ["""
    for i in range(0, len(ast)):
        compiled_string += \
f"""
      _{prefix}_{i}(), # ast[{i}]"""
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
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = [compiled_string]
    for i in range(0, len(ast)):
        compiled_strings = COMPILE(ast[i], env, prefix=f"{prefix}_{i}") + compiled_strings
    return compiled_strings

def compile_literal (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
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
    consts = [
      _consts["{prefix}"], # ast
    ]
    def {prefix} (env):
        result = consts[0]
        #logger.debug(f"result: {{result}}")
        return result
    # Once _{prefix} is called, the ast is embedded into the closure {prefix}, so it's time to quickly remove the link to that ast object.
    popped = _consts.pop("{prefix}")
    #logger.debug(f"popped : {{popped}}")
    #logger.debug(f"_consts: {{_consts}}")
    return {prefix}
"""]
    return compiled_strings

def compile_fn (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    params, body = ast[1], ast[2]
    compiled_string = \
f"""
# compile_fn
def _{prefix} ():
    consts = [
      _{prefix}_0(), # [ast[2]]
    ]
    def {prefix} (env):
        def {prefix}_fn (*args):
            params, body = {params}, "{str(body)}"
            #logger.debug(f"params: {{params}}")
            #logger.debug(f" body : {{body}}")
            #logger.debug(f" args : {{args}}")
            local_env = Env(env, params, types.List(args))
            result = consts[0](local_env)
            #logger.debug(f"result: {{result}}")
            return result
        result = {prefix}_fn
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    compiled_strings = COMPILE(body, env, prefix=f"{prefix}_0") + [compiled_string]
    return compiled_strings

def compile_hashmap (ast, env, prefix):
    #logger.debug(f"Compiling AST:\n{ast}\n")
    assert(types._hash_map_Q(ast))
    compiled_string = \
f"""
# compile_hashmap
def _{prefix} ():
    consts = ["""
    i, hashmap_literal_string = 0, ""
    for key_ast, val_ast in ast.items():
      assert(types._symbol_Q(key_ast) or \
             types._keyword_Q(key_ast) or \
             types._string_Q(key_ast))
      compiled_string += \
f"""
      _{prefix}_{i}(), # val_ast[{i}]"""
      hashmap_literal_string += f""""{key_ast}",consts[{i}](env),"""
      i += 1
    compiled_string += \
f"""
    ]
    def {prefix} (env):
        result = types._hash_map({hashmap_literal_string})
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
    i, compiled_strings = 0, []
    for key_ast, val_ast in ast.items():
        compiled_strings += COMPILE(val_ast, env, prefix=f"{prefix}_{i}")
        i += 1
    compiled_strings += [compiled_string]
    return compiled_strings

def compile_try (ast, env, prefix):
    if len(ast) < 3 or ast[2][0] != "catch*":
        compiled_strings = COMPILE(ast[1], env, prefix=prefix)
    elif ast[2][0] == "catch*":
        assert types._symbol_Q(ast[2][1])
        compiled_string = \
f"""
# compile_try
def _{prefix} ():
    consts = [
      _{prefix}_0(), # AST[1]
      _{prefix}_1(), # AST[2][2]
    ]
    def {prefix} (env):
        err = None
        try:
            result = consts[0](env) # AST[1]
        except types.MalException as exc:
            err = exc.object
        except Exception as exc:
            err = exc.args[0]
        if err:
            catch_env = Env(env, ["{ast[2][1]}"], [err])
            result = consts[1](catch_env)
        #logger.debug(f"result: {{result}}")
        return result
    return {prefix}
"""
        compiled_strings  = COMPILE(ast[1],    env, prefix=f"{prefix}_0")
        compiled_strings += COMPILE(ast[2][2], env, prefix=f"{prefix}_1")
        compiled_strings += [compiled_string]
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
        #logger.debug(f"Macro Expansion\n> AST:\n{ast}\n")
        macro_fn = env.get(ast[0])
        ast = macro_fn(*ast[1:])
        #logger.debug(f"Macro Expansion Finished ({count} fold(s)).\n> New AST:\n{ast}\n> Old AST:\n{unexpanded_ast}\n")
    return ast

def COMPILE (ast, env, prefix="blk"):
    # env is for macroexpansion
    #logger.debug(f"Compiling AST:\n{ast}\n")
    ast = macroexpand(ast, env)
    if types._symbol_Q(ast):
        compiled_strings = compile_symbol(ast, env, prefix)
    elif types._list_Q(ast):
        if len(ast) == 0:
            compiled_strings = compile_literal(ast, env, prefix)
        elif ast[0] == "quote":
            compiled_strings = compile_literal(ast[1], env, prefix)
        elif ast[0] == "quasiquote": # TODO Maybe do it with defmacro!
            compiled_strings = COMPILE(quasiquote(ast[1]), env, prefix)
        elif ast[0] == "quasiquoteexpand": # TODO Maybe do it with defmacro!
            compiled_strings = compile_literal(quasiquote(ast[1]), env, prefix)
        elif ast[0] == "macroexpand": # TODO Maybe do it with defmacro!
            compiled_strings = compile_literal(macroexpand(ast[1], env), env, prefix)
        elif ast[0] == "if":
            compiled_strings = compile_if(ast, env, prefix)
        elif ast[0] == "def!":
            compiled_strings = compile_def(ast, env, prefix)
        elif ast[0] == "let*":
            compiled_strings = compile_let(ast, env, prefix)
        elif ast[0] == "do":
            compiled_strings = compile_do(ast, env, prefix)
        elif ast[0] == "fn*":
            compiled_strings = compile_fn(ast, env, prefix)
        elif ast[0] == "try*":
            compiled_strings = compile_try(ast, env, prefix)
        else:
            compiled_strings = compile_funcall(ast, env, prefix)
    elif types._vector_Q(ast):
        compiled_strings = COMPILE(types.List([types.Symbol("vector")]+list(ast)), env, prefix)
    elif types._hash_map_Q(ast):
        compiled_strings = compile_hashmap(ast, env, prefix)
    elif types._scalar_Q(ast)    or \
         types._keyword_Q(ast)   or \
         types._function_Q(ast):
        compiled_strings = compile_literal(ast, env, prefix)
    else:
        raise Exception(f"Unknown AST Type: {type(ast)}")
    return compiled_strings
