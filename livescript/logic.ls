require! LiveScript

require! 'prelude-ls': {map, fold}
require! './builtins.ls': {DO, NIL, truthy, is-seq, mal-eql}
require! './printer.ls': {pr-str}
require! './env': {create-env, bind-value, get-value}

{sym, Macro, Lambda, MalList, MalVec, MalMap} = require './types.ls'

SPECIALS = <[ do let let* def! def fn* fn defn if or and ]>

evaluate = (env, ast) -> switch ast.type
    | \SYM => env[ast.value] or throw new Error "Undefined symbol: #{ ast.value }"
    | \LIST => new MalList map (eval-mal env), ast.value
    | \VEC => new MalVec map (eval-mal env), ast.value
    | \MAP => new MalMap [[(eval-mal env, k), (eval-mal env, ast.get(k))] for k in ast.keys()]
    | otherwise => ast

export eval-mal = (env, ast) --> while true
    return null unless ast?
    return (evaluate env, ast) if ast.type isnt \LIST

    ast = expand-macro env, ast
    return ast if ast.type isnt \LIST

    [form, ...args] = ast.value
    throw new Error("Empty call") unless form?
    switch form.value
        | 'def!', 'def' => return do-def env, args
        | 'defmacro!' => return do-fn-to-macro env, args
        | 'defmacro' => return do-defmacro env, args
        | 'macroexpand' => return expand-macro env, args[0]
        | 'fn', 'fn*' => return do-fn env, args
        | 'defn' => return do-defn env, args
        | 'quote' => return args[0]
        | 'quasiquote' => ast = do-quasi-quote args[0]
        | 'unquote' => ast = args[0]
        | 'let', 'let*' => [env, ast] = do-let env, args
        | 'do' => ast = do-do env, args
        | 'if' => ast = do-if env, args # should be macro
        | _ =>
            # Here we must be evaluating a call.
            application = evaluate env, ast
            [fn, ...args] = application.value
            if fn.type is \BUILTIN
                return fn.fn args # Cannot thunk.
            else if fn.type is \LAMBDA
                [env, ast] = apply-fn fn, args
            else
                throw new Error "Tried to call non-callable: #{ pr-str fn }"

apply-fn = (fn, args) -> [(fn.closure args), (wrap-do fn.body)]

expand-macro = (env, ast) ->
    while is-macro-call env, ast
        [name, ...args] = ast.value
        [env, do-form] = apply-fn (get-value env, name), args
        ast = eval-mal env, do-do env, do-form.value.slice(1)
    ast

do-defn = (env, [name, ...fn]) -> do-def env, [name, (do-fn env, fn)]

do-defmacro = (env, [name, ...fn]) -> do-def env, [name, (do-macro env, fn)]

is-macro-call = (env, ast) ->
    (ast?.type is \LIST) and (is-macro env, ast.value[0])

is-macro = (env, symbol) -> (get-value env, symbol)?.type is \MACRO

is-pair = (form) -> (is-seq form) and form.value.length

make-call = (name, ...args) -> new MalList [(sym name)] ++ args

do-quasi-quote = (ast) ->
    | not is-pair ast => make-call \quote, ast
    | _ =>
        [head, ...tail] = ast.value
        throw new Error("NO FST: #{ JSON.stringify(ast) }") unless head
        switch
            | mal-eql (sym \unquote), head => tail[0]
            | (is-pair head) and mal-eql (sym 'splice-unquote'), head.value[0] =>
                make-call \concat, head.value[1], (do-quasi-quote new MalList tail)
            | _ => make-call \cons, (do-quasi-quote head), (do-quasi-quote new MalList tail)

do-or = (env, exprs) ->
    return NIL unless exprs.length
    for e in exprs
        v = eval-mal env, e
        return v if truthy v
    return v

do-and = (env, [e, ...es]) ->
    combine = (a, b) -> if truthy a then (eval-mal env, b) else a
    fold combine, (eval-mal env, e), es

do-fn = (env, [names, ...bodies]) ->
    unless is-seq names
        throw new Error "Names must be a sequence, got: #{ pr-str names }"
    new Lambda env, names.value.slice(), bodies

do-macro = (env, [names, ...bodies]) ->
    unless is-seq names
        throw new Error "Names must be a sequence, got: #{ pr-str names }"
    new Macro env, names.value.slice(), bodies

do-if = (env, [test, when-true, when-false]:forms) ->
    unless forms.length in [2, 3]
        throw new Error "Expected 2 or 3 arguments to if, got #{ forms.length }"
    when-false ?= NIL
    ret = eval-mal env, test
    if (truthy ret) then when-true else when-false

do-def = (env, [key, value]:forms) ->
    unless forms.length is 2
        throw new Error "Expected 2 arguments to def, got #{ forms.length }"
    bind-value env, key, eval-mal env, value

# Form that takes a function definition, eg: (defmacro! name (fn* [] ))
do-fn-to-macro = (env, [key, value]) ->
    macro = do-macro env, value.value.slice(1)
    bind-value env, key, macro

wrap-do = (bodies) -> new MalList [DO].concat bodies

# Returns a new environment and a do-wrapped body.
do-let = (outer, [bindings, ...bodies]) ->
    env = create-env outer
    unless is-seq bindings
        throw new Error "Bindings must be a sequence, got: #{ pr-str bindings }"
    if bindings.value.length % 2
        throw new Error "There must be an even number of bindings"

    for i in [0 til bindings.value.length - 1 by 2]
        do-def env, [bindings.value[i], bindings.value[i + 1]]

    [env, (wrap-do bodies)]

do-do = (env, [...heads, last]) ->
    for body in heads
        ret = eval-mal env, body
    return last

