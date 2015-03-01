require! LiveScript

require! 'prelude-ls': {map}
require! './printer.ls': {pr-str}

{MalList, MalVec} = require './types.ls'

export eval-mal = (env, ast) -->
    evaluated = switch ast.type
        | \SYM => env[ast.value]
        | \LIST => new MalList map (eval-mal env), ast.value
        | \VEC => new MalVec map (eval-mal env), ast.value
        | otherwise => ast
    if evaluated.type is \LIST
        [fn, ...args] = evaluated.value
        if fn.type not in [\BUILTIN, \LAMBDA]
            throw new Error "Not a function: #{ pr-str fn }"
        fn.fn args
    else
        evaluated

