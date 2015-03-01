require! LiveScript

require! 'prelude-ls': {zip}
require! './builtins.ls': {is-seq, mal-eql}
require! './types.ls': types
require! './printer.ls': {pr-str}

AMP = {type: \SYM, value: '&'}

# We are going to use prototypical inheritance to implement environments.
export create-env = (outer-scope = null, names = [], exprs = []) ->
    env = if outer-scope then (Object.create outer-scope) else {}
    bind-all env, names, exprs
    return env

bind-all = (env, names = [], exprs = []) ->
    slurped = false
    i = 0
    while name = names[i]
        if mal-eql name, AMP
            throw new Error 'Can only slurp once' if slurped
            vals = exprs.slice i
            name = names[++i]
            throw new Error "No binding provided when slurping" unless name
            bind-value env, name, (new types.MalVec vals)
            slurped = true
        else
            value = exprs[i]
            throw new Error "Missing binding for #{ pr-str name }" unless value
            bind-value env, name, value
        i++

export bind-value = (env, key, value) -->
    switch key.type
        | \SYM => env[key.value] = value
        | \VEC => destructure-seq env, key, value
        | \MAP => destructure-map env, key, value
        | otherwise => throw new Error "Cannot bind to #{ that }: #{ pr-str key }"

destructure-seq = (env, key, value) -->
    unless is-seq value
        throw new Error "Expected sequence - got #{ value.type }"
    bind-all env, key.value, value.value

destructure-map = (env, key, value) ->
    unless value.type is \MAP
        throw new Error "Expected map - got #{ value.type }"
    for k in key.keys()
        v = value.get k
        throw new Error "Missing binding for #{ pr-str k }" unless v
        bind-value env, key.get(k), v

