require! LiveScript

require! 'prelude-ls': {zip}
require! './builtins.ls': builtins
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
        if builtins.mal-eql name, AMP
            throw new Error 'Can only slurp once' if slurped
            vals = exprs.slice i
            name = names[++i]
            throw new Error "No binding provided when slurping" unless name
            bind-value env, name, (new types.MalList vals)
            slurped = true
        else
            value = exprs[i]
            unless value
                throw new MalArgumentError "Missing binding for #{ pr-str name }"
            bind-value env, name, value
        i++

class MalArgumentError extends Error

    (@message) ->

    name: \MalArgumentError


export bind-value = (env, key, value) -->
    switch key.type
        | \SYM => env[key.value] = value
        | \VEC => destructure-seq env, key, value
        | \MAP => destructure-map env, key, value
        | otherwise => throw new Error "Cannot bind to #{ that }: #{ pr-str key }"

export get-value = (env, key) ->
    switch key.type
        | \SYM =>
            if /^js\/\S+/.test key.value
                obj = global[key.value.slice(3)]
                if obj? then (new types.JSObject obj) else null
            if /\S+\/\S+/.test key.value
                [obj, prop] = key.value.split '/'
                new types.JSObject global[obj][prop]
            else if /^\.\S+/.test key.value
                fn = types.from-js . (types.call-js-meth key.value.slice 1)
                new types.Builtin fn
            else if /\S+\.$/.test key.value
                constr = key.value.slice 0, (key.value.length - 1)
                fn = types.from-js . (types.instantiate constr)
                new types.Builtin fn
            else
                env[key.value]
        | otherwise => null

destructure-seq = (env, key, value) -->
    unless builtins.is-seq value
        throw new Error "Expected sequence - got #{ value.type }"
    bind-all env, key.value, value.value

destructure-map = (env, key, value) ->
    unless value.type is \MAP
        throw new Error "Expected map - got #{ value.type }"
    for k in key.keys()
        v = value.get k
        throw new Error "Missing binding for #{ pr-str k }" unless v
        bind-value env, key.get(k), v

