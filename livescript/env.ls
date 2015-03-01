require! LiveScript

require! 'prelude-ls': {zip}

require! './types.ls': types

# We are going to use prototypical inheritance to implement environments.
export create-env = (outer-scope = null, names = [], exprs = []) ->
    env = if outer-scope then (Object.create outer-scope) else {}
    i = 0

    if names.length > exprs.length
        throw new Error 'Not enough arguments'

    while name = names[i]
        throw new Error "Name is not a symbol" unless name.type is \SYM
        if name.value is '&'
            name = names[i + 1]
            throw new Error "Name is not a symbol" unless name.type is \SYM
            env[name.value] = new types.MalList exprs.slice i
        else
            env[name.value] = exprs[i]
        i++
    return env

