require! {
    LiveScript
    './env.ls': env
    'prelude-ls': {all, find, map, zip}
}
{atomic-types, mal-eql, is-atom, is-nil} = require './builtins.ls'

export class Builtin
    
    (@fn) ->

    type: \BUILTIN

export class Lambda

    (@env, @names, @body) ->

    closure: (exprs) -> env.create-env @env, @names, exprs

    type: \LAMBDA

export class MalMap

    (pairs = []) ->
        @stores = {}
        @entries = []
        for t in atomic-types
            @stores[t] = {}

        for [k, v] in pairs
            @set k, v

    set-atom = (m, k, v) ->
        m.stores[k.type][k.value] = v

    get-atom = (m, k) ->
        m.stores[k.type][k.value]

    entry-eql = (k, entry) --> mal-eql k, entry.key

    get-entry = (m, k) -> find (entry-eql k), m.entries

    set-nil = (m, v) -> m._nil = v

    set-obj = (m, k, v) ->
        if pair = get-entry m, k
            pair.value = v
        else
            m.entries.push {key: k, value: v}

    atomic-keys = (m) ->
        ks = []
        for type, store of m.stores
            for k, v of store
                ks.push {type, value: k}
        ks ++ (if m._nil then [NIL] else [])

    type: \MAP

    set: (k, v) ->
        | is-nil k => set-nil @, v
        | is-atom k => set-atom @, k, v
        | otherwise => set-obj @, k, v

    get: (k) ->
        | is-nil k => @_nil
        | is-atom k => get-atom @, k
        | otherwise => get-entry(@, k)?.value

    keys: -> (map (.0), @entries) ++ (atomic-keys @)

    equals: (b) ->
        return false unless b.type is \MAP
        my-keys = @keys
        their-keys = b.keys()
        return false unless my-keys.length is their-keys.length
        all ((k) ~> mal-eql @get(k), b.get(k)), my-keys

export class MalList

    (@value) ->

    type: \LIST

    equals: (b) ->
        return false unless b.type is @type
        return false if b.value.length isnt @value.length
        all (-> mal-eql it[0], it[1]), (zip @value, b.value)

export class MalVec extends MalList

    type: \VEC

