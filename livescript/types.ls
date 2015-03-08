require! {
    LiveScript
    './env.ls': env
    'prelude-ls': {all, find, map, zip}
}
{atomic-types, mal-eql, is-atom, is-nil} = require './builtins.ls'

DEREF = {type: \SYM, value: 'deref'}
QUOTE = {type: \SYM, value: 'quote'}
QUASIQUOTE = {type: \SYM, value: 'quasiquote'}
UNQUOTE = {type: \SYM, value: 'unquote'}
SPLICE_UNQUOTE = {type: \SYM, value: 'splice-unquote'}
WITH_META = {type: \SYM, value: 'with-meta'}

keywords = {}
keyword-array = Array 1000
keyword-counter = 0

keyword-by-value = (i) -> keyword-array[i]

export keyword = (name) -> keywords[name] ?= do ->
    value = keyword-counter++
    keyword-array[value] = {type: \KEYWORD, value, name}

export deref = (ref) -> new MalList [DEREF, ref]

export quote = (form) -> new MalList [QUOTE, form]

export quasiquote = (form) -> new MalList [QUASIQUOTE, form]

export unquote = (form) -> new MalList [UNQUOTE, form]

export splice-unquote = (form) -> new MalList [SPLICE_UNQUOTE, form]

export with-meta = (meta, data) -> new MalList [WITH_META, data, meta]

export string = (s) -> type: \STRING, value: s

export sym = (s) -> type: \SYM, value: s

export int = (i) -> type: \INT, value: i

export float = (f) -> type: \FLOAT, value: f

export class Builtin

    (@fn) ->

    type: \BUILTIN

export class Lambda

    (@env, @names, @body) ->

    closure: (exprs) -> env.create-env @env, @names, exprs

    type: \LAMBDA

export class Macro extends Lambda

    type: \MACRO

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
                ks.push (if type is \KEYWORD then (keyword-by-value k) else {type, value: k})
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

    keys: -> (map (.key), @entries) ++ (atomic-keys @)

    equals: (b) ->
        return false unless b.type is \MAP
        my-keys = @keys()
        their-keys = b.keys()
        return false unless my-keys.length is their-keys.length
        all ((k) ~> mal-eql @get(k), b.get(k)), my-keys

export class MalList

    (@value) ->

    type: \LIST

    equals: (b) ->
        return false unless b instanceof MalList
        return false if b.value.length isnt @value.length
        all (-> mal-eql it[0], it[1]), (zip @value, b.value)

    construct: (elems = []) -> new MalList elems

    cons: (x) -> @construct [x] ++ @value

    conj: (x) -> @construct @value ++ [x]

export class MalVec extends MalList

    type: \VEC

    construct: (elems = []) -> new MalVec elems

