require! {
    LiveScript
    './env.ls': env
    './printer.ls': {pr-str}
    'prelude-ls': {fold, any, all, find, map, zip}
}

{NIL, FALSE, TRUE, atomic-types, mal-eql, is-atom, is-seq, is-nil} = require './builtins.ls'

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

export atom = (val) -> new Atom val

class Atom

    (@value) ->

    type: \ATOM

export class Builtin

    (@fn) ->

    type: \BUILTIN

export class Lambda

    (@env, @names, @body) ->

    closure: (exprs) ->
        try
            env.create-env @env, @names, exprs
        catch e
            if e.name is \MalArgumentError
                throw new Error "Not enough arguments to function: #{ e.message }"
            throw e

    type: \LAMBDA

export class Macro extends Lambda

    @fromLambda = ({env, names, body}) -> new Macro env, names, body

    type: \MACRO

DELETED = {}

export class MalMap

    (pairs = [], @parent = null) ->
        @stores = {}
        @entries = []
        for t in atomic-types
            @stores[t] = {}

        for [k, v] in pairs
            @set k, v

    set-atom = (m, k, v) ->
        m.stores[k.type][k.value] = v

    get-atom = (m, k) ->
        m?.stores[k.type][k.value]

    entry-eql = (k, entry) --> mal-eql k, entry.key

    get-entry = (m, k) -> find (entry-eql k), (m?.entries ? [])

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

    atomic-pairs = (m) ->
        [{key: k, value: (get-atom m, k)} for k in (atomic-keys m)]

    type: \MAP

    set: (k, v) ->
        | is-nil k => set-nil @, v
        | is-atom k => set-atom @, k, v
        | otherwise => set-obj @, k, v

    _get: (k) ->
        | is-nil k => @_nil or @parent?._nil
        | is-atom k => (get-atom @, k) or (get-atom @parent, k)
        | otherwise => ((get-entry @, k) or (get-entry @parent, k))?.value

    get: (k) ->
        v = @_get k
        if v is DELETED then null else v

    pairs: ->
        es = @entries ++ (atomic-pairs @)
        inherited = (@parent?.pairs() ? []).filter (e) ->
            not any (mal-eql e.key, _), map (.key), es
        [p for p in (es ++ inherited) when p.value isnt DELETED]

    keys: -> [p.key for p in @pairs()]

    values: -> [p.value for p in @pairs()]

    contains: (k) ->
        v = @_get k
        return false if v is DELETED
        v? or @parent?.contains(k)

    equals: (b) ->
        return false unless b.type is \MAP
        my-keys = @keys()
        their-keys = b.keys()
        return false unless my-keys.length is their-keys.length
        all ((k) ~> mal-eql @get(k), b.get(k)), my-keys

    assoc: (pairs = []) -> new MalMap pairs, @

    dissoc: (ks) -> new MalMap [[k, DELETED] for k in ks], @

    to-js-map: ->
        pairs = atomic-pairs @
        fold ((o, p) -> o[p.key.value] = p.value.value; o), {}, pairs

export class MalList

    (@value) ->

    type: \LIST

    equals: (b) ->
        return false unless b instanceof MalList
        return false if b.value.length isnt @value.length
        all (-> mal-eql it[0], it[1]), (zip @value, b.value)

    construct: (elems = []) -> new MalList elems

    # Cons always returns a list.
    cons: (x) -> new MalList [x] ++ @value

    conj: (x) -> @construct [x] ++ @value

    contains: (e) -> any (mal-eql e _), @value

export class MalVec extends MalList

    type: \VEC

    construct: (elems = []) -> new MalVec elems

    conj: (x) -> @construct @value ++ [x]

export class UserError extends Error

    (@data) ->

    name: 'UserError'

export class JSObject

    (@value) ->

    type: \JSOBJECT

export instantiate = (name, args) -->
    cls = global[name]
    xs = [to-js-val a for a in args]
    Temp = ->
    Temp.prototype = cls.prototype
    inst = new Temp
    ret = cls.apply inst, xs
    if (Object(ret) is ret) then ret else inst

export call-js-meth = (method, args) -->
    [invocant, ...xs] = args.map to-js-val
    invocant[method].apply invocant, xs

export to-js-val = (mal-val) ->
    | is-atom mal-val => mal-val.value
    | is-seq mal-val => mal-val.value.map to-js-val
    | mal-val.type is \MAP  => mal-val.to-js-map!
    | mal-val.type is \JSOBJECT => mal-val.value
    | _ => throw new Error "Cannot convert to js-value: #{ pr-str mal-val }"

export from-js = (js-val) ->
    | not js-val? => NIL
    | js-val is true => TRUE
    | js-val is false => FALSE
    | Array.isArray js-val => new MalVec js-val.map from-js
    | \string is typeof js-val => {type: \STRING, value: js-val}
    | \number is typeof js-val => {type: (if js-val % 1 then \FLOAT else \INT), value: js-val}
    | _ => new JSObject js-val
