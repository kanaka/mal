require! {
    LiveScript
    './printer.ls': printer
    'prelude-ls': {map, zip, partition}
}

export atomic-types = <[ NIL BOOL INT FLOAT SYM STRING KEYWORD ]>

export function is-atom val then val.type in atomic-types
export function is-nil val then val.type is \NIL
export function is-seq val then val.type in [\LIST, \VEC]
export function is-number val then val.type in [\INT, \FLOAT]
export function is-callable val then val?.type in [\BUILTIN, \LAMBDA]

export truthy = (val) ->
    | val.type in [\NIL, \BOOL] => !! val.value
    | _ => true

export mal-eql = (a, b) ->
    | is-atom a => a.type is b.type and a.value is b.value
    | (is-seq a) and (is-seq b) => a.equals b
    | a.type is b.type => a.equals b
    | otherwise => false

export NIL = {type: \NIL, value: null}
export TRUE = {type: \BOOL, value: true}
export FALSE = {type: \BOOL, value: false}
export DO = {type: \SYM, value: 'do'}

# [k, v, k, v...] -> [[k, v]]
export to-pairs = (xs) -> zip.apply null, keys-and-vals xs
# [k, v, k, v...] -> [[k], [v]]
keys-and-vals = (xs) ->
    [0 to xs.length] |> (zip xs) |> ks-vs |> discard-indices

ks-vs = partition (.1) >> (% 2) >> (is 0)
discard-indices = map (map (.0))
