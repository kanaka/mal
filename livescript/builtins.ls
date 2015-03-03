export atomic-types = <[ NIL BOOL INT FLOAT SYM STRING KEYWORD ]>

export function is-atom val then val.type in atomic-types
export function is-nil val then val.type is \NIL
export function is-seq val then val.type in [\LIST, \VEC]
export function is-number val then val.type in [\INT, \FLOAT]

export truthy = (val) ->
    | val.type in [\NIL, \BOOL] => !! val.value
    | _ => true

export mal-eql = (a, b) ->
    | is-atom a => a.value is b.value
    | a.type is b.type => a.equals b
    | otherwise => false

export NIL = {type: \NIL, value: null}
export TRUE = {type: \BOOL, value: true}
export FALSE = {type: \BOOL, value: false}
export DO = {type: \SYM, value: 'do'}
