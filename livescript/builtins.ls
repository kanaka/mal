export atomic-types = <[ NIL BOOL INT FLOAT SYM STRING KEYWORD]>

export function is-atom val then val.type in atomic-types
export function is-nil val then val.type is \NIL
export function is-seq val then val.type in [\LIST, \VEC]

export mal-eql = (a, b) ->
    | is-atom a => a.value is b.value
    | a.type is b.type => a.equals b
    | otherwise => false

