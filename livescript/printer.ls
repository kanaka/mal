require! LiveScript

{is-nil, is-atom} = require './builtins.ls'

export pr-str = (ast, escape = true) ->
    f = if escape then pr-str else str
    switch ast.type
        | \NIL => 'nil'
        | \INT, \FLOAT, \BOOL, \SYM => String(ast.value)
        | \KEYWORD => ":#{ ast.name }"
        | \BUILTIN => '(fn [& args] #NATIVE-FUNCTION)'
        | \LAMBDA => "(fn [#{ ast.names.map(f).join ' ' }] #{ ast.body.map(f).join ' ' })"
        | \MACRO => "(macro [#{ ast.names.map(f).join ' ' }] #{ ast.body.map(f).join ' ' })"
        | \STRING => JSON.stringify(ast.value)
        | \LIST => "(#{ ast.value.map(f).join ' ' })"
        | \VEC => "[#{ ast.value.map(f).join ' ' }]"
        | \MAP => "{#{ ast.pairs().map((p) -> "#{ f p.key } #{ f p.value }").join ' ' }}"
        | \ATOM => "(atom #{ f ast.value })"
        | \JSOBJECT => "(js-obj #{ ast.value })"

export str = (ast) ->
    switch ast.type
        | \STRING => ast.value
        | _ => pr-str ast, false
