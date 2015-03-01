require! LiveScript

{is-nil, is-atom} = require './builtins.ls'

export pr-str = (ast) ->
    switch ast.type
        | \NIL => 'nil'
        | \SYM => ast.value
        | \INT, \FLOAT => String(ast.value)
        | \BUILTIN => '#NATIVE-FUNCTION'
        | \LAMBDA => "(fn [#{ ast.names.map(pr-str).join ' ' }] #{ ast.body.map(pr-str).join ' ' })"
        | \STRING => JSON.stringify(ast.value)
        | \LIST => "(#{ ast.value.map(pr-str).join ' ' })"
        | \VEC => "[#{ ast.value.map(pr-str).join ' ' }]"
        | \MAP => "{#{ ast.keys().map((k) -> "#{ pr-str k } #{ pr-str ast.get k }").join ' ' }}"


