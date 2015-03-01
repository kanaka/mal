require! LiveScript

{is-nil, is-atom} = require './builtins.ls'

export pr-str = (ast) ->
    switch ast.type
        | \NIL => 'nil'
        | \SYM => ast.value
        | \INT, \FLOAT => String(ast.value)
        | \STRING => JSON.stringify(ast.value)
        | \LIST => "(#{ ast.value.map(pr-str).join ' ' })"
        | \VEC => "[#{ ast.value.map(pr-str).join ' ' }]"
        | \MAP => "{#{ ast.keys().map((k) -> "#{ pr-str k } #{ pr-str ast.get k }").join ' ' }}"

