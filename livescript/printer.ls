{is-type, map, join} = require 'prelude-ls'

pr_list = (list) -> list |> map pr_str |> join ' '

export pr_str = ({type, value}: ast) ->
    switch type
    | \int => value
    | \string => value
    | \symbol => value
    | \list => '(' + (pr_list value) + ')'
    | \vector => '[' + (pr_list value) + ']'
