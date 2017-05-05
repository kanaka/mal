{is-type, map, join} = require 'prelude-ls'

export pr_str = (ast) ->
    if is-type \Array ast
        '(' + (ast |> map pr_str |> join ' ') + ')'
    else
        {type, value} = ast
        switch type
        | \int => value
        | \string => value
        | \symbol => value
