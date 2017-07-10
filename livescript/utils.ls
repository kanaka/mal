{map} = require 'prelude-ls'


export list-to-pairs = (list) ->
    [0 to (list.length - 2) by 2] \
        |> map (idx) -> [list[idx], list[idx+1]]
