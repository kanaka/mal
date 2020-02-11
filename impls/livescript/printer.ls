{is-type, map, join, obj-to-pairs} = require 'prelude-ls'
{keyword-prefix} = require './reader'


export pr_str = ({type, value}: ast, print_readably=true) ->
    switch type
    | \const    => value
    | \int      => value
    | \string   =>
        if print_readably
        then encode-string value
        else value
    | \symbol   => value
    | \keyword  => value
    | \list     => '(' + (pr_list value, print_readably) + ')'
    | \vector   => '[' + (pr_list value, print_readably) + ']'
    | \map      => '{' + (pr_map value, print_readably) + '}'
    | \function => '#<function>'
    | \atom     => '(atom ' + (pr_str value) + ')'


encode-string = (str) ->
    str |> (.replace /[\n\"\\]/g,
            (ch) -> switch ch
                | '\n' => '\\n'
                | '"' => '\\"'
                | '\\' => '\\\\')
        |> (enc) -> "\"#{enc}\""


pr_list = (list, print_readably) ->
    list |> map (ast) -> pr_str ast, print_readably
         |> join ' '


pr_map_key = (key, print_readably) ->
    if key.startsWith keyword-prefix
        key.substring 1
    else if print_readably
        encode-string key
    else
        key

pr_map = (obj, print_readably) ->
    obj |> obj-to-pairs
        |> map ([key, value]) ->
            key_str = pr_map_key key, print_readably
            value_str = pr_str value, print_readably
            key_str + ' ' + value_str
        |> join ' '
