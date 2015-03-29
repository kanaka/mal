module core

import reader
using printer

export ns

ns = {
    symbol("=") => (a,b) -> a == b,

    symbol("pr-str") => (a...) -> join(map((e)->pr_str(e, true),a)," "),
    :str => (a...) -> join(map((e)->pr_str(e, false),a),""),
    :prn => (a...) -> println(join(map((e)->pr_str(e, true),a)," ")),
    :println => (a...) -> println(join(map((e)->pr_str(e, false),a)," ")),
    symbol("read-string") => (a) -> reader.read_str(a),
    :slurp => (a) -> readall(open(a)),

    :< => <,
    :<= => <=,
    :> => >,
    :>= => >=,
    :+ => +,
    :- => -,
    symbol("*") => *,
    :/ => div,

    :list => (a...) -> [a...],
    symbol("list?") => (a) -> isa(a, Array),

    symbol("empty?") => isempty,
    :count => (a) -> a == nothing ? 0 : length(a),
    }

end
