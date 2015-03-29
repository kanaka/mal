module core

import types
import reader
using printer

export ns


function concat(args...)
    res = {}
    for a=args
        res = [res, Any[a...]]
    end
    res
end

ns = {
    symbol("=") => (a,b) -> types.equal_Q(a, b),

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

    :list => (a...) -> Any[a...],
    symbol("list?") => (a) -> isa(a, Array),

    :cons => (a,b) -> [Any[a], Any[b...]],
    :concat => concat,
    :nth => (a,b) -> b+1 > length(a) ? error("nth: index out of range") : a[b+1],
    :first => (a) -> isempty(a) ? nothing : first(a),
    :rest => (a) -> Any[a[2:end]...],
    symbol("empty?") => isempty,
    :count => (a) -> a == nothing ? 0 : length(a),
    }

end
