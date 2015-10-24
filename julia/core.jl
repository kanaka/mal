module core

import types
import reader
using printer
import readline_mod

export ns

function concat(args...)
    res = []
    for a=args
        res = [res; Any[a...]]
    end
    res
end

function do_apply(f, all_args...)
    fn = isa(f,types.MalFunc) ? f.fn : f
    args = concat(all_args[1:end-1], all_args[end])
    fn(args...)
end

function with_meta(obj, meta)
    new_obj = types.copy(obj)
    new_obj.meta = meta
    new_obj
end

ns = Dict{Any,Any}(
    symbol("=") => (a,b) -> types.equal_Q(a, b),
    :throw => (a) -> throw(types.MalException(a)),

    symbol("nil?") => (a) -> a === nothing,
    symbol("true?") => (a) -> a === true,
    symbol("false?") => (a) -> a === false,
    symbol("symbol") => (a) -> symbol(a),
    symbol("symbol?") => (a) -> typeof(a) === Symbol,
    symbol("keyword") => (a) -> a[1] == '\u029e' ? a : "\u029e$(a)",
    symbol("keyword?") => (a) -> isa(a,AbstractString) && a[1] == '\u029e',

    symbol("pr-str") => (a...) -> join(map((e)->pr_str(e, true),a)," "),
    :str => (a...) -> join(map((e)->pr_str(e, false),a),""),
    :prn => (a...) -> println(join(map((e)->pr_str(e, true),a)," ")),
    :println => (a...) -> println(join(map((e)->pr_str(e, false),a)," ")),
    symbol("read-string") => (a) -> reader.read_str(a),
    :readline => readline_mod.do_readline,
    :slurp => (a) -> readall(open(a)),

    :< => <,
    :<= => <=,
    :> => >,
    :>= => >=,
    :+ => +,
    :- => -,
    symbol("*") => *,
    :/ => div,
    symbol("time-ms") => () -> round(Int, time()*1000),

    :list => (a...) -> Any[a...],
    symbol("list?") => (a) -> isa(a, Array),
    :vector => (a...) -> tuple(a...),
    symbol("vector?") => (a) -> isa(a, Tuple),
    symbol("hash-map") => types.hash_map,
    symbol("map?") => (a) -> isa(a, Dict),
    :assoc => (a, b...) -> merge(a, types.hash_map(b...)),
    :dissoc => (a, b...) -> foldl((x,y) -> delete!(x,y),copy(a), b),
    :get => (a,b) -> a === nothing ? nothing : get(a,b,nothing),
    symbol("contains?") => haskey,
    :keys => (a) -> [keys(a)...],
    :vals => (a) -> [values(a)...],

    symbol("sequential?") => types.sequential_Q,
    :cons => (a,b) -> [Any[a]; Any[b...]],
    :concat => concat,
    :nth => (a,b) -> b+1 > length(a) ? error("nth: index out of range") : a[b+1],
    :first => (a) -> isempty(a) ? nothing : first(a),
    :rest => (a) -> Any[a[2:end]...],
    symbol("empty?") => isempty,
    :count => (a) -> a == nothing ? 0 : length(a),
    :apply => do_apply,
    :map => (a,b) -> isa(a,types.MalFunc) ? [map(a.fn,b)...] : [map(a,b)...],

    :conj => nothing,

    :meta => (a) -> isa(a,types.MalFunc) ? a.meta : nothing,
    symbol("with-meta") => with_meta,
    :atom => (a) -> types.Atom(a),
    symbol("atom?") => (a) -> isa(a,types.Atom),
    :deref => (a) -> a.val,
    :reset! => (a,b) -> a.val = b,
    :swap! => (a,b,c...) -> a.val = do_apply(b, a.val, c),
    )

end
