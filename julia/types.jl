module types

export MalException, MalFunc, sequential_Q, equal_Q, hash_map, Atom

import Base.copy

type MalException <: Exception
    malval
end

type MalFunc
    fn::Function
    ast
    env
    params
    ismacro
    meta
end

# ismacro default to false
function MalFunc(fn, ast, env, params)
    MalFunc(fn, ast, env, params, false, nothing)
end

function copy(f::MalFunc)
    MalFunc(f.fn, f.ast, f.env, f.params, f.ismacro, f.meta)
end

function sequential_Q(obj)
    isa(obj, Array) || isa(obj, Tuple)
end

function equal_Q(a, b)
    ota = typeof(a)
    otb = typeof(b)
    if !(ota === otb || (sequential_Q(a) && sequential_Q(b)))
        return false
    end

    if sequential_Q(a)
        tuple(a...) == tuple(b...)
    elseif isa(a,String)
        a == b
    else
        a === b
    end
end

function hash_map(lst...)
    hm = Dict()
    for i = 1:2:length(lst)
        hm[lst[i]] = lst[i+1]
    end
    hm
end

type Atom
    val
end

end


