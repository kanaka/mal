module types

export MalFunc

type MalFunc
    fn::Function
    ast
    env
    params
    ismacro
end

# ismacro default to false
function MalFunc(fn, ast, env, params)
    MalFunc(fn, ast, env, params, false)
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
    else
        a === b
    end
end

end


