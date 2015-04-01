module printer

import types

export pr_str

function pr_str(obj, print_readably=true)
    _r = print_readably
    if isa(obj, Array)
        "($(join([pr_str(o, _r) for o=obj], " ")))"
    elseif isa(obj, Tuple)
        "[$(join([pr_str(o, _r) for o=obj], " "))]"
    elseif isa(obj, Dict)
        "{$(join(["$(pr_str(o[1],_r)) $(pr_str(o[2],_r))" for o=obj], " "))}"
    elseif isa(obj, String)
        if length(obj) > 0 && obj[1] == '\u029e'
            ":$(obj[2:end])"
        elseif _r
            str = replace(replace(replace(obj,
                                          "\\", "\\\\"),
                                 "\"", "\\\""),
                          "\n", "\\n")
            "\"$(str)\""
        else
            obj
        end
    elseif obj == nothing
        "nil"
    elseif typeof(obj) == types.MalFunc
        "(fn* $(pr_str(obj.params,true)) $(pr_str(obj.ast,true)))"
    elseif typeof(obj) == types.Atom
        "(atom $(pr_str(obj.val,true)))"
    elseif typeof(obj) == Function
        "#<native function: $(string(obj))>"
    else
        string(obj)
    end
end

end
