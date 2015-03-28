module printer

export pr_str

function pr_str(obj, print_readably=true)
    _r = print_readably
    if isa(obj, Array)
        "($(join([pr_str(o, _r) for o=obj], " ")))"
    elseif isa(obj, Tuple)
        "[$(join([pr_str(o, _r) for o=obj], " "))]"
    elseif isa(obj, Dict)
        "{$(join(["$(o[1]) $(o[2])" for o=obj], " "))}"
    elseif isa(obj, String)
        if _r
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
    else
        string(obj)
    end
end

end
