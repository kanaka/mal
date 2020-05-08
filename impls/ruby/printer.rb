require_relative "types"

def _pr_str(obj, print_readably=true)
    _r = print_readably
    return case obj
        when List
            "(" + obj.map{|x| _pr_str(x, _r)}.join(" ") + ")"
        when Vector
            "[" + obj.map{|x| _pr_str(x, _r)}.join(" ") + "]"
        when Hash
            ret = []
            obj.each{|k,v| ret.push(_pr_str(k,_r), _pr_str(v,_r))}
            "{" + ret.join(" ") + "}"
        when String
            if obj[0] == "\u029e"
                ":" + obj[1..-1]
            elsif _r
                obj.inspect  # escape special characters
            else
                obj
            end
        when Atom
            "(atom " + _pr_str(obj.val, true) + ")"
        when nil
            "nil"
        else
            obj.to_s
    end
end
