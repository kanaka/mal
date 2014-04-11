require "types"

def _pr_str(obj, print_readably=true)
    _r = print_readably
    return case obj
        when List
            "(" + obj.map{|x| _pr_str(x, _r)}.join(" ") + ")"
        when Vector
            "[" + obj.map{|x| _pr_str(x, _r)}.join(" ") + "]"
        when String
            if _r
                "\"" + obj.gsub(/\\/, "\\\\") \
                          .gsub(/"/, "\\\\\"") \
                          .gsub(/\n/, "\\\\n") + "\""
            else
                obj
            end
        when nil
            "nil"
        else
            obj.to_s
    end
end
