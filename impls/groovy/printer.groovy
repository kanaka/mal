import groovy.json.StringEscapeUtils
import types
import types.MalSymbol
import types.MalAtom


class printer {
    def static _pr_list(lst, sep, Boolean print_readably) {
        return lst.collect{ e -> pr_str(e, print_readably) }.join(sep)
    }

    def static pr_str(exp, Boolean print_readably) {
        def _r = print_readably
        switch (exp) {
            case { types.list_Q(exp) }:
                def lst = exp.collect { pr_str(it, _r) }
                return "(${lst.join(" ")})"
            case { types.vector_Q(exp) }:
                def lst = exp.collect { pr_str(it, _r) }
                return "[${lst.join(" ")}]"
            case Map:
                def lst = []
                exp.each { k,v -> lst.add(pr_str(k,_r)); lst.add(pr_str(v,_r)) }
                return "{${lst.join(" ")}}"
            case String:
                if (types.keyword_Q(exp)) {
                    return ":" + exp.drop(1)
                } else if (print_readably) {
                    return "\"${StringEscapeUtils.escapeJava(exp)}\""
                } else {
                    return exp
                }
            case null:
                return 'nil'
            case MalSymbol:
                return exp.value
            case MalAtom:
                return "(atom ${exp.value})"
            default:
                return exp.toString()
        }
    }
}

