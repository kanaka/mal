import { _list_Q, _keyword_Q, Vector, Atom } from './types'

export function pr_str(obj, print_readably) {
    if (typeof print_readably === 'undefined') { print_readably = true }
    var _r = print_readably
    if (_list_Q(obj)) {
        return "(" + obj.map(e => pr_str(e,_r)).join(' ') + ")"
    } else if (obj instanceof Vector) {
        return "[" + obj.map(e => pr_str(e,_r)).join(' ') + "]"
    } else if (obj instanceof Map) {
        var ret = []
        for (let [k,v] of obj) {
            ret.push(pr_str(k,_r), pr_str(v,_r))
        }
        return "{" + ret.join(' ') + "}"
    } else if (typeof obj === "string") {
        if (_keyword_Q(obj)) {
            return ':' + obj.slice(1)
        } else if (_r) {
            return '"' + obj.replace(/\\/g, "\\\\")
                            .replace(/"/g, '\\"')
                            .replace(/\n/g, "\\n") + '"'
        } else {
            return obj
        }
    } else if (typeof obj === 'symbol') {
        return Symbol.keyFor(obj)
    } else if (obj === null) {
        return "nil"
    } else if (obj instanceof Atom) {
        return "(atom " + pr_str(obj.val,_r) + ")"
    } else {
        return obj.toString()
    }
}
