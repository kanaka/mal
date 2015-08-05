import { _symbol, _symbol_Q, _list_Q, _vector_Q, _hash_map_Q, Atom } from './types'

export function pr_str(obj, print_readably) {
    if (typeof print_readably === 'undefined') { print_readably = true }
    var _r = print_readably
    if (_list_Q(obj)) {
        var ret = obj.map(function(e) { return pr_str(e,_r) })
        return "(" + ret.join(' ') + ")"
    } else if (_vector_Q(obj)) {
        var ret = obj.map(function(e) { return pr_str(e,_r) })
        return "[" + ret.join(' ') + "]"
    } else if (_hash_map_Q(obj)) {
        var ret = []
        for (let [k,v] of obj) {
            ret.push(pr_str(k,_r), pr_str(v,_r))
        }
        return "{" + ret.join(' ') + "}"
    } else if (typeof obj === "string") {
        if (obj[0] === '\u029e') {
            return ':' + obj.slice(1)
        } else if (_r) {
            return '"' + obj.replace(/\\/g, "\\\\")
                .replace(/"/g, '\\"')
                .replace(/\n/g, "\\n") + '"' // string
        } else {
            return obj
        }
    } else if (_symbol_Q(obj)) {
        return Symbol.keyFor(obj)
    } else if (obj === null) {
        return "nil"
    } else if (obj instanceof Atom) {
        return "(atom " + pr_str(obj.val,_r) + ")"
    } else {
        return obj.toString()
    }
}
