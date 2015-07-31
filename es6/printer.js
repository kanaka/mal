import { Sym } from './types';

export function pr_str(obj, print_readably) {
    if (typeof print_readably === 'undefined') { print_readably = true; }
    var _r = print_readably;
    if (obj instanceof Array) {
        var ret = obj.map(function(e) { return pr_str(e,_r); });
        return "(" + ret.join(' ') + ")";
    } else if (typeof obj === "string") {
        if (obj[0] === '\u029e') {
            return ':' + obj.slice(1);
        } else if (_r) {
            return '"' + obj.replace(/\\/g, "\\\\")
                .replace(/"/g, '\\"')
                .replace(/\n/g, "\\n") + '"'; // string
        } else {
            return obj;
        }
    } else if (obj === null) {
        return "nil";
    } else {
        return obj.toString();
    }
}
