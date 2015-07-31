export function new_env(outer={}, binds=[], exprs=[]) {
    var e = Object.setPrototypeOf({}, outer);
    // Bind symbols in binds to values in exprs
    for (var i=0; i<binds.length; i++) {
        if (binds[i] == "&") {
            e[binds[i+1]] = exprs.slice(i); // variable length arguments
            break;
        } else {
            e[binds[i]] = exprs[i];
        }
    }
    Object.defineProperties(e, {
        "get": {
                value: function(sym) {
                    if (sym.name in this) {
                        return this[sym.name]
                    } else {
                        throw Error("'" + sym + "' not found")
                    }
                }
        },
        "set": {
            value: function(sym, val) {
                return this[sym.name] = val;
            }
        }
    });
    return e;
}
