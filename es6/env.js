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
    return e;
}
export function env_get(env, sym) {
    if (sym.name in env) {
        return env[sym.name]
    } else {
        throw Error("'" + sym + "' not found")
    }
}
export function env_set(env, sym, val) {
    return env[sym.name] = val;
}
