// General functions

//function _sequential_Q(lst) { return _list_Q(lst) || _vector_Q(lst); }
export const _sequential_Q = (lst) => _list_Q(lst)

export function _obj_type(obj) {
    if      (obj instanceof Sym) { return 'symbol'; }
    else if (_list_Q(obj)) {       return 'list'; }
    else if (obj === null) {       return 'nil'; }
    else if (obj === true) {       return 'true'; }
    else if (obj === false) {      return 'false'; }
    else {
        switch (typeof(obj)) {
        case 'number':   return 'number';
        case 'function': return 'function';
        case 'string': return obj[0] == '\u029e' ? 'keyword' : 'string';
        default: throw new Error("Unknown type '" + typeof(obj) + "'");
        }
    }
}


export function _equal_Q (a, b) {
    var ota = _obj_type(a), otb = _obj_type(b);
    if (!(ota === otb || (_sequential_Q(a) && _sequential_Q(b)))) {
        return false;
    }
    switch (ota) {
    case 'symbol': return a.value === b.value;
    case 'list':
    case 'vector':
        if (a.length !== b.length) { return false; }
        for (var i=0; i<a.length; i++) {
            if (! _equal_Q(a[i], b[i])) { return false; }
        }
        return true;
//    case 'hash-map':
//        var akeys = Object.keys(a).sort(),
//            bkeys = Object.keys(b).sort();
//        if (akeys.length !== bkeys.length) { return false; }
//        for (var i=0; i<akeys.length; i++) {
//            if (akeys[i] !== bkeys[i]) { return false; }
//            if (! equal_Q(a[akeys[i]], b[bkeys[i]])) { return false; }
//        }
//        return true;
    default:
        return a === b;
    }
}

// Functions
export function _malfunc(f, ast, env, params) {
    f.ast = ast;
    f.env = env;
    f.params = params;
    f.meta = null;
    f.ismacro = false;
    return f;
}
export const _malfunc_Q = f => f.ast ? true : false;

// Symbols
export class Sym {
    constructor(name) { this.name = name; }
    toString() { return this.name; }
    name() { return this.name; }
}
export const _symbol = name => new Sym(name);
export const _symbol_Q = obj => obj instanceof Sym;

// Lists
export const _list_Q = obj => Array.isArray(obj) && !obj.__isvector__;
