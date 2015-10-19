// General functions

export const _sequential_Q = lst => _list_Q(lst) || _vector_Q(lst)

export function _obj_type(obj) {
    if      (_symbol_Q(obj))   { return 'symbol' }
    else if (_list_Q(obj))     { return 'list' }
    else if (_vector_Q(obj))   { return 'vector' }
    else if (_hash_map_Q(obj)) { return 'hash-map' }
    else if (obj === null)     { return 'nil' }
    else if (obj === true)     { return 'true' }
    else if (obj === false)    { return 'false' }
    else {
        switch (typeof(obj)) {
        case 'number':   return 'number'
        case 'function': return 'function'
        case 'string': return obj[0] == '\u029e' ? 'keyword' : 'string'
        default: throw new Error(`Unknown type '${typeof(obj)}'`)
        }
    }
}

export function _equal_Q (a, b) {
    let ota = _obj_type(a), otb = _obj_type(b)
    if (!(ota === otb || (_sequential_Q(a) && _sequential_Q(b)))) {
        return false
    }
    switch (ota) {
    case 'list':
    case 'vector':
        if (a.length !== b.length) { return false }
        for (let i=0; i<a.length; i++) {
            if (! _equal_Q(a[i], b[i])) { return false }
        }
        return true
    case 'hash-map':
        let akeys = Object.keys(a).sort(),
            bkeys = Object.keys(b).sort()
        if (akeys.length !== bkeys.length) { return false }
        for (let i=0; i<akeys.length; i++) {
            if (akeys[i] !== bkeys[i]) { return false }
            if (! _equal_Q(a.get(akeys[i]), b.get(bkeys[i]))) { return false }
        }
        return true
    default:
        return a === b
    }
}

export function _clone(obj, new_meta) {
    let new_obj = null
    if (_list_Q(obj)) {
        new_obj = obj.slice(0)
    } else if (_vector_Q(obj)) {
        new_obj = _vector(...obj.slice(0))
    } else if (_hash_map_Q(obj)) {
        new_obj = new Map(obj.entries())
    } else if (obj instanceof Function) {
        new_obj = obj.clone()
    } else {
        throw Error('Unsupported type for clone')
    }
    if (typeof new_meta !== 'undefined') { new_obj.meta = new_meta }
    return new_obj
}

// Functions
export function _malfunc(f, ast, env, params) {
    f.ast = ast
    f.env = env
    f.params = params
    f.meta = null
    f.ismacro = false
    return f
}
export const _malfunc_Q = f => f.ast ? true : false
Function.prototype.clone = function() {
    let that = this
    // New function instance
    let f = function (...args) { return that.apply(this, args) }
    // Copy properties
    for (let k of Object.keys(this)) { f[k] = this[k] }
    return f
}


// Symbols
export const _symbol = name => Symbol.for(name)
export const _symbol_Q = obj => typeof obj === 'symbol'

// Keywords
export const _keyword = obj => _keyword_Q(obj) ? obj : '\u029e' + obj
export const _keyword_Q = obj => typeof obj === 'string' && obj[0] === '\u029e'

// Lists
export const _list_Q = obj => Array.isArray(obj) && !obj.__isvector__

// Vectors
// TODO: Extend Array when supported
export function _vector(...args) {
    let v = args.slice(0)
    v.__isvector__ = true
    return v
}
export const _vector_Q = obj => Array.isArray(obj) && !!obj.__isvector__

// Hash Maps
export const _hash_map = (...args) => _assoc_BANG(new Map(), ...args)
export const _hash_map_Q = hm => hm instanceof Map
export function _assoc_BANG(hm, ...args) {
    if (args % 2 === 1) {
        throw new Error('Odd number of assoc arguments')
    }
    // Use iterator/Array.from when it works
    for (let i=0; i<args.length; i+=2) {
        if (typeof args[i] !== 'string') {
            throw new Error(`expected hash-map key string, got: ${typeof args[i]}`)
        }
        hm.set(args[i], args[i+1])
    }
    return hm
}
export function _dissoc_BANG(hm, ...args) {
    for (let i=0; i<args.length; i++) { hm.delete(args[i]) }
    return hm
}


// Atoms
export class Atom {
    constructor(val) { this.val = val }
}
