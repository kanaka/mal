// General functions
export function _equal_Q (a, b) {
    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) { return false }
        for (let i=0; i<a.length; i++) {
            if (! _equal_Q(a[i], b[i])) { return false }
        }
        return true
    } else if (a instanceof Map && b instanceof Map) {
        if (a.size !== b.size) { return false }
        for (let k of a.keys()) {
            if (! _equal_Q(a.get(k), b.get(k))) { return false }
        }
        return true
    } else {
        return a === b
    }
}

export function _clone(obj, new_meta) {
    let new_obj = null
    if (_list_Q(obj)) {
        new_obj = obj.slice(0)
    } else if (obj instanceof Vector) {
        new_obj = Vector.from(obj)
    } else if (obj instanceof Map) {
        new_obj = new Map(obj.entries())
    } else if (obj instanceof Function) {
        let f = (...a) => obj.apply(f, a)  // new function instance
        new_obj = Object.assign(f, obj)    // copy original properties
    } else {
        throw Error('Unsupported type for clone')
    }
    if (typeof new_meta !== 'undefined') { new_obj.meta = new_meta }
    return new_obj
}

// Functions
export function _malfunc(f, ast, env, params, meta=null, ismacro=false) {
    return Object.assign(f, {ast, env, params, meta, ismacro})
}
export const _malfunc_Q = f => f.ast ? true : false


// Keywords
export const _keyword = obj => _keyword_Q(obj) ? obj : '\u029e' + obj
export const _keyword_Q = obj => typeof obj === 'string' && obj[0] === '\u029e'

// Lists
export const _list_Q = obj => Array.isArray(obj) && !(obj instanceof Vector)

// Vectors
export class Vector extends Array { }

// Maps
export function _assoc_BANG(hm, ...args) {
    if (args.length % 2 === 1) {
        throw new Error('Odd number of assoc arguments')
    }
    for (let i=0; i<args.length; i+=2) { hm.set(args[i], args[i+1]) }
    return hm
}


// Atoms
export class Atom {
    constructor(val) { this.val = val }
}
