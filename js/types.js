// Node vs browser behavior
var types = {};
if (typeof module === 'undefined') {
    var exports = types;
}

// General utility functions

// Clone a function
Function.prototype.clone = function() {
    var that = this;
    var temp = function () { return that.apply(this, arguments); };
    for( key in this ) {
        temp[key] = this[key];
    }
    return temp;
};

function _clone (obj) {
    var new_obj;
    switch (obj_type(obj)) {
    case 'list':
        new_obj = obj.slice(0);
        break;
    case 'vector':
        new_obj = obj.slice(0);
        new_obj.__isvector__ = true;
        break;
    case 'hash-map':
        new_obj = {};
        for (var k in obj) {
            if (obj.hasOwnProperty(k)) { new_obj[k] = obj[k]; }
        }
        break;
    case 'function':
        new_obj = obj.clone();
        break;
    default:
        throw new Error("clone of non-collection: " + obj_type(obj));
    }
    return new_obj;
}




function nil_Q(a) { return a === null ? true : false; }
function true_Q(a) { return a === true ? true : false; }
function false_Q(a) { return a === false ? true : false; }

function obj_type(obj) {
    if      (symbol_Q(obj)) {   return 'symbol'; }
    else if (list_Q(obj)) {     return 'list'; }
    else if (vector_Q(obj)) {   return 'vector'; }
    else if (hash_map_Q(obj)) { return 'hash-map'; }
    else if (nil_Q(obj)) {      return 'nil'; }
    else if (true_Q(obj)) {     return 'true'; }
    else if (false_Q(obj)) {    return 'false'; }
    else if (atom_Q(obj)) {     return 'atom'; }
    else {
        switch (typeof(obj)) {
        case 'number':   return 'number';
        case 'function': return 'function';
        case 'string':   return 'string';
        default: throw new Error("Unknown type '" + typeof(obj) + "'");
        }
    }
}

function _pr_str(obj, print_readably) {
    if (typeof print_readably === 'undefined') { print_readably = true; }
    var _r = print_readably;
    var ot = obj_type(obj);
    switch (ot) {
    case 'list':
        var ret = obj.map(function(e) { return _pr_str(e,_r); });
        return "(" + ret.join(' ') + ")";
    case 'vector':
        var ret = obj.map(function(e) { return _pr_str(e,_r); });
        return "[" + ret.join(' ') + "]";
    case 'hash-map':
        var ret = [];
        for (var k in obj) {
            ret.push(_pr_str(k,_r), _pr_str(obj[k],_r));
        }
        return "{" + ret.join(' ') + "}";
    case 'string':
        if (print_readably) {
            return '"' + obj.replace(/\\/, "\\\\").replace(/"/g, '\\"') + '"';
        } else {
            return obj;
        }
    case 'nil':
        return "nil";
    case 'atom':
        return "(atom " + _pr_str(obj.val,_r) + ")";
    default:
        return obj.toString();
    }
}

function pr_str() {
    return Array.prototype.map.call(arguments,function(exp) {
        return _pr_str(exp, true);
    }).join(" ");
}

function str() {
    return Array.prototype.map.call(arguments,function(exp) {
        return _pr_str(exp, false);
    }).join("");
}

function prn() {
    console.log.apply(console, Array.prototype.map.call(arguments,function(exp) {
        return _pr_str(exp, true);
    }));
}

function println() {
    console.log.apply(console, Array.prototype.map.call(arguments,function(exp) {
        return _pr_str(exp, false);
    }));
}

function with_meta(obj, m) {
    var new_obj = _clone(obj);
    new_obj.__meta__ = m;
    return new_obj;
}

function meta(obj) {
    // TODO: support symbols and atoms
    if ((!sequential_Q(obj)) &&
        (!(hash_map_Q(obj))) &&
        (!(function_Q(obj)))) {
        throw new Error("attempt to get metadata from: " + obj_type(obj));
    }
    return obj.__meta__;
}


function equal_Q (a, b) {
    var ota = obj_type(a), otb = obj_type(b);
    if (!(ota === otb || (sequential_Q(a) && sequential_Q(b)))) {
        return false;
    }
    switch (ota) {
    case 'symbol': return a.value === b.value;
    case 'list':
    case 'vector':
        if (a.length !== b.length) { return false; }
        for (var i=0; i<a.length; i++) {
            if (! equal_Q(a[i], b[i])) { return false; }
        }
        return true;
    case 'hash-map':
        var akeys = Object.keys(a).sort(),
            bkeys = Object.keys(b).sort();
        if (akeys.length !== bkeys.length) { return false; }
        for (var i=0; i<akeys.length; i++) {
            if (akeys[i] !== bkeys[i]) { return false; }
            if (! equal_Q(a[akeys[i]], b[bkeys[i]])) { return false; }
        }
        return true;
    default:
        return a === b;
    }
}



// Symbols
function Symbol(name) {
    this.value = name;
    return this;
}
Symbol.prototype.toString = function() { return this.value; }

function symbol(name) { return new Symbol(name); }

function symbol_Q(obj) { return obj instanceof Symbol; }


// Functions
function new_function(func, exp, env, params) {
    var f = function() {
        // TODO: figure out why this throws with 'and' macro
        //throw new Error("Attempt to invoke mal function directly");
        return func(exp, new Env(env, params, arguments));
    };
    f.__meta__ = {exp: exp, env: env, params: params};
    return f;

}
function function_Q(f) { return typeof f == "function"; }



// Errors/Exceptions
function mal_throw(exc) { throw exc; }


// Vectors
function vector() {
    var v = Array.prototype.slice.call(arguments, 0);
    v.__isvector__ = true;
    return v;
}

function vector_Q(v) { return Array.isArray(v) && v.__isvector__; }


// Lists

function list() {
    return Array.prototype.slice.call(arguments, 0);
}

function list_Q(lst) { return Array.isArray(lst) && !lst.__isvector__; }


// Hash Maps

function hash_map() {
    if (arguments.length % 2 === 1) {
        throw new Error("Odd number of hash map arguments");
    }
    var args = [{}].concat(Array.prototype.slice.call(arguments, 0));
    return assoc_BANG.apply(null, args);
}

function hash_map_Q(hm) {
    return typeof hm === "object" &&
           !Array.isArray(hm) &&
           !(hm === null) && 
           !(hm instanceof Atom);
}

function assoc_BANG(hm) {
    if (arguments.length % 2 !== 1) {
        throw new Error("Odd number of assoc arguments");
    }
    for (var i=1; i<arguments.length; i+=2) {
        var ktoken = arguments[i],
            vtoken = arguments[i+1];
        // TODO: support more than string keys
        //if (list_Q(ktoken) && hash_map_Q(ktoken)) {
        //    throw new Error("expected hash-map key atom, got collection");
        //}
        if (typeof ktoken !== "string") {
            throw new Error("expected hash-map key string, got: " + (typeof ktoken));
        }
        hm[ktoken] = vtoken;
    }
    return hm;
}

function assoc(src_hm) {
    var hm = _clone(src_hm);
    var args = [hm].concat(Array.prototype.slice.call(arguments, 1));
    return assoc_BANG.apply(null, args);
}

function dissoc_BANG(hm) {
    for (var i=1; i<arguments.length; i++) {
        var ktoken = arguments[i];
        delete hm[ktoken];
    }
    return hm;
}

function dissoc(src_hm) {
    var hm = _clone(src_hm);
    var args = [hm].concat(Array.prototype.slice.call(arguments, 1));
    return dissoc_BANG.apply(null, args);
}

function get(hm, key) {
    if (key in hm) {
        return hm[key];
    } else {
        return null;
    }
}

function contains_Q(hm, key) {
    if (key in hm) { return true; } else { return false; }
}

function keys(hm) { return Object.keys(hm); }
function vals(hm) { return Object.keys(hm).map(function(k) { return hm[k]; }); }


// Atoms
function Atom(val) { this.val = val; }
function atom(val) { return new Atom(val); }
function atom_Q(atm) { return atm instanceof Atom; }
function deref(atm) { return atm.val; }
function reset_BANG(atm, val) { return atm.val = val; }
function swap_BANG(atm, f) {
    var args = [atm.val].concat(Array.prototype.slice.call(arguments, 2));
    atm.val = f.apply(f, args);
    return atm.val;
}


// Sequence operations
function sequential_Q(lst) { return list_Q(lst) || vector_Q(lst); }

function nth(lst, idx) { return lst[idx]; }

function count(s) {
    if (Array.isArray(s)) { return s.length; }
    else {                  return Object.keys(s).length; }
}

function empty_Q(lst) { return lst.length === 0; }

function cons(a, b) { return [a].concat(b); }

function concat(lst) {
    lst = lst || [];
    return lst.concat.apply(lst, Array.prototype.slice.call(arguments, 1));
}

function conj(lst) {
    return lst.concat(Array.prototype.slice.call(arguments, 1));
}

function first(lst) { return lst[0]; }

function rest(lst) { return lst.slice(1); }



// General list related functions
function apply(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    return f.apply(f, args.slice(0, args.length-1).concat(args[args.length-1]));
}

function map(f, lst) {
    return lst.map(function(el){ return f(el); });
}


// Env implementation
function Env(outer, binds, exprs) {
    this.data = {};
    this.outer = outer || null;

    if (binds && exprs) {
        // Returns a new Env with symbols in binds bound to
        // corresponding values in exprs
        // TODO: check types of binds and exprs and compare lengths
        for (var i=0; i<binds.length;i++) {
            if (binds[i].value === "&") {
                // variable length arguments
                this.data[binds[i+1].value] = Array.prototype.slice.call(exprs, i);
                break;
            } else {
                this.data[binds[i].value] = exprs[i];
            }
        }
    }
    return this;
}
Env.prototype.find = function (key) {
    if (key in this.data) { return this; }
    else if (this.outer) {  return this.outer.find(key); }
    else { return null; }
};
Env.prototype.set = function(key, value) { this.data[key] = value; return value; },
Env.prototype.get = function(key) {
    var env = this.find(key);
    if (!env) { throw new Error("'" + key + "' not found"); }
    return env.data[key];
};

// types.ns is namespace of type functions
var ns = {'pr-str': pr_str, 'str': str, 'prn': prn, 'println': println,
          'with-meta': with_meta, 'meta': meta,
          type: obj_type, '=': equal_Q,
          symbol: symbol, 'symbol?': symbol_Q,
          'nil?': nil_Q, 'true?': true_Q, 'false?': false_Q,
          '<'  : function(a,b){return a<b;},
          '<=' : function(a,b){return a<=b;},
          '>'  : function(a,b){return a>b;},
          '>=' : function(a,b){return a>=b;},
          '+'  : function(a,b){return a+b;},
          '-'  : function(a,b){return a-b;},
          '*'  : function(a,b){return a*b;},
          '/'  : function(a,b){return a/b;},
          'throw': mal_throw,
          'list': list, 'list?': list_Q,
          'vector': vector, 'vector?': vector_Q,
          'hash-map': hash_map, 'map?': hash_map_Q,
          'assoc': assoc, 'dissoc': dissoc, 'get': get,
          'contains?': contains_Q, 'keys': keys, 'vals': vals,
          'atom': atom, 'atom?': atom_Q,
          "deref": deref, "reset!": reset_BANG, "swap!": swap_BANG,
          'sequential?': sequential_Q, 'cons': cons, 'nth': nth,
          'empty?': empty_Q, 'count': count, 'concat': concat,
          'conj': conj, 'first': first, 'rest': rest,
          'apply': apply, 'map': map};

exports.ns = types.ns = ns;
exports._pr_str = types._pr_str = _pr_str;
exports.prn = types.prn = prn;
exports.Env = types.Env = Env;

exports.symbol = types.symbol = symbol;
exports.symbol_Q = types.symbol_Q = symbol_Q;
exports.hash_map = types.hash_map = hash_map;
exports.hash_map_Q = types.hash_map_Q = hash_map_Q;
exports.new_function = types.new_function = new_function;
exports.list = types.list = list;
exports.list_Q = types.list_Q = list_Q;
exports.vector = types.vector = vector;
exports.vector_Q = types.vector_Q = vector_Q;

exports.sequential_Q = types.sequential_Q = sequential_Q;
exports.cons = types.cons = cons;
exports.concat = types.concat = concat;
exports.first = types.first = first;
exports.rest = types.rest = rest;
exports.apply = types.apply = apply;
exports.map = types.map = map;
