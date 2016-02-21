// Node vs browser behavior
var core = {};
if (typeof module === 'undefined') {
    var exports = core;
} else {
    var types = require('./types'),
        readline = require('./node_readline'),
        reader = require('./reader'),
        printer = require('./printer');
}

// Errors/Exceptions
function mal_throw(exc) { throw exc; }


// String functions
function pr_str() {
    return Array.prototype.map.call(arguments,function(exp) {
        return printer._pr_str(exp, true);
    }).join(" ");
}

function str() {
    return Array.prototype.map.call(arguments,function(exp) {
        return printer._pr_str(exp, false);
    }).join("");
}

function prn() {
    printer.println.apply({}, Array.prototype.map.call(arguments,function(exp) {
        return printer._pr_str(exp, true);
    }));
}

function println() {
    printer.println.apply({}, Array.prototype.map.call(arguments,function(exp) {
        return printer._pr_str(exp, false);
    }));
}

function slurp(f) {
    if (typeof require !== 'undefined') {
        return require('fs').readFileSync(f, 'utf-8');
    } else {
        var req = new XMLHttpRequest();
        req.open("GET", f, false);
        req.send();
        if (req.status == 200) {
            return req.responseText;
        } else {
            throw new Error("Failed to slurp file: " + f);
        }
    }
}


// Number functions
function time_ms() { return new Date().getTime(); }


// Hash Map functions
function assoc(src_hm) {
    var hm = types._clone(src_hm);
    var args = [hm].concat(Array.prototype.slice.call(arguments, 1));
    return types._assoc_BANG.apply(null, args);
}

function dissoc(src_hm) {
    var hm = types._clone(src_hm);
    var args = [hm].concat(Array.prototype.slice.call(arguments, 1));
    return types._dissoc_BANG.apply(null, args);
}

function get(hm, key) {
    if (hm != null && key in hm) {
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


// Sequence functions
function cons(a, b) { return [a].concat(b); }

function concat(lst) {
    lst = lst || [];
    return lst.concat.apply(lst, Array.prototype.slice.call(arguments, 1));
}

function nth(lst, idx) {
    if (idx < lst.length) { return lst[idx]; }
    else                  { throw new Error("nth: index out of range"); }
}

function first(lst) { return (lst === null) ? null : lst[0]; }

function rest(lst) { return (lst == null) ? [] : lst.slice(1); }

function empty_Q(lst) { return lst.length === 0; }

function count(s) {
    if (Array.isArray(s)) { return s.length; }
    else if (s === null)  { return 0; }
    else                  { return Object.keys(s).length; }
}

function conj(lst) {
    if (types._list_Q(lst)) {
        return Array.prototype.slice.call(arguments, 1).reverse().concat(lst);
    } else {
        var v = lst.concat(Array.prototype.slice.call(arguments, 1));
        v.__isvector__ = true;
        return v;
    }
}

function apply(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    return f.apply(f, args.slice(0, args.length-1).concat(args[args.length-1]));
}

function map(f, lst) {
    return lst.map(function(el){ return f(el); });
}


// Metadata functions
function with_meta(obj, m) {
    var new_obj = types._clone(obj);
    new_obj.__meta__ = m;
    return new_obj;
}

function meta(obj) {
    // TODO: support symbols and atoms
    if ((!types._sequential_Q(obj)) &&
        (!(types._hash_map_Q(obj))) &&
        (!(types._function_Q(obj)))) {
        throw new Error("attempt to get metadata from: " + types._obj_type(obj));
    }
    return obj.__meta__;
}


// Atom functions
function deref(atm) { return atm.val; }
function reset_BANG(atm, val) { return atm.val = val; }
function swap_BANG(atm, f) {
    var args = [atm.val].concat(Array.prototype.slice.call(arguments, 2));
    atm.val = f.apply(f, args);
    return atm.val;
}


// types.ns is namespace of type functions
var ns = {'type': types._obj_type,
          '=': types._equal_Q,
          'throw': mal_throw,
          'nil?': types._nil_Q,
          'true?': types._true_Q,
          'false?': types._false_Q,
          'symbol': types._symbol,
          'symbol?': types._symbol_Q,
          'keyword': types._keyword,
          'keyword?': types._keyword_Q,

          'pr-str': pr_str,
          'str': str,
          'prn': prn,
          'println': println,
          'readline': readline.readline,
          'read-string': reader.read_str,
          'slurp': slurp,
          '<'  : function(a,b){return a<b;},
          '<=' : function(a,b){return a<=b;},
          '>'  : function(a,b){return a>b;},
          '>=' : function(a,b){return a>=b;},
          '+'  : function(a,b){return a+b;},
          '-'  : function(a,b){return a-b;},
          '*'  : function(a,b){return a*b;},
          '/'  : function(a,b){return a/b;},
          "time-ms": time_ms,

          'list': types._list,
          'list?': types._list_Q,
          'vector': types._vector,
          'vector?': types._vector_Q,
          'hash-map': types._hash_map,
          'map?': types._hash_map_Q,
          'assoc': assoc,
          'dissoc': dissoc,
          'get': get,
          'contains?': contains_Q,
          'keys': keys,
          'vals': vals,

          'sequential?': types._sequential_Q,
          'cons': cons,
          'concat': concat,
          'nth': nth,
          'first': first,
          'rest': rest,
          'empty?': empty_Q,
          'count': count,
          'apply': apply,
          'map': map,
          'conj': conj,

          'with-meta': with_meta,
          'meta': meta,
          'atom': types._atom,
          'atom?': types._atom_Q,
          "deref": deref,
          "reset!": reset_BANG,
          "swap!": swap_BANG};

exports.ns = core.ns = ns;
