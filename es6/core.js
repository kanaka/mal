import { _equal_Q, _clone, _list_Q, _sequential_Q,
         _keyword, _keyword_Q, _vector, _vector_Q,
         _hash_map, _hash_map_Q, _assoc_BANG, _dissoc_BANG,
         Sym, Atom } from './types';
import { pr_str } from './printer';
import { readline } from './node_readline';
import { read_str } from './reader';

// Errors/Exceptions
function mal_throw(exc) { throw exc; }

// String functions
function do_pr_str(...args) {
    return args.map(exp => pr_str(exp, true)).join(" ");
}

function str(...args) {
    return args.map(exp => pr_str(exp, false)).join("");
}

function prn(...args) {
    console.log.apply({}, args.map(exp => pr_str(exp, true)));
    return null;
}

function println(...args) {
    console.log.apply({}, args.map(exp => pr_str(exp, false)));
    return null;
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

// Sequence functions
function nth(lst, idx) {
    if (idx < lst.length) { return lst[idx]; }
    else                  { throw new Error("nth: index out of range"); }
}

function conj(lst, ...args) {
    if (_list_Q(lst)) {
        return args.reverse().concat(lst);
    } else {
        return _vector(...lst.concat(args));
    }
}

function keys(hm) {
    let ks = [];
    for (let k of hm.keys()) { ks.push(k) };
    return ks;
}

function vals(hm) {
    let vs = [];
    for (let v of hm.values()) { vs.push(v) };
    return vs;
}

// Metadata functions
function meta(obj) {
    return 'meta' in obj ? obj['meta'] : null;
}

function with_meta(obj, m) {
    let new_obj = _clone(obj);
    new_obj.meta = m;
    return new_obj;
}

// core_ns is namespace of type functions
export const core_ns = new Map([
        ['=', _equal_Q],
        ['throw', mal_throw],

        ['nil?', a => a === null],
        ['true?', a => a === true],
        ['false?', a => a === false],
        ['symbol', a => new Sym(a)],
        ['symbol?', a => a instanceof Sym],
        ['keyword', a => _keyword(a)],
        ['keyword?', a => _keyword_Q(a)],

        ['pr-str', do_pr_str],
        ['str', str],
        ['prn', prn],
        ['println', println],
        ['read-string', read_str],
        ['readline', readline],
        ['slurp', slurp],

        ['<' , (a,b) => a<b],
        ['<=', (a,b) => a<=b],
        ['>' , (a,b) => a>b],
        ['>=', (a,b) => a>=b],
        ['+' , (a,b) => a+b],
        ['-' , (a,b) => a-b],
        ['*' , (a,b) => a*b],
        ['/' , (a,b) => a/b],
        ["time-ms", () => new Date().getTime()],

        ['list', (...a) => a],
        ['list?', _list_Q],
        ['vector', _vector],
        ['vector?', _vector_Q],
        ['hash-map', _hash_map],
        ['map?', _hash_map_Q],
        ['assoc', (m,...a) => _assoc_BANG(_clone(m), ...a)],
        ['dissoc', (m,...a) => _dissoc_BANG(_clone(m), ...a)],
        ['get', (m,a) => m === null ? null : m.has(a) ? m.get(a) : null],
        ['contains?', (m,a) => m.has(a)],
        ['keys', keys],
        ['vals', vals],

        ['sequential?', _sequential_Q],
        ['cons', (a,b) => [a].concat(b)],
        ['concat', (...a) => a.reduce((x,y) => x.concat(y), [])],
        ['nth', nth],
        ['first', a => a.length > 0 ? a[0] : null],
        ['rest', a => a.slice(1)],
        ['empty?', a => a.length === 0],
        ['count', a => a === null ? 0 : a.length],
        ['apply', (f,...a) => f(...a.slice(0, -1).concat(a[a.length-1]))],
        ['map', (f,a) => a.map(x => f(x))],

        ['conj', conj],

        ['meta', meta],
        ['with-meta', with_meta],
        ['atom', a => new Atom(a)],
        ['atom?', a => a instanceof Atom],
        ['deref', atm => atm.val],
        ['reset!', (atm,a) => atm.val = a],
        ['swap!', (atm,f,...args) => atm.val = f(...[atm.val].concat(args))]
        ]);
