import { _equal_Q, _clone, _keyword, _keyword_Q } from './types'
import { _list_Q, Vector, _assoc_BANG, Atom } from './types'
import { pr_str } from './printer'
import rl from './node_readline'
const readline = rl.readline
import { read_str } from './reader'
import { readFileSync } from 'fs'

function _error(e) { throw new Error(e) }

// String functions
function slurp(f) {
    if (typeof process !== 'undefined') {
        return readFileSync(f, 'utf-8')
    } else {
        var req = new XMLHttpRequest()
        req.open('GET', f, false)
        req.send()
        if (req.status !== 200) {
            _error(`Failed to slurp file: ${f}`)
        }
        return req.responseText
    }
}

// Sequence functions
function seq(obj) {
    if (_list_Q(obj)) {
        return obj.length > 0 ? obj : null
    } else if (obj instanceof Vector) {
        return obj.length > 0 ? Array.from(obj.slice(0)) : null
    } else if (typeof obj === "string" && !_keyword_Q(obj)) {
        return obj.length > 0 ? obj.split('') : null
    } else if (obj === null) {
        return null
    } else {
        _error('seq: called on non-sequence')
    }
}

// core_ns is namespace of type functions
export const core_ns = new Map([
    ['=', _equal_Q],
    ['throw', a => { throw a }],

    ['nil?', a => a === null],
    ['true?', a => a === true],
    ['false?', a => a === false],
    ['number?', a => typeof a === 'number'],
    ['string?', a => typeof a === "string" && !_keyword_Q(a)],
    ['symbol', a => Symbol.for(a)],
    ['symbol?', a => typeof a === 'symbol'],
    ['keyword', _keyword],
    ['keyword?', _keyword_Q],
    ['fn?', a => typeof a === 'function' && !a.ismacro ],
    ['macro?', a => typeof a === 'function' && !!a.ismacro ],

    ['pr-str', (...a) => a.map(e => pr_str(e,1)).join(' ')],
    ['str', (...a) => a.map(e => pr_str(e,0)).join('')],
    ['prn', (...a) => console.log(...a.map(e => pr_str(e,1))) || null],
    ['println', (...a) => console.log(...a.map(e => pr_str(e,0))) || null],
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
    ['vector', (...a) => Vector.from(a)],
    ['vector?', a => a instanceof Vector],
    ['hash-map', (...a) => _assoc_BANG(new Map(), ...a)],
    ['map?', a => a instanceof Map],
    ['assoc', (m,...a) => _assoc_BANG(_clone(m), ...a)],
    ['dissoc', (m,...a) => { let n = _clone(m); a.forEach(k => n.delete(k));
                             return n}],
    ['get', (m,a) => m === null ? null : m.has(a) ? m.get(a) : null],
    ['contains?', (m,a) => m.has(a)],
    ['keys', a => Array.from(a.keys())],
    ['vals', a => Array.from(a.values())],

    ['sequential?', a => Array.isArray(a)],
    ['cons', (a,b) => [a].concat(b)],
    ['concat', (...a) => a.reduce((x,y) => x.concat(y), [])],
    ['vec', (a) => Vector.from(a)],
    ['nth', (a,b) => b < a.length ? a[b] : _error('nth: index out of range')],
    ['first', a => a !== null && a.length > 0 ? a[0] : null],
    ['rest', a => a === null ? [] : Array.from(a.slice(1))],
    ['empty?', a => a.length === 0],
    ['count', a => a === null ? 0 : a.length],
    ['apply', (f,...a) => f(...a.slice(0, -1).concat(a[a.length-1]))],
    ['map', (f,a) => Array.from(a.map(x => f(x)))],

    ['conj', (s,...a) => _list_Q(s) ? a.reverse().concat(s)
                                    : Vector.from(s.concat(a))],
    ['seq', seq],

    ['meta', a => 'meta' in a ? a['meta'] : null],
    ['with-meta', (a,b) => { let c = _clone(a); c.meta = b; return c }],
    ['atom', a => new Atom(a)],
    ['atom?', a => a instanceof Atom],
    ['deref', atm => atm.val],
    ['reset!', (atm,a) => atm.val = a],
    ['swap!', (atm,f,...args) => atm.val = f(...[atm.val].concat(args))]
    ])
