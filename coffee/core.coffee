readline = require "./node_readline"
types = require "./types.coffee"
reader = require "./reader.coffee"
printer = require "./printer.coffee"
[_pr_str, println] = [printer._pr_str, printer.println]

# Sequence functions
conj = (seq, args...) ->
  switch types._obj_type(seq)
    when 'list'
      lst = types._clone(seq)
      lst.unshift(x) for x in args
      lst
    when 'vector'
      lst = types._clone(seq)
      lst.push(args...)
      types._vector(lst...)
    else throw new Error "conj called on " + types._obj_type(seq)

# Metadata functions
with_meta = (obj,m) ->
  new_obj = types._clone(obj)
  new_obj.__meta__ = m
  new_obj


exports.ns = {
  '=': (a,b) -> types._equal_Q(a,b),
  'throw': (a) -> throw a,
  'nil?': types._nil_Q,
  'true?': types._true_Q,
  'false?': types._false_Q,
  'symbol': types._symbol,
  'symbol?': types._symbol_Q,
  'keyword': types._keyword,
  'keyword?': types._keyword_Q,

  'pr-str': (a...) -> a.map((exp) -> _pr_str(exp,true)).join(" "),
  'str': (a...) -> a.map((exp) -> _pr_str(exp,false)).join(""),
  'prn': (a...) -> println(a.map((exp) -> _pr_str(exp,true))...),
  'println': (a...) -> println(a.map((exp) -> _pr_str(exp,false))...),
  'readline': readline.readline,
  'read-string': reader.read_str,
  'slurp': (a) -> require('fs').readFileSync(a, 'utf-8'),
  '<':  (a,b) -> a<b,
  '<=': (a,b) -> a<=b,
  '>':  (a,b) -> a>b,
  '>=': (a,b) -> a>=b,
  '+': (a,b) -> a+b,
  '-': (a,b) -> a-b,
  '*': (a,b) -> a*b,
  '/': (a,b) -> a/b,
  'time-ms': () -> new Date().getTime(),

  'list': (a...) -> a,
  'list?': types._list_Q,
  'vector': (a...) -> types._vector(a...),
  'vector?': types._vector_Q,
  'hash-map': (a...) -> types._hash_map(a...),
  'map?': types._hash_map_Q,
  'assoc': (a,b...) -> types._assoc_BANG(types._clone(a), b...),
  'dissoc': (a,b...) -> types._dissoc_BANG(types._clone(a), b...),
  'get': (a,b) -> if a != null and b of a then a[b] else null,
  'contains?': (a,b) -> b of a,
  'keys': (a) -> k for k of a,
  'vals': (a) -> v for k,v of a,

  'sequential?': types._sequential_Q,
  'cons': (a,b) -> [a].concat(b),
  'concat': (a=[],b...) -> a.concat(b...),
  'nth': (a,b) -> if a.length > b then a[b] else
    throw new Error "nth: index out of bounds",
  'first': (a) -> if a.length > 0 then a[0] else null,
  'rest': (a) -> a[1..],
  'empty?': (a) -> a.length == 0,
  'count': (a) -> if a == null then 0 else a.length,
  'apply': (a,b...) -> a(b[0..-2].concat(b[b.length-1])...),
  'map': (a,b) -> b.map((x) -> a(x)),
  'conj': conj,

  'with-meta': with_meta,
  'meta': (a) -> a.__meta__ or null,
  'atom': types._atom,
  'atom?': types._atom_Q,
  'deref': (a) -> a.val,
  'reset!': (a,b) -> a.val = b,
  'swap!': (a,b,c...) -> a.val = b([a.val].concat(c)...), }

# vim: ts=2:sw=2
