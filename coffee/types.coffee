Env = require("./env.coffee").Env

E = exports

# General functions
E._obj_type = _obj_type = (obj) ->
  if _symbol_Q(obj)        then 'symbol'
  else if _list_Q(obj)     then 'list'
  else if _vector_Q(obj)   then 'vector'
  else if _hash_map_Q(obj) then 'hash-map'
  else if _nil_Q(obj)      then 'nil'
  else if _true_Q(obj)     then 'true'
  else if _false_Q(obj)    then 'false'
  else if _atom_Q(obj)     then 'atom'
  else
    switch typeof obj
      when 'number'   then 'number'
      when 'function' then 'function'
      when 'string'
        if obj[0] == '\u029e' then 'keyword' else 'string'
      else throw new Error "Unknown type '" + typeof(obj) + "'"

E._sequential_Q = _sequential_Q = (o) -> _list_Q(o) or _vector_Q(o)

E._equal_Q = _equal_Q = (a,b) ->
  [ota, otb] = [_obj_type(a), _obj_type(b)]
  if !(ota == otb or (_sequential_Q(a) && _sequential_Q(b)))
    return false
  switch (ota)
    when 'symbol' then a.name == b.name
    when 'list', 'vector'
      return false if a.length != b.length
      for av,i in a
        return false if !_equal_Q(av, b[i])
      true
    when 'hash-map'
      akeys = (key for key of a)
      bkeys = (key for key of b)
      return false if akeys.length != bkeys.length
      for akey,i in akeys
        bkey = bkeys[i]
        return false if akey != bkey
        return false if !_equal_Q(a[akey], b[bkey])
      true
    else a == b

E._clone = _clone = (obj) ->
  switch _obj_type(obj)
    when 'list' then obj[0..-1]
    when 'vector' then _vector(obj[0..-1]...)
    when 'hash-map'
      new_obj = {}
      new_obj[k] = v for k,v of obj
      new_obj
    when 'function'
      new_obj = (args...) -> obj(args...)
      new_obj[k] = v for k,v of obj
      new_obj
    else throw new Error "clone called on non-collection" + _obj_type(obj)


# Scalars
E._nil_Q = _nil_Q = (o) -> o == null
E._true_Q = _true_Q = (o) -> o == true
E._false_Q = _false_Q = (o) -> o == false

# Symbols
class Symbol
  constructor: (@name) ->
E._symbol = (str) -> new Symbol str
E._symbol_Q = _symbol_Q = (o) -> o instanceof Symbol

# Keywords
E._keyword = _keyword = (o) ->
  _keyword_Q(o) && o || ("\u029e" + o)
E._keyword_Q = _keyword_Q = (o) ->
  typeof o == 'string' && o[0] == "\u029e"

# Functions
E._function = (evalfn, ast, env, params) ->
  fn = (args...) -> evalfn(ast, new Env(env, params, args))
  fn.__ast__ = ast
  fn.__gen_env__ = (args) -> new Env(env, params, args)
  fn.__ismacro__ = false
  fn
E._function_Q = _function_Q = (o) -> !!o.__ast__

# Lists
E._list_Q = _list_Q = (o) -> Array.isArray(o) && !o.__isvector__

# Vectors
E._vector = _vector = (args...) ->
  v = args
  v.__isvector__ = true
  v
E._vector_Q = _vector_Q = (o) -> Array.isArray(o) && !!o.__isvector__

# Hash Maps
E._hash_map = (args...) ->
  args = [{}].concat args
  _assoc_BANG(args...)
E._assoc_BANG = _assoc_BANG = (hm, args...) ->
  if args.length %% 2 == 1
    throw new Error "Odd number of hash map arguments"
  hm[k] = args[i+1] for k, i in args when i %% 2 == 0
  hm
E._dissoc_BANG = (hm, args...) ->
  delete hm[k] for k, i in args
  hm
E._hash_map_Q = _hash_map_Q = (o) ->
  typeof o == "object" && !Array.isArray(o) &&
                          !(o == null) &&
                          !(o instanceof Symbol) &&
                          !(o instanceof Atom)


# Atoms
class Atom
  constructor: (@val) ->
E._atom = (val) -> new Atom val
E._atom_Q = _atom_Q = (o) -> o instanceof Atom

# vim: ts=2:sw=2
