types = require "./types.coffee"
_symbol = types._symbol


class Reader
  constructor: (@tokens) -> @position = 0
  next: -> @tokens[@position++]
  peek: -> @tokens[@position]
  skip: ->
    @position++
    @

tokenize = (str) ->
    re = /[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)/g
    results = []
    while (match = re.exec(str)[1]) != ""
      continue if match[0] == ';'
      results.push(match)
    results

read_atom = (rdr) ->
  token = rdr.next()
  if token.match /^-?[0-9]+$/ then parseInt token,10
  else if token.match /^-?[0-9][0-9.]*$/ then parseFloat token,10
  else if token[0] == '"'
    token.slice(1, token.length-1)
      .replace(/\\"/g, '"')
      .replace(/\\n/g, "\n")
      .replace(/\\\\/g, "\\")
  else if token[0] == ':' then types._keyword(token[1..])
  else if token == "nil" then null
  else if token == "true" then true
  else if token == "false" then false
  else _symbol(token)

read_list = (rdr, start='(', end=')') ->
  ast = []
  token = rdr.next()
  throw new Error "expected '" + start + "'" if token != start
  while (token = rdr.peek()) != end
    throw new Error "expected '" + end + "', got EOF" if !token
    ast.push read_form rdr
  rdr.next()
  ast

read_vector = (rdr) ->
  types._vector(read_list(rdr, '[', ']')...)

read_hash_map = (rdr) ->
  types._hash_map(read_list(rdr, '{', '}')...)

read_form = (rdr) ->
  token = rdr.peek()
  switch token
    when '\'' then [_symbol('quote'), read_form(rdr.skip())]
    when '`'  then [_symbol('quasiquote'), read_form(rdr.skip())]
    when '~'  then [_symbol('unquote'), read_form(rdr.skip())]
    when '~@' then [_symbol('splice-unquote'), read_form(rdr.skip())]
    when '^'
      meta = read_form(rdr.skip())
      [_symbol('with-meta'), read_form(rdr), meta]
    when '@' then [_symbol('deref'), read_form(rdr.skip())]

    # list
    when ')' then throw new Error "unexpected ')'"
    when '(' then read_list(rdr)
    # vector
    when ']' then throw new Error "unexpected ']'"
    when '[' then read_vector(rdr)
    # hash-map
    when '}' then throw new Error "unexpected '}'"
    when '{' then read_hash_map(rdr)
    # atom
    else read_atom(rdr)


exports.BlankException = BlankException = (msg) -> null

exports.read_str = read_str = (str) ->
  tokens = tokenize(str)
  throw new BlankException() if tokens.length == 0
  read_form(new Reader(tokens))

#console.log read_str "(1 \"two\" three)"
#console.log read_str "[1 2 3]"
#console.log read_str '{"abc" 123 "def" 456}'

# vim: ts=2:sw=2
